package models

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import com.github.nscala_time.time.Imports.DateTime
import models.ModelHelper._
import play.api.Play.current
import play.api._
import play.api.libs.concurrent.Akka
import play.api.libs.json.{JsError, Json, Reads}
import play.api.libs.ws._
import scalikejdbc._

import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global

object OpenDataReceiver {
  val props = Props[OpenDataReceiver]
  var receiver: ActorRef = _

  def startup() = {
    receiver = Akka.system.actorOf(props, name = "openDataReceiver")
    Logger.info(s"OpenData receiver starts")
  }

  def reloadEpaData(start:DateTime, end:DateTime): Unit = {
    receiver ! ReloadEpaData(start, end)
  }

  case object GetEpaHourData
  case class ReloadEpaData(start:DateTime, end:DateTime)

  val mtNameToMonitorType: Map[String, _root_.models.MonitorType.Value] = Map(
    "PM10" -> MonitorType.withName("PM10"),
    "PM25" -> MonitorType.withName("PM25"),
    "SO2" -> MonitorType.withName("SO2"),
    "NOX" -> MonitorType.withName("NOx"),
    "CO" -> MonitorType.withName("CO"),
    "O3" -> MonitorType.withName("O3"),
    "THC" -> MonitorType.withName("THC"),
    "NO" -> MonitorType.withName("NO"),
    "CH4" -> MonitorType.withName("CH4"),
    "NO2" -> MonitorType.withName("NO2"),
    "NMHC" -> MonitorType.withName("NMHC"),
    "WD_SPEED" -> MonitorType.withName("WD_SPEED"),
    "WD_DIR" -> MonitorType.withName("WD_DIR"),
    "RAIN" -> MonitorType.withName("RAIN"),
    "TEMP" -> MonitorType.withName("TEMP"),
    "HUMID" -> MonitorType.withName("HUMID")
  )
}

import java.util.Date

case class HourData(
                     SiteId: String,
                     SiteName: String,
                     ItemId: String,
                     ItemName: String,
                     ItemEngName: String,
                     ItemUnit: String,
                     MonitorDate: Date,
                     MonitorValues: Seq[Double])

case class OpenDataConfig(enable: Boolean, upstream: String)
class OpenDataReceiver extends Actor with ActorLogging {

  import OpenDataReceiver._
  import com.github.nscala_time.time.Imports._

  val timer = {
    import scala.concurrent.duration._
    context.system.scheduler.schedule(Duration(5, SECONDS), Duration(3, HOURS), receiver, GetEpaHourData)
  }

  import scala.xml._

  private val openDataConfig = {
    val config = current.configuration
    val enable = config.getBoolean("openData.enable").getOrElse(false)
    val upstream = config.getString("openData.upstream").getOrElse("")
    OpenDataConfig(enable, upstream)
  }
  Logger.info(s"OpenDataReceiver start: openDataConfig: $openDataConfig")

  def receive: Receive = {
    case GetEpaHourData =>
      if (openDataConfig.enable) {
        val start = SystemConfig.getEpaLast
        val end = DateTime.now().withMillisOfDay(0)
        if (start < end) {
          Logger.info(s"Get EpaData ${start.toString("yyyy-MM-d")} => ${end.toString("yyyy-MM-d")}")
          fetchOpenEpaData(start, end)
        }
      }

    case ReloadEpaData(start, end) =>
      if (openDataConfig.enable) {
        Logger.info(s"reload EpaData ${start.toString("yyyy-MM-d")} => ${end.toString("yyyy-MM-d")}")
        fetchOpenEpaData(start, end)
      }
  }

  case class MtRecord(mtName: String, value: Option[Double], var status: String)

  case class RecordListID(time: Date, monitor: String)

  case class RecordList(var mtDataList: Seq[MtRecord], _id: RecordListID) {
    def mtMap: Map[String, MtRecord] = {
      val pairs =
        mtDataList map { data => data.mtName -> data }
      pairs.toMap
    }
  }

  implicit val r1: Reads[RecordListID] = Json.reads[RecordListID]
  implicit val r2: Reads[MtRecord] = Json.reads[MtRecord]
  implicit val r3: Reads[RecordList] = Json.reads[RecordList]

  private def fetchOpenEpaData(start: DateTime, end: DateTime): Unit = {
    val epaMonitorsIDs = EpaMonitor.idMap.keys.map(id => s"Epa$id").mkString(":")
    val startNum = start.getMillis
    val endNum = end.getMillis
    val f = WS.url(s"${openDataConfig.upstream}/HourRecord/$epaMonitorsIDs/$startNum/$endNum").get()
    f onFailure errorHandler()
    for (response <- f) yield {
      val ret = response.json.validate[Seq[RecordList]]
      ret.fold(
        err => {
          Logger.error(JsError.toJson(err).toString())
          false
        },
        recordLists => {
          Logger.info(s"Total ${recordLists.size} records fetched.")
          upsertRecordLists(recordLists)
          true
        }
      )
    }

    def upsertRecordLists(recordLists: Seq[RecordList]): Int = {
      import scala.collection.mutable.Map
      val recordMap = mutable.Map.empty[EpaMonitor.Value, mutable.Map[DateTime, mutable.Map[MonitorType.Value, Double]]]

      recordLists.foreach(recordList => {
        val epaMonitor = EpaMonitor.idMap(recordList._id.monitor.drop(3).toInt)
        val time = new DateTime(recordList._id.time)
        val timeMap = recordMap.getOrElseUpdate(epaMonitor, mutable.Map.empty[DateTime, mutable.Map[MonitorType.Value, Double]])
        recordList.mtDataList.foreach(mtData => {
          val mtMap = timeMap.getOrElseUpdate(time, mutable.Map.empty[MonitorType.Value, Double])
          for (mt <- mtNameToMonitorType.get(mtData.mtName); value <- mtData.value)
            mtMap.put(mt, value)
        })
      })

      val updateCounts =
        for {
          monitorMap <- recordMap
          monitor = monitorMap._1
          timeMaps = monitorMap._2
          dateTime <- timeMaps.keys.toList.sorted
          mtValue <- timeMaps(dateTime)
        } yield {
          val MStation = EpaMonitor.map(monitor)
          val MItem = MonitorType.map(mtValue._1).epa_mapping.get
          val MDate: java.sql.Timestamp = dateTime
          try {
            DB autoCommit { implicit session =>
              sql"""
                UPDATE dbo.hour_data
                SET MValue = ${mtValue._2}
                WHERE MStation=${MStation.id} and MDate=${MDate} and MItem=${MItem};

                IF(@@ROWCOUNT = 0)
                BEGIN
                  INSERT INTO dbo.hour_data (MStation, MDate, MItem, MValue)
                  VALUES(${MStation.id}, ${MDate}, ${MItem}, ${mtValue._2})
                END
              """.update.apply
            }
          } catch {
            case ex: Exception =>
              Logger.error(s"upsertRecordLists error: ${ex.getMessage}", ex)
              0
          }
        }

      if (updateCounts.sum != 0)
        Logger.info(s"EPA ${updateCounts.sum} records have been upserted.")

      recordLists.size
    }
  }

  def getEpaHourData(start: DateTime, end: DateTime) {
    val limit = 500

    def parser(node: Elem) = {
      import scala.collection.mutable.Map
      import scala.xml.Node
      val recordMap = Map.empty[EpaMonitor.Value, Map[DateTime, Map[MonitorType.Value, Double]]]

      def filter(dataNode: Node) = {
        val monitorDateOpt = dataNode \ "MonitorDate".toUpperCase
        val mDate =
          try{
            DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))
          }catch{
            case _:Exception=>
              DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd"))
          }

        start <= mDate && mDate < end
      }

      def processData(dataNode: Node) {
        val siteName = dataNode \ "SiteName".toUpperCase
        val itemId = dataNode \ "ItemId".toUpperCase
        val monitorDateOpt = dataNode \ "MonitorDate".toUpperCase
        val siteID = (dataNode \ "SiteId".toUpperCase).text.trim.toInt

        try {
          //Filter interested EPA monitor
          if (EpaMonitor.idMap.contains(siteID) &&
            MonitorType.eapIdMap.contains(itemId.text.trim().toInt)) {
            val epaMonitor = EpaMonitor.withName(siteName.text.trim())
            val monitorType = MonitorType.eapIdMap(itemId.text.trim().toInt)
            val mDate = try {
              DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd HH:mm:ss"))
            }catch {
              case _:Exception=>
                DateTime.parse(s"${monitorDateOpt.text.trim()}", DateTimeFormat.forPattern("YYYY-MM-dd"))
            }

            val monitorNodeValueSeq =
              for (v <- 0 to 23) yield {
                val monitorValue = try {
                  Some((dataNode \ "MonitorValue%02d".format(v).toUpperCase).text.trim().toDouble)
                } catch {
                  case x: Throwable =>
                    None
                }
                (mDate + v.hour, monitorValue)
              }

            val timeMap = recordMap.getOrElseUpdate(epaMonitor, Map.empty[DateTime, Map[MonitorType.Value, Double]])
            for {(mDate, mtValueOpt) <- monitorNodeValueSeq} {
              val mtMap = timeMap.getOrElseUpdate(mDate, Map.empty[MonitorType.Value, Double])
              for (mtValue <- mtValueOpt)
                mtMap.put(monitorType, mtValue)
            }
          }
        } catch {
          case x: Throwable =>
            Logger.error("failed", x)
        }
      }

      val data = node \ "data"

      val qualifiedData = data.filter(filter)

      qualifiedData.map {
        processData
      }

      val updateCounts =
        for {
          monitorMap <- recordMap
          monitor = monitorMap._1
          timeMaps = monitorMap._2
          dateTime <- timeMaps.keys.toList.sorted
          mtValue <- timeMaps(dateTime)
        } yield {
          val MStation = EpaMonitor.map(monitor)
          val MItem = MonitorType.map(mtValue._1).epa_mapping.get
          val MDate: java.sql.Timestamp = dateTime
          DB autoCommit { implicit session =>
            sql"""
              UPDATE dbo.hour_data
              SET MValue = ${mtValue._2}
              WHERE MStation=${MStation.id} and MDate=${MDate} and MItem=${MItem};

              IF(@@ROWCOUNT = 0)
              BEGIN
                INSERT INTO dbo.hour_data (MStation, MDate, MItem, MValue)
                VALUES(${MStation.id}, ${MDate}, ${MItem}, ${mtValue._2})
              END
            """.update.apply
          }
        }

      if(updateCounts.sum != 0)
        Logger.debug(s"EPA ${updateCounts.sum} records have been upserted.")

      qualifiedData.size
    }

    def getThisMonth(skip: Int) {
      val url = s"https://data.epa.gov.tw/api/v2/aqx_p_15?format=xml&offset=${skip}&limit=${limit}&api_key=1f4ca8f8-8af9-473d-852b-b8f2d575f26a&&sort=MonitorDate%20desc"
      val future =
        WS.url(url).get().map {
          response =>
            try {
              parser(response.xml)
            } catch {
              case ex: Exception =>
                Logger.error(ex.toString())
                throw ex
            }
        }
      future onFailure (errorHandler())
      future onSuccess ({
        case ret: Int =>
          if (ret < limit) {
            Logger.info(s"Import EPA ${start.getYear()}/${start.getMonthOfYear()} complete")
          } else
            getThisMonth(skip + limit)
      })
    }

    def getMonthData(year:Int, month:Int, skip: Int) {
      val url = f"https://data.epa.gov.tw/api/v2/aqx_p_15?format=xml&offset=$skip%d&limit=$limit&year_month=$year%d_$month%02d&api_key=1f4ca8f8-8af9-473d-852b-b8f2d575f26a"
      val f = WS.url(url).get()
      f onFailure (errorHandler())
      for(resp <- f) {
        try {
          val updateCount = parser(resp.xml)
          if (updateCount < limit) {
            Logger.info(f"Import EPA $year/$month%02d complete")
            val dataLast = new DateTime(year, month, 1, 0, 0).plusMonths(1)
            SystemConfig.setEpaLast(dataLast)
            if(dataLast < end)
              self ! ReloadEpaData(dataLast, end)
          } else
            getMonthData(year, month, skip + limit)
        } catch {
          case ex: Exception =>
            Logger.error(ex.toString)
            throw ex
        }
      }
    }

    if(start.toString("yyyy-M") == DateTime.now().toString("yyyy-M"))
      getThisMonth(0)
    else{
      getMonthData(start.getYear, start.getMonthOfYear, 0)
    }
  }


  override def postStop = {
    timer.cancel()
  }

}

