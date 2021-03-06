package controllers

import com.github.nscala_time.time.Imports._
import controllers.PdfUtility._
import models.ModelHelper._
import models.Realtime._
import models.{AQI, _}
import play.api.Logger
import play.api.Play.current
import play.api.i18n._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.mvc._

import java.sql.Timestamp
import javax.inject._

case class XAxis(categories: Option[Seq[String]], gridLineWidth: Option[Int] = None, tickInterval: Option[Int] = None)

case class AxisLineLabel(align: String, text: String)

case class AxisLine(color: String, width: Int, value: Float, label: Option[AxisLineLabel])

case class AxisTitle(text: Option[Option[String]])

case class YAxis(labels: Option[String], title: AxisTitle, plotLines: Option[Seq[AxisLine]], opposite: Boolean = false,
                 floor: Option[Int] = None, ceiling: Option[Int] = None, min: Option[Int] = None, max: Option[Int] = None, tickInterval: Option[Int] = None,
                 gridLineWidth: Option[Int] = None, gridLineColor: Option[String] = None)

case class seqData(name: String, data: Seq[Seq[Option[Double]]], yAxis: Int = 0, chartType: Option[String] = None,
                   status: Option[Seq[Option[String]]] = None)

case class HighchartData(chart: Map[String, String],
                         title: Map[String, String],
                         xAxis: XAxis,
                         yAxis: Seq[YAxis],
                         series: Seq[seqData],
                         downloadFileName: Option[String] = None)

case class FrequencyTab(header: Seq[String], body: Seq[Seq[String]], footer: Seq[String])

case class WindRoseReport(chart: HighchartData, table: FrequencyTab)

object Realtime {
  implicit val xaWrite = Json.writes[XAxis]
  implicit val axisLineLabelWrite = Json.writes[AxisLineLabel]
  implicit val axisLineWrite = Json.writes[AxisLine]
  implicit val axisTitleWrite = Json.writes[AxisTitle]
  implicit val yaWrite = Json.writes[YAxis]
  type lof = (Long, Option[Float])

  implicit val seqDataWrite: Writes[seqData] = (
    (__ \ "name").write[String] and
      (__ \ "data").write[Seq[Seq[Option[Double]]]] and
      (__ \ "yAxis").write[Int] and
      (__ \ "type").write[Option[String]] and
      (__ \ "status").write[Option[Seq[Option[String]]]]) (unlift(seqData.unapply))
  implicit val hcWrite = Json.writes[HighchartData]
  implicit val feqWrite = Json.writes[FrequencyTab]
  implicit val wrWrite = Json.writes[WindRoseReport]

}

class Realtime @Inject()(val messagesApi: MessagesApi) extends Controller with I18nSupport {

  import Realtime._

  def realtimeStat(outputTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      val outputType = OutputType.withName(outputTypeStr)

      val current = getLatestRecordTime(TableType.Min).getOrElse(DateTime.now.withSecondOfMinute(0): Timestamp)
      val sub_current = current.toDateTime - 60.second
      val rt_status = getRealtimeMinStatus(sub_current, group.privilege)
      val currentHr = getLatestRecordTime(TableType.Hour).getOrElse(DateTime.now.withMinuteOfHour(0): Timestamp)
      val rt_psi = getRealtimePSI(currentHr)
      val pm25Map = getRealtimePm25(currentHr)
      val aqiMap = AQI.getRealtimeAQI(currentHr)
      val output = views.html.realtimeStatus(sub_current, rt_status, MonitorType.psiList, rt_psi, group.privilege, pm25Map, aqiMap)
      val title = "即時資訊"
      outputType match {
        case OutputType.html =>
          Ok(output)
        case OutputType.pdf =>
          Ok.sendFile(creatPdfWithReportHeader(title, output),
            fileName = _ =>
              play.utils.UriEncoding.encodePathSegment(title + current.toString("YYMMdd_hhmm") + ".pdf", "UTF-8"))
      }
  }

  case class AqiSubIndex(name:String, value: Option[Float], aqi: Option[Float])
  case class AqiResult(time: String, aqi: Option[Float], subIndex: Seq[AqiSubIndex])
  def realtimeAQI() = Action {
    implicit request =>
      val currentHr = getLatestRecordTime(TableType.Hour).getOrElse(DateTime.now.withMinuteOfHour(0): Timestamp)
      val aqiMap = AQI.getRealtimeAQI(currentHr)
      val aqi: (Option[Float], Map[AQI.Value, (Option[Float], Option[Float])]) = aqiMap(Monitor.mvList(0))

      val subIndex = aqi._2.map(kp => { AqiSubIndex(kp._1.toString, kp._2._1, kp._2._2)})
      val ret = AqiResult(currentHr.toString, aqi._1, subIndex.toList)
      implicit val w = Json.writes[AqiSubIndex]
      implicit val w2 = Json.writes[AqiResult]
      Ok(Json.toJson(ret))
  }

  def realtimeImg = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      def listAllFiles = {
        //import java.io.FileFilter
        val allFiles = new java.io.File("\\\\PC-PC\\tsmc\\").listFiles().toList
        allFiles.filter(p => p != null).sortBy { f => f.lastModified() }.reverse
      }

      val imgFileList = listAllFiles
      if (!imgFileList.isEmpty) {
        import java.nio.file._
        Logger.info(imgFileList.head.getAbsolutePath)

        val srcPath = Paths.get(imgFileList.head.getAbsolutePath)

        imgFileList.drop(1).foreach { f => f.delete() }

        val realtimeImgPath = s"${current.path.getAbsolutePath}/public/images/realtime.jpg"
        val destPath = Paths.get(realtimeImgPath)

        Files.move(srcPath, destPath, StandardCopyOption.REPLACE_EXISTING)
      }

      Ok(views.html.realtimeImage(group.privilege))
  }

  def realtimeTrend() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      Ok(views.html.realtimeTrend(group.privilege, false))
  }

  def realtimeMinTrend() = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get
      Ok(views.html.realtimeTrend(group.privilege, true))
  }

  def realtimeHourTrendChart(monitorStr: String, monitorTypeStr: String) = Security.Authenticated {
    implicit request =>
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map {
        MonitorType.withName
      }

      val current = getLatestRecordTime(TableType.Hour).getOrElse(DateTime.now.withMinuteOfHour(0): Timestamp)
      val reportUnit = ReportUnit.Hour
      val monitorStatusFilter = MonitorStatusFilter.ValidData
      val start = current.toDateTime - 1.day
      val end = current.toDateTime + 1.hour

      import Query.trendHelper
      val chart = trendHelper(monitors, Array.empty[EpaMonitor.Value], monitorTypes, reportUnit, monitorStatusFilter, start, end)

      Results.Ok(Json.toJson(chart))
  }

  def realtimeMinTrendChart(monitorStr: String, monitorTypeStr: String) = Security.Authenticated {
    implicit request =>
      val monitorStrArray = monitorStr.split(':')
      val monitors = monitorStrArray.map {
        Monitor.withName
      }
      val monitorTypeStrArray = monitorTypeStr.split(':')
      val monitorTypes = monitorTypeStrArray.map {
        MonitorType.withName
      }

      val current = getLatestRecordTime(TableType.Min).getOrElse(DateTime.now.withSecond(0): Timestamp)
      val reportUnit = ReportUnit.Min
      val monitorStatusFilter = MonitorStatusFilter.ValidData
      val start = current.toDateTime - 4.hour
      val end = current.toDateTime + 1.minute

      import Query.trendHelper
      val chart = trendHelper(monitors, Array.empty[EpaMonitor.Value], monitorTypes, reportUnit, monitorStatusFilter, start, end)

      Results.Ok(Json.toJson(chart))
  }

  def highchartJson(monitorTypeStr: String) = Security.Authenticated {
    implicit request =>
      val userInfo = Security.getUserinfo(request).get
      val group = Group.getGroup(userInfo.groupID).get

      val mt = MonitorType.withName(monitorTypeStr)
      val mtCase = MonitorType.map(mt)

      val current = getLatestRecordTime(TableType.Min).getOrElse(DateTime.now: Timestamp)
      val currentDateTime: DateTime = current
      val latestRecordTime = current.toDateTime - 1.minutes

      import controllers.Query.trendHelper
      val chart = trendHelper(Monitor.mvList.toArray, Array.empty[EpaMonitor.Value], Array(mt), ReportUnit.Min,
        MonitorStatusFilter.ValidData, currentDateTime - 1.hour, currentDateTime)
      Ok(Json.toJson(chart))
  }

  def realtimeMap = Security.Authenticated {
    implicit request =>
      val current = getLatestRecordTime(TableType.Min).getOrElse(DateTime.now(): Timestamp)
      val sub_current = current.toDateTime - 1.minute
      val weatherMap = getRealtimeWeatherMap(sub_current)
      val statusMap = getRealtimeMonitorStatusMap(sub_current)

      def getStatusIndex(statusMapOpt: Option[Map[MonitorType.Value, Option[String]]]): (Int, String) = {
        val statusBuilder = new StringBuilder
        if (statusMapOpt.isEmpty)
          return (4, s"<strong>所有測項:${MonitorStatus.map(MonitorStatus.DATA_LOSS_STAT).desp}</strong>")

        val statusMap = statusMapOpt.get
        val statusIndexes = statusMap.map { mt_status =>
          val status = mt_status._2.getOrElse(MonitorStatus.NORMAL_STAT)
          if (MonitorStatus.isNormalStat(status))
            0
          else if (MonitorStatus.isCalbration(status)) {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            1
          } else if (MonitorStatus.isRepairing(status)) {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            2
          } else if (MonitorStatus.isMaintance(status)) {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            3
          } else {
            statusBuilder.append(s"${MonitorType.map(mt_status._1).desp}:${MonitorStatus.map(status).desp}<br/>")
            4
          }
        }

        if (statusIndexes.size == 0)
          (0, "")
        else
          (statusIndexes.max, statusBuilder.toString())
      }

      val mapInfos =
        for {
          m <- Monitor.mvList
          weather = weatherMap.getOrElse(m, WeatherStat(None, None))
          status = statusMap.get(m)
        } yield {
          val (statusIndex, statusStr) = getStatusIndex(status)
          MonitorInfo(m.toString(), statusIndex, weather.windDir.getOrElse(0f), weather.windSpeed.getOrElse(0f), statusStr)
        }

      //EPA monitor
      val WIND_DIR = MonitorType.WD_DIR
      val WIND_SPEED = MonitorType.WD_SPEED
      val currentHour = current.withMinuteOfHour(0).withSecondOfMinute(0).withMillisOfSecond(0) - 2.hour
      val epaWeatherMap = Record.getEpaHourMap(EpaMonitor.epaList,
        List(WIND_DIR, WIND_SPEED), currentHour)

      val epaMapInfos =
        for {
          m <- EpaMonitor.epaList
          weather = epaWeatherMap.getOrElse(m, Map.empty[MonitorType.Value, Float])
        } yield {
          MonitorInfo(m.toString(), 0, weather.getOrElse(WIND_DIR, 0), weather.getOrElse(WIND_SPEED, 0), "")
        }
      Ok(Json.toJson(RealtimeMapInfo(mapInfos ++ epaMapInfos)))
  }

  def alarmNofificationSocket = WebSocket.acceptWithActor[String, String] { request =>
    out =>
      AlarmNotifier.props(out)
  }

  implicit val monitorInfoWrite = Json.writes[MonitorInfo]
  implicit val mapInfoWrite = Json.writes[RealtimeMapInfo]

  case class MonitorInfo(id: String, status: Int, winDir: Float, winSpeed: Float, statusStr: String)

  case class RealtimeMapInfo(info: Seq[MonitorInfo])
}