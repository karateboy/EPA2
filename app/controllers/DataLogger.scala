package controllers

import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.Logger
import play.api.libs.json._
import play.api.mvc._

import java.sql.Timestamp
import java.util.Date

case class LatestRecordTime(time: Long)
case class RecordListID(time: Date, monitor: String)
case class RecordList(mtDataList: Seq[MtRecord], _id: RecordListID) {
  def toHourRecord(monitor: Monitor.Value) = {
    val tm = new Timestamp(_id.time.getTime)
    Record.HourRecord(monitor.toString(), tm, None, mtDataList)
  }
}

case class CalibrationJSON(var monitorType: String, startTime: Long, endTime: Long, zero_val: Option[Double],
                           span_std: Option[Double], span_val: Option[Double]) {
  def zero_dev = zero_val map {
    Math.abs(_)
  }

  def span_dev = {
    if (span_val.isDefined && span_std.isDefined)
      Some(Math.abs(span_val.get - span_std.get))
    else
      None
  }

  def span_dev_ratio =
    if (span_dev.isDefined && span_std.isDefined && span_std.get != 0)
      Some(span_dev.get / span_std.get)
    else
      None
}

case class Alarm2JSON(time: Long, src: String, level: Int, info: String)

class DataLogger extends Controller {
  implicit val latestRecordTimeWrite = Json.writes[LatestRecordTime]
  implicit val mtRecordRead = Json.reads[MtRecord]
  implicit val idReads = Json.reads[RecordListID]
  implicit val RecordListRead = Json.reads[RecordList]
  implicit val CalibrationRead = Json.reads[CalibrationJSON]

  def getRecordRange(tabType: TableType.Value)(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = models.Realtime.getLatestMonitorRecordTime(tabType, monitor)

    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  def getHourRecordRange = getRecordRange(TableType.Hour) _

  def getMinRecordRange = getRecordRange(TableType.Min) _

  def exportCSV(monitor: Monitor.Value, calibrated: Boolean = false)(recordList: RecordList) = {
    import scala.collection.mutable.StringBuilder
    val sb = new StringBuilder
    implicit val tm = new DateTime(recordList._id.time)
    implicit val calibrationMap = Calibration.getDailyCalibrationMap(monitor, tm)

    sb.append("Site,")
    sb.append("Date,")
    for (r <- recordList.mtDataList) {
      r.mtName match {
        case "NH3" =>
          sb.append("FLOW, Status,")

        case "TSP" =>
          sb.append("RT, Status,")

        case mt: String =>
          sb.append(mt + ", Status,")
      }
    }
    sb.deleteCharAt(sb.length - 1)
    sb.append("\n")
    sb.append(monitor.toString + ",")
    sb.append(tm.toString("YYYY-MM-dd HH:mm:ss") + ",")
    for (r <- recordList.mtDataList) {
      if (!calibrated) {
        r.mtName match {
          case _: String =>
            for(v<-r.value)
            sb.append(v)
            sb.append(",")
            sb.append(r.status)
            sb.append(",")
        }
      } else {
        val mt = MonitorType.withName(r.mtName)
        import Calibration._
        implicit val v = r.value.map(_.toFloat)

        if (canCalibrate(mt)) {
          val calibrated = doCalibrate(mt)
          sb.append(calibrated.get)
          sb.append(",")
        } else if (mt == MonitorType.NO2 &&
          canCalibrate(MonitorType.NOx) && canCalibrate(MonitorType.NO)) {
          //A293=> NO2, A223=>NOX, A283=> NO
          val recNOxOpt = recordList.mtDataList.find { r => r.mtName == "NOx" }
          val recNO_Opt = recordList.mtDataList.find { r => r.mtName == "NO" }

          val interpolatedNO2 =
            for {
              recNOx <- recNOxOpt
              recNO <- recNO_Opt
              NOx <- doCalibrate(MonitorType.NOx)(recNOx.value.map(_.toFloat), tm, calibrationMap)
              NO <- doCalibrate(MonitorType.NO)(recNO.value.map(_.toFloat), tm, calibrationMap)
            } yield NOx - NO

          if (interpolatedNO2.isDefined) {
            sb.append(interpolatedNO2.get)
          } else {
            sb.append(v.get)
          }
          sb.append(",")
        } else if (mt == MonitorType.NMHC &&
          canCalibrate(MonitorType.CH4) && canCalibrate(MonitorType.THC)) {
          //A296=>NMHC, A286=>CH4, A226=>THC
          val recCH4Opt = recordList.mtDataList.find { r => r.mtName == "CH4" }
          val recTHC_Opt = recordList.mtDataList.find { r => r.mtName == "THC" }

          val calibratedCH4 = doCalibrate(MonitorType.CH4)
          val calibratedTHC = doCalibrate(MonitorType.THC)
          val interpolatedNMHC =
            for {
              recCH4 <- recCH4Opt
              recTHC <- recTHC_Opt
              ch4 <- doCalibrate(MonitorType.CH4)(recCH4.value.map(_.toFloat), tm, calibrationMap)
              thc <- doCalibrate(MonitorType.THC)(recTHC.value.map(_.toFloat), tm, calibrationMap)
            } yield thc - ch4

          if (interpolatedNMHC.isDefined) {
            sb.append(interpolatedNMHC.get)
          } else {
            sb.append(v.get)
          }
          sb.append(",")
        } else {
          sb.append(v.get)
          sb.append(",")
        }
        sb.append(r.status)
        sb.append(",")
      }
    }
    sb.deleteCharAt(sb.length - 1)
    sb.append("\n")
    sb.toString()
  }

  //  def toHourRecord(monitor: Monitor.Value)(recordList: RecordList) = {
  //    import java.sql.Timestamp
  //    val tm = new Timestamp(recordList.time)
  //    val hr = Record.HourRecord(monitor.toString(), tm, None, recordList.mtDataList)
  //    hr
  //  }

  def upsertDataRecord(tabType: TableType.Value)(monitorStr: String): Action[JsValue] = Action(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[RecordList]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        recordListSeq => {
          recordListSeq.foreach(recordList => recordList.mtDataList.foreach(mtRecord=>{
            if(mtRecord.mtName == "NOX")
              mtRecord.mtName = "NOx"
          } ))
          val hrList = recordListSeq.map {
            _.toHourRecord(monitor)
          }

          //Export
          import play.api.Play.current
          val path = if (tabType == TableType.Hour)
            current.path.getAbsolutePath + "/export/hour/"
          else
            current.path.getAbsolutePath + "/export/minute/"

          def saveCSV() = {
            try {
              recordListSeq map {
                recordList =>
                  import java.io.FileOutputStream
                  val time = new DateTime(recordList._id.time)
                  val csvStr = exportCSV(monitor, SystemConfig.getApplyCalibration)(recordList)
                  val fileName = s"${monitor.toString}_${time.toString("YYMMddHHmm")}.csv"
                  val os = new FileOutputStream(path + fileName)
                  os.write(csvStr.getBytes("UTF-8"))
                  os.close()
              }
            } catch {
              case ex: Throwable =>
                Logger.error("failed to export csv", ex)
            }
          }
          saveCSV

          val auditedHrList = hrList map {
            hr =>
              Auditor.auditHourRecord(monitor, Monitor.map(monitor).autoAudit, hr)
          }

          auditedHrList.foreach { hr =>
            try {
              hr.save(tabType)
              if(tabType == TableType.Hour)
                Uploader.upload(hr, path)
            } catch {
              case ex: Throwable =>
                Logger.error("Failed to insert=>", ex)
            }
          }

          Ok(Json.obj("ok" -> true))
        })
  }

  def unsertHourRecord = upsertDataRecord(TableType.Hour) _

  def unsertMinRecord = upsertDataRecord(TableType.Min) _

  def getCalibrationRange(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = Calibration.getLatestMonitorRecordTime(monitor)
    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  import Calibration._

  def toCalibrationItem(json: CalibrationJSON)(monitorStr: String) = {
    val monitor = Monitor.withName(monitorStr)
    if(json.monitorType == "NOX")
      json.monitorType = "NOx"
    val mt = MonitorType.withName(json.monitorType)

    CalibrationItem(monitor, mt,
      new DateTime(json.startTime), new DateTime(json.endTime), json.span_std.map {
        _.toFloat
      },
      Some(0), json.zero_val.map {
        _.toFloat
      },
      json.zero_dev.map {
        _.toFloat
      }, Some(0),
      json.span_std.map {
        _.toFloat
      }, json.span_val.map {
        _.toFloat
      },
      json.span_dev.map {
        _.toFloat
      }, json.span_dev_ratio.map {
        _.toFloat * 100
      })
  }

  def insertCalibrationRecord(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[CalibrationJSON]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        recordListSeq => {
          val calibrationList = recordListSeq.map {
            toCalibrationItem(_)(monitorStr)
          }
          calibrationList.foreach { calibration =>
            try {
              calibration.save
            } catch {
              case ex: Throwable =>
                Logger.error("Failed to insert calibration.", ex)
            }
          }
          Ok(Json.obj("ok" -> true))
        })
  }

  def exportCalibration(startStr: String, endStr: String) = Action {
    val start = DateTime.parse(startStr)
    val end = DateTime.parse(endStr)
    Logger.info(s"export $start to $end")

    var current = start
    while (current <= end) {
      Logger.info(s"export $current")
      for (m <- Monitor.mvList) {
        exportDailyCalibrationCSV(m, current)
      }
      current += 1.day
    }
    Ok("finished!")
  }

  def getAlarmRange(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = Alarm2.getLatestAlarmTime(monitor)
    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  def toAlarm2(json: Alarm2JSON)(monitorStr: String) = {
    val monitor = Monitor.withName(monitorStr)
    Alarm2(monitor, new DateTime(json.time), json.src, json.level, json.info)
  }

  def insertAlarmRecord(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      implicit val ar2JsonRead = Json.reads[Alarm2JSON]

      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[Alarm2JSON]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        alarm2JsonSeq => {
          val alarm2Seq = alarm2JsonSeq.map {
            toAlarm2(_)(monitorStr)
          }
          Alarm2.insertAlarmSeq(alarm2Seq)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getInstrumentStatusRange(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val timeOpt = InstrumentStatus.getLatestTime(monitor)
    val latestRecordTime = timeOpt.map {
      time =>
        LatestRecordTime(time.getMillis)
    }.getOrElse(LatestRecordTime(0))

    Ok(Json.toJson(latestRecordTime))
  }

  def insertInstrumentStatusRecord(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      import InstrumentStatus._
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[InstrumentStatusJSON]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        instrumentStatusSeq => {
          InstrumentStatus.insert(monitor, instrumentStatusSeq)
          Ok(Json.obj("ok" -> true))
        })
  }

  def getInstrumentStatusTypeIds(monitorStr: String) = Action {
    val monitor = Monitor.withName(monitorStr)
    val instrumentStatusTypeMapOpt = Monitor.map(monitor).instrumentStatusTypeMapOpt
    val instrumentStatusTypeIds = instrumentStatusTypeMapOpt.map { istMap =>
      istMap.map { map =>
        map.instrumentId + map.statusTypeSeq.mkString("")
      }.mkString("")
    }.getOrElse("")

    Ok(Json.toJson(instrumentStatusTypeIds))
  }

  def updateInstrumentStatusTypeMap(monitorStr: String) = Action(BodyParsers.parse.json) {
    implicit request =>
      import Monitor._
      val monitor = Monitor.withName(monitorStr)
      val result = request.body.validate[Seq[InstrumentStatusTypeMap]]
      result.fold(err => {
        Logger.error(JsError(err).toString())
        BadRequest(Json.obj("ok" -> false, "msg" -> JsError(err).toString().toString()))
      },
        newMap => {
          val newMonitor = Monitor.map(monitor).updateInstrumentStatusTypeMap(Some(newMap.toList))
          Monitor.updateInstrumentStatusTypeMap(newMonitor)
          Ok(Json.obj("ok" -> true))
        })
  }

}