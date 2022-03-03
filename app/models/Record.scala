package models
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import play.api.libs.json._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import play.api.i18n._

case class Stat(
  avg:       Option[Float],
  min:       Option[Float],
  max:       Option[Float],
  count:     Int,
  total:     Int,
  overCount: Int) {
  val effectPercent = if (total != 0) Some(count.toFloat * 100 / total) else None
  val overPercent = if (total != 0) Some(overCount.toFloat * 100 / total) else None
}

case class MonitorTypeRecord(monitorType: MonitorType.Value, dataList: List[(Timestamp, Option[Float], Option[String])], stat: Stat)
case class DailyReport(
  typeList: Seq[MonitorTypeRecord])

object TableType extends Enumeration {
  val Min = Value("Min")
  val Hour = Value("Hour")
  val defaultMap = Map((Min -> "分鐘資料"), (Hour -> "小時資料"))
  def map(key: TableType.Value)(implicit messages: Messages) = {
    val messageKey = s"dataSet.$key"
    if (Messages.isDefinedAt(messageKey))
      Messages(messageKey)
    else
      defaultMap(key)
  }
}

case class MtRecord(mtName: String, value: Option[Double], status: String)

object Record {
  case class HourRecord(
    monitor:  String,
    date:     Timestamp,
    chk:      Option[String] = None,
    dataList: Seq[MtRecord]  = Seq.empty[MtRecord]) {

    def save(tab: TableType.Value) {
      val tab_name = Record.getTabName(tab)
      implicit val write = Json.writes[MtRecord]

      DB localTx { implicit session =>
        sql"""
          IF NOT EXISTS (SELECT * FROM $tab_name WHERE DP_NO = $monitor and M_DateTime = $date)
            INSERT INTO $tab_name
             ([DP_NO], [M_DateTime], [CHK], [dataList])
            VALUES
           ($monitor, $date, $chk, ${Json.toJson(dataList).toString})
          ELSE
            UPDATE $tab_name
            SET dataList = ${Json.toJson(dataList).toString}
            WHERE DP_NO = $monitor and M_DateTime = $date
            """.update.apply
      }
    }

    def valueMap: Map[MonitorType.Value, (Option[Float], Option[String])] = {
      val pairSeq = dataList flatMap {
        mtRecord =>
          try {
            val mt = MonitorType.withName(mtRecord.mtName)
            val value = (mtRecord.value.map(_.toFloat): Option[Float], Some(mtRecord.status): Option[String])
            Some(mt -> value)
          } catch {
            case _: java.util.NoSuchElementException =>
              None
          }
      }

      pairSeq.toMap
    }

    def recordMap = {
      val vMap = valueMap
      def optMap(mt: MonitorType.Value) = {
        vMap.getOrElse(mt, (None, None))
      }
      optMap _
    }
  }

  case class SixSecRecord(
    monitor:       Monitor.Value,
    time:          DateTime,
    winSpeed:      Seq[Option[Float]],
    winSpeed_stat: Seq[Option[String]],
    winDir:        Seq[Option[Float]],
    winDir_stat:   Seq[Option[String]])

  type MinRecord = HourRecord

  def emptySixSecRecord(m: Monitor.Value, t: DateTime, status: String) = SixSecRecord(m, t,
    List.fill(10)(Some(0f)),
    List.fill(10)(Some(MonitorStatus.DATA_LOSS_STAT)),
    List.fill(10)(Some(0f)),
    List.fill(10)(Some(MonitorStatus.DATA_LOSS_STAT)))

  def mapper(rs: WrappedResultSet) = {
    implicit val reads = Json.reads[MtRecord]

    val dataList = Json.parse(rs.string(4)).validate[Seq[MtRecord]].getOrElse(Seq.empty[MtRecord])
    HourRecord(rs.string(1), rs.timestamp(2), rs.stringOpt(3), dataList)
  }

  def sixSecMapper(rs: WrappedResultSet) = {
    val windSpeed =
      for (loc <- 0 to 9) yield {
        rs.floatOpt(4 + loc * 2)
      }

    val windSpeed_stat =
      for (loc <- 0 to 9) yield {
        rs.stringOpt(5 + loc * 2)
      }

    val windDir =
      for (loc <- 0 to 9) yield {
        rs.floatOpt(24 + loc * 2)
      }

    val windDir_stat =
      for (loc <- 0 to 9) yield {
        rs.stringOpt(24 + loc * 2)
      }

    SixSecRecord(Monitor.withName(rs.string(1)), rs.timestamp(2), windSpeed, windSpeed_stat, windDir, windDir_stat)
  }

  def getHourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()

    if (startTime == endTime)
      List.empty[HourRecord]
    else {
      val tab_name = getTabName(TableType.Hour)
      val result = sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
      result
    }
  }

  def getUncheckedHourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorName = monitor.toString()

    if (startTime == endTime)
      List.empty[HourRecord]
    else {

      val tab_name = getTabName(TableType.Hour)
      sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end} and CHK is Null
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
    }
  }

  def getInvalidHourRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime

    if (startTime == endTime)
      List.empty[HourRecord]
    else {

      val monitorName = monitor.toString()
      val tab_name = getTabName(TableType.Hour)
      sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end} and CHK = 'BAD'
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
    }
  }

  def getMinRecords(monitor: Monitor.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession): List[Record.HourRecord] = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime

    if (startTime == endTime)
      List.empty[HourRecord]
    else {

      val monitorName = monitor.toString()
      val tab_name = getTabName(TableType.Min)
      sql"""
        Select * 
        From ${tab_name}
        Where DP_NO=${monitorName} and M_DateTime >= ${start} and M_DateTime < ${end}
        ORDER BY M_DateTime ASC
      """.map { mapper }.list().apply()
    }
  }

  def secRecordProject(mt: MonitorType.Value) =
    (rs: SixSecRecord) => {
      assert(mt == MonitorType.WD_SPEED || mt == MonitorType.WD_DIR)
      val start = rs.time
      val values =
        for (i <- 0 to 9) yield {
          if (mt == MonitorType.WD_SPEED) {
            (start + (6 * i).second, (rs.winSpeed(i), rs.winSpeed_stat(i)))
          } else {
            (start + (6 * i).second, (rs.winDir(i), rs.winDir_stat(i)))
          }
        }
      values.toList
    }

  val timeProjection: (HourRecord => Timestamp) = {
    rs => rs.date
  }

  def monitorTypeProject2(mt: MonitorType.Value) = {
    def projection(hr: HourRecord) = {
      hr.recordMap(mt)
    }
    projection _
  }

  def updateRecordStatus(tabType: TableType.Value, monitor: Monitor.Value,
                         monitorType: String, mill: Long, newStatus: String)(implicit session: DBSession = AutoSession) = {
    val recordTime: DateTime = new Timestamp(mill)
    val monitorName = monitor.toString()
    val tab_name = getTabName(tabType)

    val hr = if (tabType == TableType.Hour)
      Record.getHourRecords(monitor, recordTime, recordTime + 1.hour).head
    else
      Record.getMinRecords(monitor, recordTime, recordTime + 1.minute).head

    val updatedDataList = hr.dataList.map { mtr =>
      //val mt = MonitorType.withName(mtr.mtName)
      if (mtr.mtName == monitorType.toString())
        MtRecord(mtr.mtName, mtr.value, newStatus)
      else
        mtr
    }
    val newHr = HourRecord(hr.monitor, hr.date, hr.chk, updatedDataList)
    newHr.save(tabType)
  }

  def emptyRecord(monitor: String, start: DateTime) = {
    HourRecord(
      monitor,
      start,
      None, Seq.empty[MtRecord])
  }

  case class RecordValidationReport(start: DateTime, end: DateTime,
                                    hourReport: Map[Monitor.Value, Int],
                                    minReport:  Map[Monitor.Value, Int])

  def getRecordValidationReport(start: DateTime, end: DateTime) = {
    DB readOnly { implicit session =>
      val tab_name = getTabName(TableType.Hour)
      val hrRecords =
        sql"""
        SELECT DP_NO, count(DP_NO)
        FROM ${tab_name}
        Where M_DateTime >= ${start} and M_DateTime < ${end}
        GROUP BY DP_NO
      """.map { rs => (Monitor.withName(rs.string(1)), rs.int(2)) }.list().apply()

      val hourReport = Map(hrRecords: _*)

      val mintab_name = getTabName(TableType.Min)
      val minRecords =
        sql"""
        SELECT DP_NO, count(DP_NO)
        FROM ${mintab_name}
        Where M_DateTime >= ${start} and M_DateTime < ${end}
        GROUP BY DP_NO
      """.map { rs => (Monitor.withName(rs.string(1)), rs.int(2)) }.list().apply()

      val minReport = Map(minRecords: _*)

      RecordValidationReport(start, end, hourReport, minReport)
    }
  }

  def getDays(current: DateTime, endTime: DateTime): List[DateTime] = {
    if (current == endTime)
      Nil
    else
      current :: getDays(current + 1.days, endTime)
  }

  def windAvg(sum_sin: Double, sum_cos: Double) = {
    val degree = Math.toDegrees(Math.atan2(sum_sin, sum_cos)).toFloat
    if (degree >= 0)
      degree
    else
      degree + 360
  }

  type RecordT = (Timestamp, Option[Float], Option[String])
  def windAvg(windSpeed: List[RecordT], windDir: List[RecordT]): Float = {
    def validFilter(t: RecordT) = {
      if (t._2.isEmpty)
        false
      else {
        t._3 match {
          case Some(stat) => MonitorStatus.isNormalStat(stat)
          case _          => false
        }
      }
    }

    if (windSpeed.length != windDir.length)
      Logger.error(s"windSpeed #=${windSpeed.length} windDir #=${windDir.length}")

    val windRecord = windSpeed.zip(windDir)
    val validWind = windRecord.filter(t => validFilter(t._1) && validFilter(t._2)).map(r => (r._1._2.get, r._2._2.get))
    val wind_sin = validWind.map(v => v._1 * Math.sin(Math.toRadians(v._2))).sum
    val wind_cos = validWind.map(v => v._1 * Math.cos(Math.toRadians(v._2))).sum
    windAvg(wind_sin, wind_cos)
  }

  def windAvgF(windSpeed: List[Float], windDir: List[Float]): Float = {
    if (windSpeed.length != windDir.length)
      Logger.error(s"windSpeed #=${windSpeed.length} windDir #=${windDir.length}")

    val windRecord = windSpeed.zip(windDir)
    val wind_sin = windRecord.map(v => v._1 * Math.sin(Math.toRadians(v._2))).sum
    val wind_cos = windRecord.map(v => v._1 * Math.cos(Math.toRadians(v._2))).sum
    windAvg(wind_sin, wind_cos)
  }

  def getPeriodReport(monitor: Monitor.Value, start: DateTime, period: Period, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                      monitorStatusFilter: MonitorStatusFilter.Value = MonitorStatusFilter.ValidData) = {
    DB localTx { implicit session =>

      val isHourRecord = (start + period >= start + 1.hour)
      val end = start + period
      val totalPeriod = new Period(start, end)

      val originalPeriodRecordList =
        if (isHourRecord)
          getHourRecords(monitor, start, start + period)
        else
          getMinRecords(monitor, start, start + period)

      val reportList =
        originalPeriodRecordList

      def statusFilter(data: (DateTime, (Option[Float], Option[String]))): Boolean = {
        if (data._2._1.isEmpty || data._2._2.isEmpty)
          return false

        val stat = data._2._2.get

        MonitorStatusFilter.isMatched(monitorStatusFilter, stat)
      }

      val usedMonitoredTypes = Monitor.map(monitor).monitorTypes.filter { includeTypes.contains(_) }

      val actualMonitoredTypes =
        if (usedMonitoredTypes.length == 0)
          includeTypes
        else
          usedMonitoredTypes

      val typeResultList =
        for {
          mt <- includeTypes
          t = monitorTypeProject2(mt)
          total = reportList.size
          projections = reportList.map(rs => (rs.date, t(rs)._1, t(rs)._2))
          validStat = { t: (Timestamp, Option[Float], Option[String]) =>
            statusFilter(t._1, (t._2, t._3))
          }

          validValues = projections.filter(validStat).map(t => t._2.getOrElse {
            Logger.error("#1 Unexpected Null value! " + t._1.toString())
            0f
          })
          count = validValues.length
        } yield {
          val avg = if (MonitorType.windDirList.contains(mt)) {
            val windDir = projections
            val wsT = monitorTypeProject2(MonitorType.WD_SPEED)
            val windSpeed = reportList.map(rs => (rs.date, wsT(rs)._1, wsT(rs)._2))
            windAvg(windSpeed, windDir)
          } else {
            val sum = validValues.sum
            if (count != 0) sum / count else 0
          }

          val stat =
            if (count != 0) {
              val max = validValues.max
              val min = validValues.min
              Stat(Some(avg), Some(min), Some(max), count, total, 0)
            } else
              Stat(None, None, None, count, total, 0)

          MonitorTypeRecord(mt, projections, stat)
        }

      DailyReport(typeResultList)
    }
  }

  def getDailyReport(monitor: Monitor.Value, start: DateTime, includeTypes: List[MonitorType.Value] = MonitorType.monitorReportList,
                     monitorStatusFilter: MonitorStatusFilter.Value = MonitorStatusFilter.ValidData) = {

    DB readOnly { implicit session =>
      //Apply calibration
      val calibrationMap = Calibration.getDailyCalibrationMap(monitor, start)

      val originalList = getHourRecords(monitor, start, start + 1.day)
      val recordMap = Map(originalList.map { r => r.date -> r }: _*)

      val reportList =
        if (originalList.length == 24)
          originalList
        else {
          val endTime = start + 1.day
          import controllers.Report

          for (t <- getPeriods(start, endTime, 1.hour))
            yield recordMap.getOrElse(t, emptyRecord(monitor.toString(), t))
        }

      def statusFilter(data: (DateTime, (Option[Float], Option[String]))): Boolean = {
        if (data._2._1.isEmpty || data._2._2.isEmpty)
          return false

        val stat = data._2._2.get

        MonitorStatusFilter.isMatched(monitorStatusFilter, stat)
      }

      val usedMonitoredTypes = Monitor.map(monitor).monitorTypes.filter { includeTypes.contains(_) }

      val actualMonitoredTypes =
        if (usedMonitoredTypes.length == 0)
          includeTypes
        else
          usedMonitoredTypes

      val typeResultList =
        for {
          mt <- includeTypes
          t = monitorTypeProject2(mt)
          total = recordMap.size
          projections = reportList.map { rs =>
            def canCalibrate(mt: MonitorType.Value) = {
              calibrationMap.contains(mt) &&
                findCalibration(calibrationMap(mt)).isDefined
            }

            def doCalibrate(mt: MonitorType.Value) = {
              val isTHCcalibrated = Play.current.configuration.getBoolean("THC.calibrated").getOrElse(true)
              if (!isTHCcalibrated && mt == MonitorType.THC) {
                monitorTypeProject2(mt)(rs)._1
              } else
                findCalibration(calibrationMap(mt)).get._2.calibrate(monitorTypeProject2(mt)(rs)._1)
            }

            def findCalibration(calibrationList: List[(DateTime, Calibration.CalibrationItem)]) = {
              val candidate = calibrationList.takeWhile(p => p._1 < rs.date)
              if (candidate.length == 0)
                None
              else
                Some(candidate.last)
            }

            if (SystemConfig.getApplyCalibration && canCalibrate(mt)) {
              val calibrated = doCalibrate(mt)
              (rs.date, calibrated, t(rs)._2)
            } else if (SystemConfig.getApplyCalibration && mt == MonitorType.NO2 &&
              canCalibrate(MonitorType.NOx) && canCalibrate(MonitorType.NO)) {
              //A293=> NO2, A223=>NOX, A283=> NO
              val calibratedNOx = doCalibrate(MonitorType.NOx)
              val calibratedNO = doCalibrate(MonitorType.NO)
              val interpolatedNO2 =
                for (NOx <- calibratedNOx; NO <- calibratedNO)
                  yield NOx - NO

              (rs.date, interpolatedNO2, t(rs)._2)
            } else if (SystemConfig.getApplyCalibration && mt == MonitorType.NMHC &&
              canCalibrate(MonitorType.CH4) && canCalibrate(MonitorType.THC)) {
              //A296=>NMHC, A286=>CH4, A226=>THC
              val calibratedCH4 = doCalibrate(MonitorType.CH4)
              val calibratedTHC = doCalibrate(MonitorType.THC)

              val interpolatedNMHC =
                for (ch4 <- calibratedCH4; thc <- calibratedTHC)
                  yield thc - ch4

              (rs.date, interpolatedNMHC, t(rs)._2)
            } else
              (rs.date, t(rs)._1, t(rs)._2)
          }
          validStat = { t: (Timestamp, Option[Float], Option[String]) =>
            statusFilter(t._1, (t._2, t._3))
          }

          validValues = projections.filter(validStat).map(t => t._2.getOrElse {
            Logger.error("#2 Unexpected Null value!")
            0f
          })
          count = validValues.length

        } yield {
          val stat =
            if (count >= 16) {
              val avg = if (MonitorType.windDirList.contains(mt)) {
                val windDir = projections
                val windSpeedT = monitorTypeProject2(MonitorType.WD_SPEED)
                val windSpeed = reportList.map(rs => (rs.date, windSpeedT(rs)._1, windSpeedT(rs)._2))
                windAvg(windSpeed, windDir)
              } else {
                val sum = validValues.sum
                if (count != 0) sum / count else 0
              }
              val max = validValues.max
              val min = validValues.min
              Stat(Some(avg), Some(min), Some(max), count, total, 0)
            } else
              Stat(None, None, None, count, total, 0)
          MonitorTypeRecord(mt, projections, stat)
        }

      DailyReport(typeResultList)
    }
  }

  case class MonitorEffectiveRate(monitor: Monitor.Value, rateMap: Map[MonitorType.Value, Float])
  def getMonitorEffectiveRate(monitor: Monitor.Value, start: DateTime): MonitorEffectiveRate = {
    val end = start + 1.month
    getMonitorEffectiveRate(monitor, start, end)
  }

  def getMonitorEffectiveRate(monitor: Monitor.Value, start: DateTime, end: DateTime) = {
    val records = Record.getHourRecords(monitor, start, end)
    val duration = new Interval(start, end).toDuration()
    val expected_count = duration.getStandardHours
    val ratePair =
      for {
        mt <- MonitorType.mtvList
        mtList = records.map(monitorTypeProject2(mt))
        count = mtList.count(r => (r._1.isDefined && r._2.isDefined && MonitorStatus.isNormalStat(r._2.get)))
      } yield {
        (mt -> count.toFloat / expected_count)
      }
    val rateMap = Map(ratePair: _*)
    MonitorEffectiveRate(monitor, rateMap)
  }

  case class MonitorTypeEffectiveRate(monitorType: MonitorType.Value, rateMap: Map[Monitor.Value, Float])
  def getMonitorTypeEffectiveRate(monitorType: MonitorType.Value, start: DateTime) = {
    val end = start + 1.month

    val duration = new Interval(start, end).toDuration()
    val expected_count = duration.getStandardHours
    val ratePair =
      for {
        m <- Monitor.mvList
        records = Record.getHourRecords(m, start, end)
        mtList = records.map(monitorTypeProject2(monitorType))
        count = mtList.count(r => (r._1.isDefined && r._2.isDefined && MonitorStatus.isNormalStat(r._2.get)))
      } yield {
        (m -> count.toFloat / expected_count)
      }
    val rateMap = Map(ratePair: _*)
    MonitorTypeEffectiveRate(monitorType, rateMap)
  }
  def getMonitorTypeYearlyEffectiveRate(monitorType: MonitorType.Value, start: DateTime) = {
    val end = start + 1.year
    var current = start
    import scala.collection.mutable.ListBuffer
    val result = ListBuffer[MonitorTypeEffectiveRate]()
    while (current < end) {
      result += getMonitorTypeEffectiveRate(monitorType, current)
      current += 1.month
    }
    result.toList
  }

  def getMonitorYearlyEffectiveRate(monitor: Monitor.Value, start: DateTime) = {
    val end = start + 1.year
    var current = start
    import scala.collection.mutable.ListBuffer
    val result = ListBuffer[MonitorEffectiveRate]()
    while (current < end) {
      result += getMonitorEffectiveRate(monitor, current)
      current += 1.month
    }
    result.toList
  }

  def getStatMonitorEffectiveRate(rateList: List[MonitorEffectiveRate]) = {
    val statList =
      for {
        mt <- MonitorType.mtvList
        mtRateList = rateList.map(r => r.rateMap(mt))
        count = mtRateList.count(r => r != 0)
        sum = mtRateList.sum
        avg = if (count != 0)
          sum / count else 0
      } yield if (count != 0)
        (mt -> Stat(Some(avg), Some(mtRateList.filter { _ != 0 }.min), Some(mtRateList.max), count, mtRateList.length, 0))
      else
        (mt -> Stat(None, None, None, count, mtRateList.length, 0))

    Map(statList: _*)
  }

  def getStatYearlyMonthlyEffectiveRate(rateList: List[MonitorTypeEffectiveRate]) = {
    val statList =
      for {
        m <- Monitor.mvList
        mRateList = rateList.map(r => r.rateMap(m))
        count = mRateList.count(r => r != 0)
        sum = mRateList.sum
        avg = if (count != 0)
          sum / count else 0
      } yield if (count != 0)
        (m -> Stat(Some(avg), Some(mRateList.filter { _ != 0 }.min), Some(mRateList.max), count, mRateList.length, 0))
      else
        (m -> Stat(None, None, None, count, mRateList.length, 0))

    Map(statList: _*)
  }

  def getWindRose(monitor: Enumeration#Value, epa: Boolean, monitorType: MonitorType.Value, start: DateTime, end: DateTime, level: List[Float], nDiv: Int = 16) = {
    val windRecords = if (!epa) {
      val records = getHourRecords(monitor.asInstanceOf[Monitor.Value], start, end)
      //Record.monitorTypeProject2(monitorType)
      records.map { r =>
        val mtValue = Record.monitorTypeProject2(monitorType)(r)._1
        val wind_dir = r.recordMap(MonitorType.WD_DIR)._1
        (wind_dir, mtValue)
      }
    } else {
      val mtValue = getEpaHourRecord(monitor.asInstanceOf[EpaMonitor.Value], monitorType, start, end)
      val mtValueMap = mtValue.map { r => r.time -> r.value }.toMap
      val windDir = getEpaHourRecord(monitor.asInstanceOf[EpaMonitor.Value], MonitorType.WD_DIR, start, end)

      val windDirMap = windDir.map { r => r.time -> r.value }.toMap
      for (time <- getPeriods(start, end, 1.hour))
        yield (windDirMap.get(time), mtValueMap.get(time))

    }

    assert(windRecords.length != 0)

    val step = 360f / nDiv
    import scala.collection.mutable.ListBuffer
    val windDirPair =
      for (d <- 0 to nDiv - 1) yield {
        (d -> ListBuffer[Float]())
      }
    val windMap = Map(windDirPair: _*)

    var total = 0
    for (w <- windRecords) {
      if (w._1.isDefined && w._2.isDefined) {
        val dir = (Math.ceil((w._1.get - (step / 2)) / step).toInt) % nDiv
        windMap(dir) += w._2.get
        total += 1
      }
    }

    def winSpeedPercent(winSpeedList: ListBuffer[Float]) = {
      val count = new Array[Float](level.length + 1)
      def getIdx(v: Float): Int = {
        for (i <- 0 to level.length - 1) {
          if (v < level(i))
            return i
        }

        return level.length
      }

      for (w <- winSpeedList) {
        val i = getIdx(w)
        count(i) += 1
      }

      assert(total != 0)
      count.map(_ * 100 / total)
    }

    windMap.map(kv => (kv._1, winSpeedPercent(kv._2)))
  }

  def getComparedList(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime, nYear: Int) = {
    val lastYearStart = start - 1.year
    val lastYearEnd = end - 1.year

    def f(r: HourRecord) = {
      (timeProjection(r), monitorTypeProject2(monitorType)(r))
    }

    val result =
      for {
        i <- 0 to nYear - 1
        records = getHourRecords(monitor, start - i.year, end - i.year)
      } yield records.map(f).filter(t => t._2._1.isDefined && t._2._2.isDefined && MonitorStatus.isValid(t._2._2.get))

    result
  }

  def getRegressionData(monitor: Monitor.Value, monitorType: MonitorType.Value, start: DateTime, end: DateTime) = {

    val records = getHourRecords(monitor, start, end)

    val typeRecords = records.map {
      r => (timeProjection(r), monitorTypeProject2(monitorType)(r))
    }

    typeRecords
  }

  def getTabName(tab: TableType.Value) = {
    tab match {
      case TableType.Hour =>
        SQLSyntax.createUnsafely(s"[HourRecord2]")
      case TableType.Min =>
        SQLSyntax.createUnsafely(s"[MinRecord2]")
    }
  }

  case class EpaHourRecord(monitor: EpaMonitor.Value, time: DateTime, monitorType: MonitorType.Value, value: Float)
  def getEpaHourRecord(epaMonitor: EpaMonitor.Value, monitorType: MonitorType.Value, startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorId = EpaMonitor.map(epaMonitor).id
    val monitorTypeStrOpt = MonitorType.map(monitorType).epa_mapping
    if (monitorTypeStrOpt.isEmpty) {
      Logger.error(s"Epa mapping is empty for $monitorType")
      List.empty[EpaHourRecord]
    } else {
      val monitorTypeStr = monitorTypeStrOpt.get
      sql"""
        Select * 
        From hour_data
        Where MStation=${monitorId} and MItem=${monitorTypeStr} and MDate >= ${start} and MDate < ${end}
        ORDER BY MDate ASC
      """.map {
        rs => EpaHourRecord(EpaMonitor.idMap(rs.int(2)), rs.timestamp(3), MonitorType.epaMap(rs.string(4)), rs.float(5))
      }.list().apply()
    }
  }

  def getEpaHourMap(epaMonitorList: List[EpaMonitor.Value], monitorTypeList: List[MonitorType.Value], time: DateTime)(implicit session: DBSession = AutoSession) = {
    val monitorIdList = epaMonitorList.map(EpaMonitor.map(_).id)
    val mStr = SQLSyntax.createUnsafely(monitorIdList.mkString("('", "','", "')"))
    val monitorTypes = monitorTypeList.flatMap(MonitorType.map(_).epa_mapping)
    val mtStr = SQLSyntax.createUnsafely(monitorTypes.mkString("('", "','", "')"))
    val records =
      sql"""
        Select * 
        From hour_data
        Where MStation in ${mStr} and MItem in ${mtStr} and MDate = ${time: Timestamp}
      """.map {
        rs => EpaHourRecord(EpaMonitor.idMap(rs.int(2)), rs.timestamp(3), MonitorType.epaMap(rs.string(4)), rs.float(5))
      }.list().apply()

    var recordMap = Map.empty[EpaMonitor.Value, Map[MonitorType.Value, Float]]

    records.foreach { r =>
      val mtMap = recordMap.getOrElse(r.monitor, Map.empty[MonitorType.Value, Float])
      val newMtMap = mtMap ++ Map(r.monitorType -> r.value)
      recordMap = recordMap + (r.monitor -> newMtMap)
    }
    recordMap
  }

  def getEpaRecordMap(epaMonitorList: List[EpaMonitor.Value], monitorTypeList: List[MonitorType.Value], 
      startTime: DateTime, endTime: DateTime)(implicit session: DBSession = AutoSession) = {
    val start: Timestamp = startTime
    val end: Timestamp = endTime
    val monitorIdList = epaMonitorList.map(EpaMonitor.map(_).id)
    val mStr = SQLSyntax.createUnsafely(monitorIdList.mkString("('", "','", "')"))
    val monitorTypes = monitorTypeList.flatMap(MonitorType.map(_).epa_mapping)
    val mtStr = SQLSyntax.createUnsafely(monitorTypes.mkString("('", "','", "')"))
    val records =
      sql"""
        Select * 
        From hour_data
        Where MStation in ${mStr} and MItem in ${mtStr} and  MDate >= ${start} and MDate < ${end}
      """.map {
        rs => EpaHourRecord(EpaMonitor.idMap(rs.int(2)), rs.timestamp(3), MonitorType.epaMap(rs.string(4)), rs.float(5))
      }.list().apply()

    import scala.collection.mutable.Map
    var recordMap = Map.empty[EpaMonitor.Value, Map[DateTime, Map[MonitorType.Value, Float]]]

    records.foreach { r =>
      val timeMap = recordMap.getOrElseUpdate(r.monitor, Map.empty[DateTime, Map[MonitorType.Value, Float]])
      val mtMap = timeMap.getOrElseUpdate(r.time, Map.empty[MonitorType.Value, Float])
      mtMap.put(r.monitorType, r.value)
    }
    recordMap
  }

  def compareEpaReport(monitor: Monitor.Value, epaMonitor: EpaMonitor.Value, start: DateTime, end: DateTime) = {
    val hrRecord = getHourRecords(monitor, start, end)
    val pairs =
      for {
        mt <- MonitorType.epaReportList
        mtRecord = hrRecord.map { rs =>
          (timeProjection(rs).toDateTime, monitorTypeProject2(mt)(rs))
        }
        mtMap = Map(mtRecord: _*)
      } yield {
        val data = mtRecord.filter(
          r => r._2._1.isDefined && r._2._2.isDefined && MonitorStatus.isValid(r._2._2.get)).map(_._2._1.get)
        val count = data.length
        val stat =
          if (count != 0) {
            if (MonitorType.windDirList.contains(mt)) {
              val windDir = mtRecord.map(r => (r._1: java.sql.Timestamp, r._2._1, r._2._2))
              val wsT = monitorTypeProject2(MonitorType.WD_SPEED)
              val windSpeed = hrRecord.map(rs => (rs.date: java.sql.Timestamp, wsT(rs)._1, wsT(rs)._2))
              val wind_avg = windAvg(windSpeed, windDir)
              Stat(Some(wind_avg), Some(data.min), Some(data.max), count, 24, 0)
            } else {
              Stat(Some(data.sum / count), Some(data.min), Some(data.max), count, 24, 0)
            }
          } else
            Stat(None, None, None, count, 24, 0)

        mt -> (mtMap, stat)
      }

    val localMap = Map(pairs: _*)

    val epaPairs =
      for {
        mt <- MonitorType.epaReportList
        epaRecord = getEpaHourRecord(epaMonitor, mt, start, end)
        epaPairs = epaRecord.map { r => r.time -> r }
        epaMap = Map(epaPairs: _*)
      } yield {
        val data = epaPairs.map(t => t._2.value)
        val count = data.length
        val stat =
          if (count != 0) {
            if (MonitorType.windDirList.contains(mt)) {
              val windDir = data
              val windSpeed = getEpaHourRecord(epaMonitor, MonitorType.WD_SPEED, start, end).map { r => r.value }
              val wind_avg = windAvgF(windSpeed, windDir)
              Stat(Some(wind_avg), Some(data.min), Some(data.max), count, 24, 0)
            } else {
              Stat(Some(data.sum / count), Some(data.min), Some(data.max), count, 24, 0)
            }
          } else
            Stat(None, None, None, count, 24, 0)

        mt -> (epaMap, stat)
      }
    val epaMap = Map(epaPairs: _*)

    (localMap, epaMap)
  }
}