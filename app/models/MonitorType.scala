package models

import play.api.i18n._
import play.api._
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import ModelHelper._

case class MonitorType(id: String, default_desp: String, unit: String,
                       std_internal_default: Option[Float], std_law: Option[Float], std_hour: Option[Float],
                       std_day: Option[Float], std_year: Option[Float],
                       zd_internal: Option[Float], zd_law: Option[Float],
                       sd_internal: Option[Float], sd_law: Option[Float],
                       epa_mapping: Option[String],
                       prec: Int, order: Int,
                       level1: Option[Float], level2: Option[Float], level3: Option[Float], level4: Option[Float]) {

  def desp(implicit messages: Messages) = {
    val key = s"mt.$id"
    if (Messages.isDefinedAt(key))
      Messages(key)
    else
      default_desp
  }
}

object MonitorType extends Enumeration {
  implicit val mtReads: Reads[MonitorType.Value] = EnumUtils.enumReads(MonitorType)
  implicit val mtWrites: Writes[MonitorType.Value] = EnumUtils.enumWrites

  def mapper(r: WrappedResultSet) = MonitorType(id = r.string(1),
    default_desp = r.string(2),
    unit = r.string(3),
    std_internal_default = r.floatOpt(5),
    std_law = r.floatOpt(6),
    std_hour = r.floatOpt(7),
    std_day = r.floatOpt(8),
    std_year = r.floatOpt(9),
    zd_internal = r.floatOpt(10),
    zd_law = r.floatOpt(11),
    sd_internal = r.floatOpt(12),
    sd_law = r.floatOpt(13),
    epa_mapping = r.stringOpt(14),
    prec = r.int(15),
    order = r.int(16),
    level1 = r.floatOpt(17),
    level2 = r.floatOpt(18),
    level3 = r.floatOpt(19),
    level4 = r.floatOpt(20))

  private def mtList: List[MonitorType] =
    DB readOnly { implicit session =>
      sql"""
        Select *
        From MonitorType
      """.map {
        mapper
      }.list.apply
    }

  var map: Map[Value, MonitorType] = Map(mtList.map { e => Value(e.id) -> e }: _*)
  val mtvAllList = mtList.map(mt => MonitorType.withName(mt.id))

  def mtvList = {
    var mtSet = Set.empty[MonitorType.Value]
    for (m <- Monitor.mvList) {
      mtSet = mtSet.union(Monitor.map(m).monitorTypes.toSet)
    }

    mtvAllList.filter {
      mtSet.contains
    }.sortBy {
      map(_).order
    }
  }

  def myMtvList(implicit p: Privilege) = {
    mtvList.filter {
      p.allowedMonitorTypes.contains
    }.sortBy {
      map(_).order
    }
  }

  def realtimeList = {
    var mtSet = Set.empty[MonitorType.Value]
    for (m <- Monitor.mvList) {
      mtSet = mtSet.union{
          val normalMtv = Monitor.map(m).monitorTypes.filter { mt => !MonitorType.map(mt).id.startsWith("_") }
          normalMtv.toSet
      }
    }

    mtSet.toList.sortBy {
      map(_).order
    }
  }

  def updateMonitorType(mt: MonitorType.Value, colname: String, newValue: String) = {
    DB localTx { implicit session =>
      val updateValue =
        if (newValue == "-")
          None
        else if (colname.equalsIgnoreCase("DESP") || colname.equalsIgnoreCase("UNIT")) {
          Some(newValue)
        } else {
          import java.lang.Float
          val v = Float.parseFloat(newValue)
          Some(v)
        }

      val col = SQLSyntax.createUnsafely(s"${colname}")
      sql"""
        Update MonitorType
        Set ${col}=${updateValue}
        Where ITEM=${mt.toString}  
        """.update.apply

      val old = map(mt)

      val newMtOpt =
        sql"""
          Select *
          From MonitorType
          Where ITEM=${mt.toString}
        """.map {
          mapper
        }.single.apply
      map = (map + (mt -> newMtOpt.get))
    }
  }

  val TSP = MonitorType.withName("TSP")
  val PM10 = MonitorType.withName("PM10")
  val PM25 = MonitorType.withName("PM25")
  val TS = MonitorType.withName("TS")
  val SO2 = MonitorType.withName("SO2")
  val NOx = MonitorType.withName("NOx")
  val CO = MonitorType.withName("CO")
  val O3 = MonitorType.withName("O3")
  val THC = MonitorType.withName("THC")
  val NH3 = MonitorType.withName("NH3")
  val NOY = MonitorType.withName("NOY")
  val NOY_NO = MonitorType.withName("NOY_NO")
  val H2S = MonitorType.withName("H2S")
  val NO = MonitorType.withName("NO")
  val CH4 = MonitorType.withName("CH4")
  val RHUMID = MonitorType.withName("RHUMID")
  val RT = MonitorType.withName("RT")
  val NO2 = MonitorType.withName("NO2")
  val NMHC = MonitorType.withName("NMHC")
  val WD_SPEED = MonitorType.withName("WD_SPEED")
  val WD_DIR = MonitorType.withName("WD_DIR")
  val RAIN = MonitorType.withName("RAIN")
  val TEMP = MonitorType.withName("TEMP")
  val HUMID = MonitorType.withName("HUMID")
  val PRESS = MonitorType.withName("PRESS")
  //New Mt

  val psiList = List(PM10, SO2, CO, O3, NO2)
  val aqiList = List(O3, PM25, PM10, CO, SO2, NO2)
  val windDirList = List(MonitorType.withName("WD_DIR"))

  val monitorReportList =
    List(SO2, NOx, NO2, NO, CO, O3, THC, CH4, NMHC, NH3, TS, TSP, PM10, PM25,
      WD_SPEED, WD_DIR, TEMP, HUMID, PRESS, RAIN)

  val calibrationList = List(SO2, NOx, NO2, NO, CO, O3, THC, CH4, NMHC, NH3, TS,
    TSP, PM10, PM25,
    RHUMID, RT)

  val epaList =
    List(PM10, PM25, SO2, NOx, CO, O3, THC, NO, CH4, NO2, NMHC, WD_SPEED, WD_DIR, RAIN, TEMP, HUMID)

  val eapIdMap = {
    val pairs =
      for(epaMt <- epaList if MonitorType.map(epaMt).epa_mapping.isDefined)
        yield {
          MonitorType.map(epaMt).epa_mapping.get.toInt -> epaMt
        }
    pairs.toMap
  }

  val epaReportList =
    List(WD_DIR, WD_SPEED, SO2, NO2, CO, O3, PM10, THC, NMHC, PM25)

  val epaMap = {
    map.filter(p => p._2.epa_mapping.isDefined).map(kv => (kv._2.epa_mapping.get, kv._1))
  }
  
  val epaIdMap = {
    val pairs = 
      for(epaMt <- epaList if MonitorType.map(epaMt).epa_mapping.isDefined)
        yield {
        MonitorType.map(epaMt).epa_mapping.get.toInt -> epaMt
      }
    pairs.toMap
  }

  import com.github.nscala_time.time.Imports._
  import java.sql.Timestamp

  def getManualAuditTooltip(m: Monitor.Value, mt: MonitorType.Value, v: (Option[Float], Option[String]),
                            dataTime: DateTime, tabType: TableType.Value = TableType.Hour): String = {
    if (v._1.isEmpty || v._2.isEmpty)
      return ""

    val tagInfo = MonitorStatus.getTagInfo(v._2.get)
    if (tagInfo.statusType != StatusType.Manual)
      return ""

    val auditLogOpt = ManualAuditLog.getLog(tabType, m, dataTime, mt.toString())
    if (auditLogOpt.isEmpty)
      return ""

    val log = auditLogOpt.get
    val reason = if (log.reason.isDefined)
      log.reason.get
    else
      "無"

    return s"""
      title=${log.modified_time.toString("YYYY-MM-dd-HH:mm")}-${log.operator}註記-理由:${reason}
      data-toggle=tooltip data-container=body data-trigger=hover
      """
  }

  def getStyleStr(m: Monitor.Value, mt: MonitorType.Value, v: (Option[Float], Option[String])) = {
    val mtCase = map(mt)
    if (v._1.isEmpty || v._2.isEmpty)
      s"Color:Black;background-color:White"
    else {
      val value = v._1.get
      val status = v._2.get

      val internal_std = Monitor.map(m).getStdInternal(mt)
      val overInternal =
        if (internal_std.isDefined && (value > internal_std.get))
          true
        else
          false

      val overLaw =
        if (mtCase.std_law.isDefined && (value > mtCase.std_law.get))
          true
        else
          false

      MonitorStatus.getCssStyleStr(status, overInternal, overLaw)
    }
  }

  def format(mt: MonitorType.Value, v: Option[Float]) = {
    if (v.isEmpty)
      "-"
    else {
      val prec = map(mt).prec
      s"%.${prec}f".format(v.get)
    }
  }

  def formatAvg(avg: Option[Float]) = {
    if (avg.isEmpty)
      "-"
    else {
      s"%.0f".format(avg.get * 100)
    }
  }

  def formatValue(v: Option[Float]) = {
    if (v.isEmpty)
      "-"
    else {
      s"%.0f".format(v.get)
    }
  }
}