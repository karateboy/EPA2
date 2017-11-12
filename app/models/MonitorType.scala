package models

import play.api.i18n._
import play.api._
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import ModelHelper._

case class MonitorType(id: String, unit: String,
                       std_internal_default: Option[Float], std_law: Option[Float], std_hour: Option[Float],
                       std_day: Option[Float], std_year: Option[Float],
                       zd_internal: Option[Float], zd_law: Option[Float],
                       sd_internal: Option[Float], sd_law: Option[Float],
                       epa_mapping: Option[String],
                       prec: Int, order: Int,
                       level1: Option[Float], level2: Option[Float], level3: Option[Float], level4: Option[Float]) {

  def desp(implicit messages: Messages) = {
    val key = s"mt.$id"
    Messages(key)
  }
}

object MonitorType extends Enumeration {
  implicit val mtReads: Reads[MonitorType.Value] = EnumUtils.enumReads(MonitorType)
  implicit val mtWrites: Writes[MonitorType.Value] = EnumUtils.enumWrites

  def mapper(r: WrappedResultSet) = MonitorType(id = r.string(1),
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
      mtSet = mtSet.union(Monitor.map(m).monitorTypes.toSet)
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
        else if(colname.equalsIgnoreCase("DESP")||colname.equalsIgnoreCase("UNIT")){
          Some(newValue)
        }else
        {
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

  val A213 = MonitorType.withName("TSP")
  val A214 = MonitorType.withName("PM10")
  val A215 = MonitorType.withName("PM25")
  val A221 = MonitorType.withName("TS")
  val A222 = MonitorType.withName("SO2")
  val A223 = MonitorType.withName("NOx")
  val A224 = MonitorType.withName("CO")
  val A225 = MonitorType.withName("O3")
  val A226 = MonitorType.withName("THC")
  val A229 = MonitorType.withName("NH3")
  val A232 = MonitorType.withName("NOY")
  val A233 = MonitorType.withName("NOY_NO")
  val A234 = MonitorType.withName("H2S")
  val A283 = MonitorType.withName("NO")
  val A286 = MonitorType.withName("CH4")
  val A288 = MonitorType.withName("RHUMID")
  val A289 = MonitorType.withName("RT")
  val A293 = MonitorType.withName("NO2")
  val A296 = MonitorType.withName("NMHC")
  val C211 = MonitorType.withName("WD_SPEED")
  val C212 = MonitorType.withName("WD_DIR")
  val C213 = MonitorType.withName("RAIN")
  val C214 = MonitorType.withName("TEMP")
  val C215 = MonitorType.withName("HUMID")
  val C216 = MonitorType.withName("PRESS")
  //New Mt


  val psiList = List(A214, A222, A224, A225, A293)
  val windDirList = List(MonitorType.withName("WD_DIR"))

  /*  val monitorReportList = 
    List(A222, A223, A293, A283, A224, A225, A226, A286, A296, A229, A232, A233, A235, A221,
        A213, A214, A215,        
        C211, C212, C214, C215, C216, C213)
  */

  val monitorReportList =
    List(A222, A223, A293, A283, A224, A225, A226, A286, A296, A229, A232, A233, A234, 
      A221, A213, A214, A215,
      C211, C212, C214, C215, C216, C213)

  val calibrationList = List(A222, A223, A293, A283, A224, A225, A226, A286, A296, A229, A221,
    A213, A214, A215,
    A288, A289)

  val epaList =
    List(A214, A215, A222, A223, A224, A225, A226, A283, A286, A293, A296, C211, C212, C213, C214, C215)

  val epaReportList =
    List(C212, C211, A222, A293, A224, A225, A214, A226, A296)

  val epaMap = {
    map.filter(p => p._2.epa_mapping.isDefined).map(kv => (kv._2.epa_mapping.get, kv._1))
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

    val auditLogOpt = ManualAuditLog.getLog(tabType, m, dataTime, mt)
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