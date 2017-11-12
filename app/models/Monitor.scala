package models

import scala.collection.Map
import java.sql.Date
import play.api.Logger
import scalikejdbc._
import scalikejdbc.config._
import EnumUtils._
import play.api.libs.json._
import ModelHelper._

case class InstrumentStatusType(key: String, addr: Int, desc: String, unit: String)

case class InstrumentStatusTypeMap(instrumentId: String, statusTypeSeq: Seq[InstrumentStatusType])

case class Monitor(id: String, name: String, lat: Double, lng: Double, url: String, autoAudit: AutoAudit,
                   monitorTypes: Seq[MonitorType.Value], monitorTypeStds: Seq[MonitorTypeStandard],
                   instrumentStatusTypeMapOpt: Option[Seq[InstrumentStatusTypeMap]]) {
  private val stdMap = Map(monitorTypeStds.map { r => r.id -> r.std_internal }: _*)

  def getStdInternal(mt: MonitorType.Value) = {
    val monitorStd = stdMap.get(mt)
    if (monitorStd.isDefined)
      monitorStd
    else
      MonitorType.map(mt).std_internal_default
  }

  def getNewStd(mt: MonitorType.Value, v: Float) = {
    if (stdMap.get(mt).isEmpty)
      MonitorTypeStandard(mt, v) :: monitorTypeStds.toList
    else {
      monitorTypeStds.map { s =>
        if (s.id != mt)
          s
        else
          MonitorTypeStandard(s.id, v)
      }
    }
  }

  def updateInstrumentStatusTypeMap(newMapOpt: Option[List[InstrumentStatusTypeMap]]) =
    Monitor(id, name, lat, lng, url, autoAudit,
      monitorTypes, monitorTypeStds, newMapOpt)

}

case class MonitorTypeStandard(id: MonitorType.Value, std_internal: Float)

object Monitor extends Enumeration {
  implicit val mReads: Reads[Monitor.Value] = EnumUtils.enumReads(Monitor)
  implicit val mWrites: Writes[Monitor.Value] = EnumUtils.enumWrites
  implicit val mtStdRead = Json.reads[MonitorTypeStandard]
  implicit val mtStdWrite = Json.writes[MonitorTypeStandard]
  implicit val istRead = Json.reads[InstrumentStatusType]
  implicit val istWrite = Json.writes[InstrumentStatusType]
  implicit val istMapRead = Json.reads[InstrumentStatusTypeMap]
  implicit val istMapWrite = Json.writes[InstrumentStatusTypeMap]

  lazy val monitorList: List[Monitor] =
    DB readOnly { implicit session =>
      sql"""
        Select * 
        From Monitor
        """.map { r =>
        val autoAuditJson = r.stringOpt("AutoAudit").getOrElse(Json.toJson(AutoAudit.default).toString())
        val autoAudit = Json.parse(autoAuditJson).validate[AutoAudit].get
        val monitorTypesJson = r.stringOpt("monitorTypes").getOrElse(Json.toJson(Seq.empty[MonitorType.Value]).toString())
        val monitorTypes = Json.parse(monitorTypesJson).validate[Seq[MonitorType.Value]].get
        val monitorTypeStdJson = r.stringOpt("monitorTypeStd").getOrElse(Json.toJson(Seq.empty[MonitorTypeStandard]).toString())
        val monitorTypeStd = Json.parse(monitorTypeStdJson).validate[Seq[MonitorTypeStandard]].get
        val instrumentStatusTypeMap = r.stringOpt("instrumentStatusTypeMap").flatMap {
          json =>
            val ret = Json.parse(json).validate[List[InstrumentStatusTypeMap]].asOpt
            if (ret.isEmpty)
              Logger.error("Invalid instrumentStatusTypeMap")
            ret
        }
        Monitor(r.string(1), r.string("monitorName"), r.string("monitorY").toDouble, r.string("monitorX").toDouble, r.string("imageUrl"),
          autoAudit, monitorTypes, monitorTypeStd, instrumentStatusTypeMap)
      }.list.apply
    }

  var map: Map[Value, Monitor] = Map(monitorList.map { e => Value(e.id) -> e }: _*)

  lazy val mvList = monitorList.map { m => Monitor.withName(m.id) }

  def myMvList(p: Privilege) = {
    mvList.filter {
      p.allowedMonitors.contains
    }
  }

  def getInstrumentList(monitor: Monitor.Value) = {
    val statusTypeMapOpt = map(monitor).instrumentStatusTypeMapOpt
    val mapOpt = statusTypeMapOpt.map { mapList => mapList.map { map => map.instrumentId -> map.statusTypeSeq } }.map(_.toMap)
    if (mapOpt.isDefined) {
      mapOpt.get.keys.toList.sorted
    } else
      List.empty[String]
  }

  def getDisplayName(m: Monitor.Value) = {
    map(m).name
  }

  def updateMonitorTypes(m: Monitor.Value, mt: Seq[MonitorType.Value]) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, oldM.url, oldM.autoAudit, mt,
      oldM.monitorTypeStds, oldM.instrumentStatusTypeMapOpt)
    updateMonitorAuditMt(newM)
    map = map + (m -> newM)
  }

  def updateMonitorAutoAudit(m: Monitor.Value, autoAudit: AutoAudit) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, oldM.url, autoAudit, oldM.monitorTypes,
      oldM.monitorTypeStds, oldM.instrumentStatusTypeMapOpt)
    updateMonitorAuditMt(newM)
    map = map + (m -> newM)
  }

  def updateMonitorAuditMt(m: Monitor)(implicit session: DBSession = AutoSession) = {
    sql"""
        Update Monitor
        Set autoAudit=${Json.toJson(m.autoAudit).toString}, monitorTypes=${Json.toJson(m.monitorTypes).toString}
        Where DP_NO=${m.id}  
        """.update.apply

    val newMap = map + (Monitor.withName(m.id) -> m)
    map = newMap
  }

  def updateStdInternal(m: Monitor)(implicit session: DBSession = AutoSession) = {
    sql"""
        Update Monitor
        Set monitorTypeStd=${Json.toJson(m.monitorTypeStds).toString}
        Where DP_NO=${m.id}  
        """.update.apply

    val newMap = map + (Monitor.withName(m.id) -> m)
    map = newMap
  }

  def updateInstrumentStatusTypeMap(m: Monitor)(implicit session: DBSession = AutoSession) = {
    val jsonOpt = m.instrumentStatusTypeMapOpt.map { istMap =>
      Json.toJson(istMap).toString
    }
    sql"""
        Update Monitor
        Set instrumentStatusTypeMap=$jsonOpt
        Where DP_NO=${m.id}  
        """.update.apply

    val newMap = map + (Monitor.withName(m.id) -> m)
    map = newMap
  }

  def updateImgUrl(m: Monitor.Value, url: String)(implicit session: DBSession = AutoSession) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, oldM.lat, oldM.lng, url, oldM.autoAudit, oldM.monitorTypes,
      oldM.monitorTypeStds, oldM.instrumentStatusTypeMapOpt)
    sql"""
        Update Monitor
        Set imageUrl=${url}
        Where DP_NO=${oldM.id}  
        """.update.apply
    val newMap = map + (m -> newM)
    map = newMap
  }

  def updateLocation(m: Monitor.Value, lat: Double, lng: Double)(implicit session: DBSession = AutoSession) = {
    val oldM = map(m)
    val newM = Monitor(oldM.id, oldM.name, lat, lng, oldM.url, oldM.autoAudit, oldM.monitorTypes,
      oldM.monitorTypeStds, oldM.instrumentStatusTypeMapOpt)
    sql"""
        Update Monitor
        Set monitorY=${lat}, monitorX=${lng}
        Where DP_NO=${oldM.id}  
        """.update.apply
    val newMap = map + (m -> newM)
    map = newMap
  }

  def getCenterLat = {
    val monitorLatList = mvList.map {
      map(_).lat
    }
    monitorLatList.sum / monitorLatList.length
  }

  def getCenterLng = {
    val monitorLngList = mvList.map {
      map(_).lng
    }
    monitorLngList.sum / monitorLngList.length
  }

}
