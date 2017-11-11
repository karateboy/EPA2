package models
import java.sql.Date
import java.sql.Timestamp
import scalikejdbc._
import play.api._
import com.github.nscala_time.time.Imports._
import models.ModelHelper._
import models._
import Record._
import MonitorType._

/**
 * @author user
 */
class AuditStat(hr: HourRecord) {
  val name = hr.monitor
  val date = hr.date
  var chk = hr.chk
  var map = hr.recordMap
  
  def getStat(mt: MonitorType.Value) = {
    map(mt)._2
  }

  def setStat(mt: MonitorType.Value, stat: String){
    val v_s = map(mt)
    val pair = mt->(v_s._1, Some(stat))
    map = map + pair
  }

  def clear() = {
    chk = None
    for (mt <- MonitorType.mtvAllList) {
      val stat = getStat(mt)
      if (stat.isDefined && MonitorStatus.getTagInfo(stat.get).statusType == StatusType.Auto) {
        val internalStatus = MonitorStatus.switchTagToInternal(stat.get)
        setStat(mt, internalStatus)
      }
    }
    updateDB
  }

  def setAuditStat(mt: MonitorType.Value, lead: Char) {
    val old_stat = getStat(mt)
    if (old_stat.isEmpty)
      return

    val tagInfo = MonitorStatus.getTagInfo(old_stat.get)

    if (tagInfo.statusType == StatusType.Internal) {
      val autoAsNormal = SystemConfig.getConfig(SystemConfig.AutoAuditAsNormal, "True").toBoolean
      val l =
        if (autoAsNormal)
          lead.toLower
        else
          lead.toUpper

      setStat(mt, l + tagInfo.id)

    }
  }

  def updateDB()(implicit session: DBSession = AutoSession) = {
    import play.api.libs.json._
    val tab_name = Record.getTabName(TableType.Hour)
    val dataList = map map {
      x =>
        MtRecord(x._1.toString(), x._2._1.get, x._2._2.get)
    }
    implicit val write = Json.writes[MtRecord]
    val dataListStr = Json.toJson(dataList).toString()
    sql"""
    Update ${tab_name}
     Set  [CHK] = ${chk}, 
       [dataList] = ${dataListStr}
        Where DP_NO=${name} and M_DateTime = ${date}        
      """.update.apply
  }
}

object Auditor {
  def clearAuditData(records: List[HourRecord]) = {
    val auditStatList = records.map { new AuditStat(_) }
    auditStatList.foreach { _.clear }
  }

  def isOk(r: (Option[Float], Option[String])) = {
    r._1.isDefined && r._2.isDefined &&
      (MonitorStatus.isNormalStat(r._2.get) || MonitorStatus.getTagInfo(r._2.get).statusType == StatusType.Auto)
  }
      
  def auditHourData(monitor: Monitor.Value, auditConfig: AutoAudit, start: DateTime, end: DateTime, reaudit:Boolean = false)(implicit session: DBSession = AutoSession) = {
    val records =
      if(reaudit)
        getHourRecords(monitor, start, end).toArray
      else
        getUncheckedHourRecords(monitor, start, end).toArray
      

    for {
      hr <- records.zipWithIndex
      record = hr._1
      idx = hr._2
      targetStat = {
        if(reaudit){
          val as = new AuditStat(record)
          as.clear()
          as
        }else
          new AuditStat(record)}
    } {
      var invalid = false
      if(auditConfig.minMaxRule.checkInvalid(record, targetStat))
        invalid = true

      if(auditConfig.compareRule.checkInvalid(record, targetStat))
        invalid = true

      if(auditConfig.differenceRule.checkInvalid(record, targetStat, monitor, record.date))
        invalid = true

      if(auditConfig.persistenceRule.checkInvalid(record, targetStat, monitor, record.date))
        invalid = true
      

      if(auditConfig.spikeRule.checkInvalid(record, targetStat, monitor, record.date))
        invalid = true

      if(auditConfig.twoHourRule.checkInvalid(record, targetStat, monitor, record.date))
        invalid = true
        
      if(auditConfig.threeHourRule.checkInvalid(record, targetStat, monitor, record.date))
        invalid = true
        
      if(auditConfig.fourHourRule.checkInvalid(record, targetStat, monitor, record.date))
        invalid = true
        
      if(auditConfig.monoRule.checkInvalid(record, targetStat, monitor, record.date))
        invalid = true
        
      //Save
      if (invalid)
        targetStat.chk = Some("BAD")
      else
        targetStat.chk = Some("OK")

      targetStat.updateDB
    }
  }

}