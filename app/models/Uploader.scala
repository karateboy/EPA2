package models

import javax.xml.ws.Holder
import com.github.nscala_time.time.Imports._
import java.io.FileOutputStream
import play.api._
import Record._
import models.ModelHelper._

object Uploader {
  val cdxConfig = Play.current.configuration.getConfig("cdx").getOrElse(Configuration.empty)

  val enable = cdxConfig.getBoolean("enable").getOrElse(false)
  val user = cdxConfig.getString("user").getOrElse("anonymouse")
  val password = cdxConfig.getString("password").getOrElse("password")
  val serviceID = "AQX_S_00"
  val siteCounty = cdxConfig.getString("siteCounty").getOrElse("10013")
  val siteID = cdxConfig.getString("siteID").getOrElse("026")

  Logger.info(s"CDX is ${enable}")
  if (enable) {
    Logger.info(s"siteCounty=${siteCounty}")
    Logger.info(s"siteID=${siteID}")
  }

  def init = {
    Logger.info("CDX is ready!")
  }
  case class ItemIdMap(epaId: Int, itemName: String, itemCode: String, unit: String)
  import MonitorType._
  val itemIdMap = Map(
    SO2 -> ItemIdMap(1, "二氧化硫", "SO2", "ppb"),
    NOx -> ItemIdMap(5, "氮氧化物", "	NOx", "ppb"),
    NO -> ItemIdMap(6, "一氧化氮", "NO", "ppb"),
    NO2 -> ItemIdMap(7, "二氧化氮", "NO2", "ppb"),
    CO -> ItemIdMap(2, "一氧化碳", "CO", "ppm"),
    O3 -> ItemIdMap(3, "臭氧", "O3", "ppb"),
    CH4 -> ItemIdMap(25, "甲烷", "CH4", "ppm"),
    NMHC -> ItemIdMap(9, "非甲烷", "NMHC", "ppm"),
    THC -> ItemIdMap(8, "總碳氫", "THC", "ppm"),
    PM10 -> ItemIdMap(4, "懸浮微粒", "PM10", "ug/m3"),
    PM25 -> ItemIdMap(27, "細懸浮微粒", "PM2.5", "ug/m3"),
    WD_SPEED -> ItemIdMap(10, "風速", "WS", "m/2"),
    WD_DIR -> ItemIdMap(11, "風向", "WD", "Deg"),
    TEMP -> ItemIdMap(14, "大氣溫度", "AMB_TEMP", "Deg"),
    HUMID -> ItemIdMap(31, "溼度", "HUM", "%"),
    PRESS -> ItemIdMap(17, "大氣壓力", "Press", "atm"),
    RAIN -> ItemIdMap(23, "雨量", "RF", "mm"),
    RT -> ItemIdMap(16, "室內溫度", "SHELT_TEMP", "deg"))

  def mtRecprdToXML(siteCounty: String, siteID: String, dateTime: DateTime, mtRecord: MtRecord) = {
    val map = itemIdMap(MonitorType.withName(mtRecord.mtName))
    val dateStr = dateTime.toString("YYYY-MM-dd")
    val timeStr = dateTime.toString("HH:mm:ss")
    <aqs:AirQualityData>
      <aqs:SiteIdentifierDetails>
        <aqs:SiteCounty>{ siteCounty }</aqs:SiteCounty>
        <aqs:SiteID>{ siteID }</aqs:SiteID>
      </aqs:SiteIdentifierDetails>
      <aqs:MonitorIdentifierDetails>
        <aqs:Parameter>{ "%03d".format(map.epaId) }</aqs:Parameter>
      </aqs:MonitorIdentifierDetails>
      <aqs:TransactionProtocolDetails>
        <aqs:SamplingDurationCode>1</aqs:SamplingDurationCode>
      </aqs:TransactionProtocolDetails>
      <aqs:SubDailyRawData>
        <aqs:ActionIndicator>I</aqs:ActionIndicator>
        <aqs:SampleCollectionStartDate>{ dateStr }</aqs:SampleCollectionStartDate>
        <aqs:SampleCollectionStartTime>{ timeStr }</aqs:SampleCollectionStartTime>
        {
          val valueElem = <aqs:ReportedSampleValue>{ mtRecord.value.getOrElse("") }</aqs:ReportedSampleValue>
          if(MonitorStatus.isValid(mtRecord.status)){
            valueElem  
          }else{
            if (MonitorStatus.isCalbrating(mtRecord.status)){
              valueElem ++ <aqs:QualifierCode01>D50</aqs:QualifierCode01>
            }else{
              valueElem ++ <aqs:QualifierCode01>D51</aqs:QualifierCode01>
            }
          }          
        }
      </aqs:SubDailyRawData>
    </aqs:AirQualityData>
  }

  def getXml(path: String, hr: HourRecord) = {
    import scala.xml._
    val xmlList = hr.dataList.flatMap { mtRecord =>
      try {
        val mt = MonitorType.withName(mtRecord.mtName)
        if (itemIdMap.contains(mt))
          Some(mtRecprdToXML(siteCounty, siteID, hr.date, mtRecord))
        else
          None
      } catch {
        case x: java.util.NoSuchElementException =>
          None
      }
    }
    val nowStr = DateTime.now().toString("YYYY-MM-dd_hh:mm:ss")

    val xml =
      <aqs:AirQualitySubmission xmlns:aqs="http://taqm.epa.gov.tw/taqm/aqs/schema/" Version="1.0" n1:schemaLocation="http://taqm.epa.gov.tw/taqm/aqs/schema/" xmlns:n1="http://www.w3.org/2001/XMLSchema-instance">
        <aqs:FileGenerationPurposeCode>AQS</aqs:FileGenerationPurposeCode>
        <aqs:FileGenerationDateTime>{ nowStr }</aqs:FileGenerationDateTime>
        { xmlList }
      </aqs:AirQualitySubmission>

    val tempFile = s"${serviceID}_${hr.date.toString("MMdd")}${hr.date.getHourOfDay}_${user}.xml"
    scala.xml.XML.save(path + tempFile, xml, "UTF-8", true)
    //scala.io.Source.fromFile(tempFile)("UTF-8").mkString
    xml.toString
  }

  def upload(hr: HourRecord, localPath: String) = {
    val xmlStr = getXml(localPath, hr)
    if (enable) {
      val fileName = s"${serviceID}_${hr.date.toString("MMdd")}${hr.date.getHourOfDay}_${user}.xml"
      val errMsgHolder = new Holder("")
      val resultHolder = new Holder(Integer.valueOf(0))
      val unknownHolder = new Holder(new java.lang.Boolean(true))
      CdxWebService.service.putFile(user, password, fileName, xmlStr.getBytes("UTF-8"), errMsgHolder, resultHolder, unknownHolder)
      if (resultHolder.value != 1) {
        Logger.error(s"errMsg:${errMsgHolder.value}")
        Logger.error(s"ret:${resultHolder.value.toString}")
        Logger.error(s"unknown:${unknownHolder.value.toString}")
      } else {
        Logger.info(s"Success upload ${hr.date.toString}")
      }
    }
  }
}