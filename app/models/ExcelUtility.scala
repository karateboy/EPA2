package models
import play.api._
import play.api.Play.current
import org.apache.poi.openxml4j.opc._
import org.apache.poi.xssf.usermodel._

import models.Record._
import models.ModelHelper._
import com.github.nscala_time.time.Imports._
import java.io._
import java.nio.file.Files
import java.nio.file._
import org.apache.poi.ss.usermodel._
import javax.inject._
import play.api.i18n._
import controllers.HighchartData
import controllers.IntervalReport
import controllers.MonthHourReport
import controllers.MonthlyReport
import controllers.Report

object ExcelUtility {
  val docRoot = "/report_template/"

  private def prepareTemplate(templateFile: String) = {
    val templatePath = Paths.get(current.path.getAbsolutePath + docRoot + templateFile)
    val reportFilePath = Files.createTempFile("temp", ".xlsx");

    Files.copy(templatePath, reportFilePath, StandardCopyOption.REPLACE_EXISTING)

    //Open Excel
    val pkg = OPCPackage.open(new FileInputStream(reportFilePath.toAbsolutePath().toString()))
    val wb = new XSSFWorkbook(pkg);

    (reportFilePath, pkg, wb)
  }

  def finishExcel(reportFilePath: Path, pkg: OPCPackage, wb: XSSFWorkbook) = {
    val out = new FileOutputStream(reportFilePath.toAbsolutePath().toString());
    wb.write(out);
    out.close();
    pkg.close();

    new File(reportFilePath.toAbsolutePath().toString())
  }

  def createStyle(mt: MonitorType.Value)(implicit wb: XSSFWorkbook) = {
    val prec = MonitorType.map(mt).prec
    val format_str = "0." + "0" * prec
    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)
    style.setDataFormat(format.getFormat(format_str))
    style.setBorderBottom(BorderStyle.THIN)
    style.setBottomBorderColor(IndexedColors.BLACK.getIndex())
    style.setBorderLeft(BorderStyle.THIN)
    style.setLeftBorderColor(IndexedColors.BLACK.getIndex())
    style.setBorderRight(BorderStyle.THIN)
    style.setRightBorderColor(IndexedColors.BLACK.getIndex())
    style.setBorderTop(BorderStyle.THIN)
    style.setTopBorderColor(IndexedColors.BLACK.getIndex())
    style
  }

  def createColorStyle(fgColors: Array[XSSFColor], mt: MonitorType.Value)(implicit wb: XSSFWorkbook) = {
    fgColors.map {
      color =>
        val style = createStyle(mt)
        style.setFillForegroundColor(color)
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
        style
    }
  }

  def getStyle(tag: String, normalStyle: XSSFCellStyle, abnormalStyles: Array[XSSFCellStyle]) = {
    import MonitorStatus._
    val info = MonitorStatus.getTagInfo(tag)
    info.statusType match {
      case StatusType.Internal =>
        {
          if (isNormalStat(tag))
            normalStyle
          else if (isCalbration(tag))
            abnormalStyles(0)
          else if (isRepairing(tag))
            abnormalStyles(1)
          else if (isMaintance(tag))
            abnormalStyles(2)
          else
            abnormalStyles(3)
        }
      case StatusType.Auto =>
        if (SystemConfig.getConfig(SystemConfig.AutoAuditAsNormal, "True").toBoolean)
          normalStyle
        else
          abnormalStyles(3)
      case StatusType.Manual =>
        abnormalStyles(4)
    }
  }

  def createAllDailyReport(monitor: Monitor.Value, reportDate: DateTime) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("all_daily_report.xlsx")
    val format = wb.createDataFormat();
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)

    val fgColors =
      {
        val seqColors =
          for (col <- 3 to 7)
            yield titleRow.getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    def fillMonitorDailyReport(monitor: Monitor.Value, data: DailyReport, sheetIdx: Int) = {
      val sheet = wb.getSheetAt(sheetIdx)

      sheet.getRow(1).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(0).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      sheet.getRow(2).getCell(0).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

      val mtDataPair = data.typeList map { mtr => mtr.monitorType -> mtr }
      val mtDataMap = mtDataPair.toMap
      for {
        (mt, idx) <- MonitorType.monitorReportList.zipWithIndex
        col = idx + 1
      } {
        if (!mtDataMap.contains(mt)) {
          sheet.setColumnHidden(col, true)
        } else {
          val mtRecord = mtDataMap(mt)
          val normalStyle = createStyle(mt)
          val abnormalStyles = createColorStyle(fgColors, mt)
          for {
            row <- 4 to 27
            cell = sheet.getRow(row).getCell(col)
            cellData = mtRecord.dataList(row - 4)
          } {
            val (date, valueOpt, statusOpt) = cellData

            if (valueOpt.isDefined && statusOpt.isDefined) {
              cell.setCellValue(valueOpt.get)
              val cellStyle = getStyle(statusOpt.get, normalStyle, abnormalStyles)
              cell.setCellStyle(cellStyle)
            } else
              cell.setCellValue("-")
          }
        }

      }

      for {
        col <- 1 to data.typeList.length
      } {
        val stat = data.typeList(col - 1).stat
        stat.avg.fold(sheet.getRow(28).getCell(col).setCellValue("-"))(avg => sheet.getRow(28).getCell(col).setCellValue(avg))
        stat.max.fold(sheet.getRow(29).getCell(col).setCellValue("-"))(max => sheet.getRow(29).getCell(col).setCellValue(max))
        stat.min.fold(sheet.getRow(30).getCell(col).setCellValue("-"))(min => sheet.getRow(30).getCell(col).setCellValue(min))
        stat.effectPercent.fold(sheet.getRow(31).getCell(col).setCellValue("-"))(ef => sheet.getRow(31).getCell(col).setCellValue(ef))
      }

      //Hide col not in use
      for {
        col <- 1 to data.typeList.length
      } {
        val mt = data.typeList(col - 1).monitorType
        if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
          sheet.setColumnHidden(col, true)
        }
      }

    }

    val dailyReport = Record.getDailyReport(monitor, reportDate)

    fillMonitorDailyReport(monitor, dailyReport, 0)

    wb.setActiveSheet(0)
    finishExcel(reportFilePath, pkg, wb)
  }

  def createDailyReport(monitor: Monitor.Value, reportDate: DateTime, data: DailyReport) = {

    implicit val (reportFilePath, pkg, wb) = prepareTemplate("daily_report.xlsx")
    val format = wb.createDataFormat();
    val sheet = wb.getSheetAt(0)
    val titleRow = sheet.getRow(2)
    val titleCell = titleRow.getCell(0)

    val fgColors =
      {
        val seqColors =
          for (col <- 3 to 7)
            yield titleRow.getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    titleCell.setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(19).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    titleRow.getCell(19).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

    for {
      col <- 1 to data.typeList.length
      mtRecord = data.typeList(col - 1)
      normalStyle = createStyle(mtRecord.monitorType)
      abnormalStyles = createColorStyle(fgColors, mtRecord.monitorType)
      row <- 4 to 27
      cell = sheet.getRow(row).getCell(col)
      cellData = mtRecord.dataList(row - 4)
    } {
      val (date, valueOpt, statusOpt) = cellData
      if (valueOpt.isEmpty || statusOpt.isEmpty) {
        cell.setCellValue("-")
      } else {
        val value = valueOpt.get
        val status = statusOpt.get
        cell.setCellValue(value)

        val cellStyle = getStyle(status, normalStyle, abnormalStyles)
        cell.setCellStyle(cellStyle)
      }
    }

    for {
      col <- 1 to data.typeList.length
    } {
      val stat = data.typeList(col - 1).stat
      def fillOpt(vOpt: Option[Float], row: Int) {
        vOpt.fold(sheet.getRow(row).getCell(col).setCellValue("-"))(v => sheet.getRow(row).getCell(col).setCellValue(v))
      }

      fillOpt(stat.avg, 28)
      fillOpt(stat.max, 29)
      fillOpt(stat.min, 30)
      fillOpt(stat.effectPercent.map { _ * 100 }, 31)
    }

    //Hide col not in use
    for {
      col <- 1 to data.typeList.length
    } {
      val mt = data.typeList(col - 1).monitorType
      if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
        sheet.setColumnHidden(col, true)
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def createMonthlyReport(monitor: Monitor.Value, reportDate: DateTime, data: MonthlyReport, nDay: Int)(implicit messages: Messages) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("monthly_report.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val mtDataPair = data.typeArray map { mtr => mtr.monitorType -> mtr }
    val mtDataMap = mtDataPair.toMap

    val fgColors =
      {
        val seqColors =
          for (col <- 17 to 21)
            yield wb.getSheetAt(2).getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    def fillEffectSheet(sheet: XSSFSheet) = {
      sheet.getRow(0).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(0).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      sheet.getRow(2).getCell(0).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

      import org.apache.poi.hssf.util.HSSFColor
      def createInvalidStyle(mt: MonitorType.Value) = {
        val style = createStyle(mt)
        style.setFillForegroundColor(HSSFColor.HSSFColorPredefined.RED.getIndex)
        style.setFillPattern(FillPatternType.SOLID_FOREGROUND)
        style
      }

      for {
        (mt, idx) <- MonitorType.monitorReportList.zipWithIndex
        col = idx + 1
      } {
        
        if (mtDataMap.contains(mt) && Monitor.map(monitor).monitorTypes.contains(mt)) {
          val mtRecord = mtDataMap(mt)
          for {
            row <- 5 to (5 + mtRecord.dataList.length - 1)
            cell = sheet.getRow(row).getCell(col)
            cellData = mtRecord.dataList(row - 5)
            invalidStyle = createInvalidStyle(mt)
          } {
            cell.setCellValue(cellData.count)
            if (cellData.count < 16)
              cell.setCellStyle(invalidStyle)
          }

          val sum = mtRecord.dataList.map(_.count).sum
          sheet.getRow(36).getCell(col).setCellValue(sum)
          sheet.getRow(37).getCell(col).setCellValue(nDay * 24)
          evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(col))
        } else {
          sheet.setColumnHidden(col, true)
        }
      }
    }

    // Fill Graph title
    val graph_list = List(MonitorType.WD_SPEED, MonitorType.SO2, MonitorType.CO, MonitorType.NO2,
      MonitorType.O3, MonitorType.PM10, MonitorType.NMHC, MonitorType.TSP)

    def fillMonthlySheet(sheet: XSSFSheet) = {
      sheet.getRow(0).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
      sheet.getRow(1).getCell(22).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      sheet.getRow(2).getCell(22).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

      val abnormalColor =
        {
          val seqColors =
            for (col <- 3 to 3)
              yield sheet.getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor
          seqColors.toArray
        }

      for {
        (mt, idx) <- MonitorType.monitorReportList.zipWithIndex
        col = idx + 1
      } {
        if (mtDataMap.contains(mt) && Monitor.map(monitor).monitorTypes.contains(mt)) {
          val mtRecord = mtDataMap(mt)
          val normalStyle = createStyle(mtRecord.monitorType)
          val abnormalStyles = createColorStyle(abnormalColor, mt)
          for {
            row <- 5 to (5 + mtRecord.dataList.length - 1)
            cell = sheet.getRow(row).getCell(col)
            cellData = mtRecord.dataList(row - 5)
          } {
            if (cellData.avg.isDefined)
              cell.setCellValue(cellData.avg.get)
            else
              cell.setCellValue("-")

            if (cellData.count >= 16)
              cell.setCellStyle(normalStyle)
            else
              cell.setCellStyle(abnormalStyles(0))
          }

          val stat = mtRecord.stat

          stat.avg.fold(sheet.getRow(36).getCell(col).setCellValue("-"))(avg => sheet.getRow(36).getCell(col).setCellValue(avg))
          stat.max.fold(sheet.getRow(37).getCell(col).setCellValue("-"))(max => sheet.getRow(37).getCell(col).setCellValue(max))
          stat.min.fold(sheet.getRow(38).getCell(col).setCellValue("-"))(min => sheet.getRow(38).getCell(col).setCellValue(min))
          evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(col))
        } else {
          sheet.setColumnHidden(col, true)
        }
      }

      for {
        mt <- graph_list.zipWithIndex
        row = sheet.getRow(46)
      } {
        val mtCase = MonitorType.map(mt._1)
        val title =
          if (!Monitor.map(monitor).monitorTypes.contains(mt._1))
            s"${Monitor.map(monitor).name}無${mtCase.desp}測項"
          else if (mtCase.std_law.isDefined)
            s"${Monitor.map(monitor).name}${mtCase.desp}小時趨勢圖 (法規:${mtCase.std_law.get}${mtCase.unit})"
          else
            s"${Monitor.map(monitor).name}${mtCase.desp}小時趨勢圖 "

        row.getCell(mt._2).setCellValue(title)
      }
    }

    def fillMonthlyHourSheet(report: MonthHourReport) = {

      for {
        (mt, mt_idx) <- MonitorType.monitorReportList.zipWithIndex
        sheetIndex = 2 + mt_idx
        sheet = wb.getSheetAt(sheetIndex)
      } {
        if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
          wb.setSheetHidden(sheetIndex, true)
        } else {
          sheet.getRow(1).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
          sheet.getRow(1).getCell(25).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
          sheet.getRow(2).getCell(25).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))
          sheet.getRow(43).getCell(0).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

          val normalStyle = createStyle(mt)
          val abnormalStyles = createColorStyle(fgColors, mt)
          for {
            day <- 0 to nDay - 1
            row = day + 4
            dayRecord = report.dailyReports(day)
          } {
            //Stat
            val stat = dayRecord.typeList(mt_idx).stat
            if (stat.avg.isDefined) {
              sheet.getRow(row).getCell(25).setCellValue(stat.avg.get)
              sheet.getRow(row).getCell(29).setCellValue(stat.avg.get * stat.count)
            } else {
              sheet.getRow(row).getCell(25).setCellValue("-")
              sheet.getRow(row).getCell(29).setCellValue("-")
            }
            sheet.getRow(row).getCell(26).setCellValue(stat.count)
            if (stat.max.isDefined)
              sheet.getRow(row).getCell(27).setCellValue(stat.max.get)
            else
              sheet.getRow(row).getCell(27).setCellValue("-")

            if (stat.min.isDefined)
              sheet.getRow(row).getCell(28).setCellValue(stat.min.get)
            else
              sheet.getRow(row).getCell(28).setCellValue("-")

            val dayMap = dayRecord.typeList.groupBy { _.monitorType }
            for {
              col <- 1 to 24
              cell = sheet.getRow(row).getCell(col)
              cellData = dayMap(mt).head.dataList(col - 1)
            } {
              val (date, valueOpt, statusOpt) = cellData
              if (valueOpt.isEmpty || statusOpt.isEmpty) {
                cell.setCellValue("-")
              } else {
                cell.setCellValue(valueOpt.get)
                val style = getStyle(statusOpt.get, normalStyle, abnormalStyles)
                cell.setCellStyle(style)
              }

            } // col loop
          } // row loop
          for {
            col <- 1 to 24
          } {
            val stat = report.hourStatArray(col - 1)
            if (stat.count >= 20) {
              evaluator.evaluateFormulaCell(sheet.getRow(35).getCell(col))
              sheet.getRow(36).getCell(col).setCellValue(stat.count)
              evaluator.evaluateFormulaCell(sheet.getRow(37).getCell(col))
              evaluator.evaluateFormulaCell(sheet.getRow(38).getCell(col))
              evaluator.evaluateFormulaCell(sheet.getRow(39).getCell(col))
            } else {
              sheet.getRow(35).getCell(col).setCellValue("-")
              sheet.getRow(36).getCell(col).setCellValue(stat.count)
              sheet.getRow(37).getCell(col).setCellValue("-")
              sheet.getRow(38).getCell(col).setCellValue("-")
              sheet.getRow(39).getCell(col).setCellValue("-")
            }
          }
        } //Sheet is ok        
      }
      wb.setForceFormulaRecalculation(true)
    }

    def fillGraphHourSheet(report: MonthHourReport, idx: Int) = {
      val sheet = wb.getSheetAt(idx)
      val graph_idx = graph_list.zipWithIndex
      var row_start = 1
      for {
        dayReport <- report.dailyReports
      } {
        val mtDayReportPair = dayReport.typeList map { tl => tl.monitorType -> tl}
        val mtDayReportMap = mtDayReportPair.toMap 
        for {
          (mt, mt_i) <- MonitorType.monitorReportList.zipWithIndex if(mtDayReportMap.contains(mt))
          mtDayReport = mtDayReportMap(mt)
          (data, idx) <- mtDayReport.dataList.zipWithIndex
        } {
          if (data._2.isDefined && data._3.isDefined && MonitorStatus.isValid(data._3.get)) {
            if (sheet.getRow(row_start + idx) == null)
              sheet.createRow(row_start + idx).createCell(mt_i + 1).setCellValue(data._2.get)
            else
              sheet.getRow(row_start + idx).createCell(mt_i + 1).setCellValue(data._2.get)
          }

          if (mt_i == 0) {
            if (sheet.getRow(row_start + idx) == null)
              sheet.createRow(row_start + idx).createCell(0).setCellValue(data._1.toDateTime().toString("YYYY-MM-dd HH:mm"))
            else
              sheet.getRow(row_start + idx).createCell(0).setCellValue(data._1.toDateTime().toString("YYYY-MM-dd HH:mm"))
          }

          val idxOpt = graph_idx.find(p => p._1 == dayReport.typeList(mt_i).monitorType)
          if (idxOpt.isDefined) {
            val graph_idx = idxOpt.get
            val std_internal = Monitor.map(monitor).getStdInternal(graph_idx._1)
            if (std_internal.isDefined) {
              sheet.getRow(row_start + idx).createCell(22 + graph_idx._2).setCellValue(std_internal.get)
            }
          }
        }
        row_start += dayReport.typeList(0).dataList.length
      }
    }

    // 有效率月報
    fillEffectSheet(wb.getSheetAt(0))
    fillMonthlySheet(wb.getSheetAt(1))
    val monthlyHourReport = Report.monthlyHourReportHelper(monitor, reportDate)
    fillMonthlyHourSheet(monthlyHourReport)
    fillGraphHourSheet(monthlyHourReport, 2 + MonitorType.monitorReportList.length)

    wb.setActiveSheet(0)
    finishExcel(reportFilePath, pkg, wb)
  }

  def createYearlyReport(monitor: Monitor.Value, reportDate: DateTime, report: IntervalReport) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("yearly_report.xlsx")
    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(30).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(30).setCellValue("資料日期:" + reportDate.toString("YYYY年"))

    val abnormalColor =
      {
        val seqColors =
          for (col <- 3 to 3)
            yield sheet.getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    for {
      col <- 1 to report.typeArray.length
      mt = report.typeArray(col - 1).monitorType
      normalStyle = createStyle(mt)
      abnormalStyles = createColorStyle(abnormalColor, mt)
    } {
      if (!Monitor.map(monitor).monitorTypes.contains(mt)) {
        sheet.setColumnHidden(col, true)
      }

      for {
        row <- 4 to 4 + 12 - 1
        data = report.typeArray(col - 1).dataList(row - 4)
      } {
        val cell = sheet.getRow(row).getCell(col)
        data.avg.fold(cell.setCellValue("_"))(avg => cell.setCellValue(avg))
        if (data.count >= 20)
          cell.setCellStyle(normalStyle)
        else
          cell.setCellStyle(abnormalStyles(0))
      }
      val stat = report.typeArray(col - 1).stat
      def fillOpt(vOpt: Option[Float], idx: Int) {
        vOpt.fold({
          sheet.getRow(idx).getCell(col).setCellValue("-")
          sheet.getRow(idx).getCell(col).setCellStyle(abnormalStyles(0))
        })({ v =>
          sheet.getRow(idx).getCell(col).setCellStyle(normalStyle)
          sheet.getRow(idx).getCell(col).setCellValue(v)
        })
      }

      fillOpt(stat.avg, 16)
      fillOpt(stat.max, 17)
      fillOpt(stat.min, 18)
      fillOpt(stat.effectPercent, 19)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def createSingleSiteEffectiveReport(monitor: Monitor.Value, reportDate: DateTime, rateList: List[MonitorEffectiveRate], statMap: Map[MonitorType.Value, Stat]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("effective_single.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("監測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(16).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(16).setCellValue("資料日期:" + reportDate.toString("YYYY年"))

    for {
      row <- 4 to 4 + 12 - 1
      mt <- MonitorType.monitorReportList.zipWithIndex
      data = rateList(row - 4)
      cell = sheet.getRow(row).getCell(mt._2 + 1)
    } {
      cell.setCellValue(data.rateMap(mt._1) * 100)
    }

    for {
      mt <- MonitorType.monitorReportList.zipWithIndex
      stat = statMap(mt._1)
    } {
      sheet.getRow(16).getCell(mt._2 + 1).setCellValue(stat.min.get * 100)
      sheet.getRow(17).getCell(mt._2 + 1).setCellValue(stat.max.get * 100)
      sheet.getRow(18).getCell(mt._2 + 1).setCellValue(stat.avg.get * 100)
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def createMultipleSiteEffectiveReport(monitorType: MonitorType.Value, reportDate: DateTime, rateList: List[MonitorTypeEffectiveRate], statMap: Map[Monitor.Value, Stat])(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("effective_mulitiple.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測項名稱:" + MonitorType.map(monitorType).desp)
    sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY年"))

    for {
      m <- Monitor.mvList.zipWithIndex
      cell = sheet.getRow(3).getCell(m._2 + 1)
    } {
      cell.setCellValue(Monitor.map(m._1).name)
    }

    for {
      row <- 4 to 4 + 12 - 1
      m <- Monitor.mvList.zipWithIndex
      data = rateList(row - 4).rateMap(m._1)
      cell = sheet.getRow(row).getCell(m._2 + 1)
    } {
      cell.setCellValue(data * 100)
    }

    for {
      m <- Monitor.mvList.zipWithIndex
      stat = statMap(m._1)
    } {
      def fillOpt(vOpt: Option[Float], idx: Int) {
        vOpt.fold(sheet.getRow(idx).getCell(m._2 + 1).setCellValue("-"))(v => sheet.getRow(idx).getCell(m._2 + 1).setCellValue(v * 100))
      }

      fillOpt(stat.min, 16)
      fillOpt(stat.max, 17)
      fillOpt(stat.avg, 18)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

    def aqiDailyReport(monitor: Monitor.Value, reportDate: DateTime,
                     aqiHourRecords: Seq[(Option[Float], Map[AQI.Value, (Option[Float], Option[Float])])])(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("aqi_daily.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(15).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(15).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

    import org.apache.poi.ss.usermodel._

    val greenStyle = sheet.getRow(31).getCell(0).getCellStyle
    val yellowStyle = sheet.getRow(31).getCell(1).getCellStyle
    val orangeStyle = sheet.getRow(31).getCell(2).getCellStyle
    val redStyle = sheet.getRow(31).getCell(3).getCellStyle
    val violetStyle = sheet.getRow(31).getCell(4).getCellStyle
    val brownStyle = sheet.getRow(31).getCell(5).getCellStyle

    for {
      row <- 5 to 5 + 24 - 1
      data = aqiHourRecords(row - 5)
    } {
      if (data._1.isDefined) {
        val cell = sheet.getRow(row).getCell(1)
        val v = data._1.get
        cell.setCellValue(v)
        if (v < 50)
          cell.setCellStyle(greenStyle)
        else if (v <= 100)
          cell.setCellStyle(yellowStyle)
        else if (v < 150)
          cell.setCellStyle(orangeStyle)
        else if (v < 200)
          cell.setCellStyle(redStyle)
        else if (v < 300)
          cell.setCellStyle(violetStyle)
        else
          cell.setCellStyle(brownStyle)
      } else
        sheet.getRow(row).getCell(1).setCellValue("-")

      for {
        aqi_type_idx <- AQI.realtimeList.zipWithIndex
        idx = aqi_type_idx._2
        aqi = data._2(aqi_type_idx._1)
      } {
        if (aqi._1.isDefined)
          sheet.getRow(row).getCell(2 + idx * 2 + 1).setCellValue(aqi._1.get)
        else
          sheet.getRow(row).getCell(2 + idx * 2 + 1).setCellValue("-")

        if (aqi._2.isDefined)
          sheet.getRow(row).getCell(2 + idx * 2).setCellValue(aqi._2.get)
        else
          sheet.getRow(row).getCell(2 + idx * 2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def aqiMonthlyReport(monitor: Monitor.Value, reportDate: DateTime, aqiDailyList: List[AqiReport], nDays: Int)(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("aqi_monthly.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(13).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(13).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))

    val greenStyle = sheet.getRow(37).getCell(0).getCellStyle
    val yellowStyle = sheet.getRow(37).getCell(1).getCellStyle
    val orangeStyle = sheet.getRow(37).getCell(2).getCellStyle
    val redStyle = sheet.getRow(37).getCell(3).getCellStyle
    val violetStyle = sheet.getRow(37).getCell(4).getCellStyle
    val brownStyle = sheet.getRow(37).getCell(5).getCellStyle

    for {
      row <- 5 to 5 + nDays - 1
      data = aqiDailyList(row - 5)
    } {
      if (data.aqi.isDefined) {
        val cell = sheet.getRow(row).getCell(1)
        val v = data.aqi.get
        cell.setCellValue(v)
        if (v < 50)
          cell.setCellStyle(greenStyle)
        else if (v <= 100)
          cell.setCellStyle(yellowStyle)
        else if (v < 150)
          cell.setCellStyle(orangeStyle)
        else if (v < 200)
          cell.setCellStyle(redStyle)
        else if (v < 300)
          cell.setCellStyle(violetStyle)
        else
          cell.setCellStyle(brownStyle)

      } else
        sheet.getRow(row).getCell(1).setCellValue("-")

      for {
        aqi_type_idx <- AQI.dailyList.zipWithIndex
        idx = aqi_type_idx._2
        aqi = data.sub_map(aqi_type_idx._1)
      } {
        if (aqi._1.isDefined)
          sheet.getRow(row).getCell(2 + idx * 2 + 1).setCellValue(aqi._1.get)
        else
          sheet.getRow(row).getCell(2 + idx * 2 + 1).setCellValue("-")

        if (aqi._2.isDefined)
          sheet.getRow(row).getCell(2 + idx * 2).setCellValue(aqi._2.get)
        else
          sheet.getRow(row).getCell(2 + idx * 2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }


  def psiDailyReport(monitor: Monitor.Value, reportDate: DateTime, psiHourRecords: List[(Option[Float], Map[MonitorType.Value, (Option[Float], Option[Float])])])(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("psi_daily.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(10).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(10).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月dd日"))

    for {
      mtv <- MonitorType.psiList.zipWithIndex
    } {
      sheet.getRow(3).getCell(2 + mtv._2 * 2).setCellValue(MonitorType.map(mtv._1).desp)
    }

    import org.apache.poi.ss.usermodel._

    val greenStyle = sheet.getRow(31).getCell(0).getCellStyle
    val yellowStyle = sheet.getRow(31).getCell(1).getCellStyle
    val violetStyle = sheet.getRow(31).getCell(2).getCellStyle
    val brownStyle = sheet.getRow(31).getCell(3).getCellStyle

    for {
      row <- 5 to 5 + 24 - 1
      data = psiHourRecords(row - 5)
    } {
      if (data._1.isDefined) {
        val cell = sheet.getRow(row).getCell(1)
        val v = data._1.get
        cell.setCellValue(v)
        if (v < 50)
          cell.setCellStyle(greenStyle)
        else if (v <= 100)
          cell.setCellStyle(yellowStyle)
        else if (v < 200)
          cell.setCellStyle(violetStyle)
        else
          cell.setCellStyle(brownStyle)
      } else
        sheet.getRow(row).getCell(1).setCellValue("-")

      for {
        mtv <- MonitorType.psiList.zipWithIndex
        psi = data._2(mtv._1)
      } {
        if (psi._1.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue(psi._1.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue("-")

        if (psi._2.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue(psi._2.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  import models.Realtime._
  def psiMonthlyReport(monitor: Monitor.Value, reportDate: DateTime, psiDailyList: List[PsiReport], nDays: Int)(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("psi_monthly.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)
    sheet.getRow(1).getCell(10).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(10).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月"))
    for {
      mtv <- MonitorType.psiList.zipWithIndex
    } {
      sheet.getRow(3).getCell(2 + mtv._2 * 2).setCellValue(MonitorType.map(mtv._1).desp)
    }

    val greenStyle = sheet.getRow(38).getCell(0).getCellStyle
    val yellowStyle = sheet.getRow(38).getCell(1).getCellStyle
    val violetStyle = sheet.getRow(38).getCell(2).getCellStyle
    val brownStyle = sheet.getRow(38).getCell(3).getCellStyle

    for {
      row <- 5 to 5 + nDays - 1
      data = psiDailyList(row - 5)
    } {
      if (data.psi.isDefined) {
        val cell = sheet.getRow(row).getCell(1)
        val v = data.psi.get
        cell.setCellValue(v)
        if (v < 50)
          cell.setCellStyle(greenStyle)
        else if (v <= 100)
          cell.setCellStyle(yellowStyle)
        else if (v < 200)
          cell.setCellStyle(violetStyle)
        else
          cell.setCellStyle(brownStyle)
      } else
        sheet.getRow(row).getCell(1).setCellValue("-")

      for {
        mtv <- MonitorType.psiList.zipWithIndex
        psi = data.sub_map(mtv._1)
      } {
        if (psi._1.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue(psi._1.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2 + 1).setCellValue("-")

        if (psi._2.isDefined)
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue(psi._2.get)
        else
          sheet.getRow(row).getCell(2 + mtv._2 * 2).setCellValue("-")
      }
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def covertDegToDir(degree: Float) = {
    val dirMap =
      Map(
        (0 -> "北"), (1 -> "北北東"), (2 -> "東北"), (3 -> "東北東"), (4 -> "東"),
        (5 -> "東南東"), (6 -> "東南"), (7 -> "南南東"), (8 -> "南"),
        (9 -> "南南西"), (10 -> "西南"), (11 -> "西西南"), (12 -> "西"),
        (13 -> "西北西"), (14 -> "西北"), (15 -> "北北西"))

    val step = 360 / 16
    val dir = Math.ceil((degree - (step / 2)) / step).toInt % 16
    dirMap(dir)
  }

  def epaCompareReport(monitor: Monitor.Value, epaMonitor: EpaMonitor.Value, reportDate: DateTime, myMap: Map[MonitorType.Value, (Map[DateTime, (Option[Float], Option[String])], Stat)], epaMap: Map[MonitorType.Value, (Map[DateTime, EpaHourRecord], Stat)], hours: List[DateTime]) = {
    implicit val (reportFilePath, pkg, wb) = prepareTemplate("epa_compare.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name + "/環保署測站:" + EpaMonitor.map(epaMonitor).name)
    sheet.getRow(1).getCell(24).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(24).setCellValue("資料日期:" + reportDate.toString("YYYY年MM月dd"))

    val abnormalColor =
      {
        val seqColors =
          for (col <- 9 to 13)
            yield wb.getSheetAt(0).getRow(2).getCell(col).getCellStyle.getFillForegroundXSSFColor

        seqColors.toArray
      }

    var row = 5
    for {
      mt <- MonitorType.epaReportList.zipWithIndex
      normalStyle = createStyle(mt._1)
      abnormalStyles = createColorStyle(abnormalColor, mt._1)
      //row = mt._2 * 2 + 5
    } {
      sheet.getRow(row).getCell(1).setCellValue(Monitor.map(monitor).name)
      sheet.getRow(row + 1).getCell(1).setCellValue(EpaMonitor.map(epaMonitor).name)
      if (mt._1 == MonitorType.WD_DIR) {
        sheet.getRow(row + 2).getCell(1).setCellValue(Monitor.map(monitor).name)
        sheet.getRow(row + 3).getCell(1).setCellValue(EpaMonitor.map(epaMonitor).name)
      }

      for {
        hr <- hours.zipWithIndex
        col = hr._2 + 2
        cell = sheet.getRow(row).getCell(col)
      } {
        val vOpt = myMap(mt._1)._1.get(hr._1)
        if (vOpt.isDefined) {
          val p = vOpt.get
          if (mt._1 == MonitorType.WD_DIR) {
            val cellDir = sheet.getRow(row + 2).getCell(col)
            cell.setCellValue(p._1.get)
            cellDir.setCellValue(covertDegToDir(p._1.get))
          } else
            cell.setCellValue(p._1.get)

          val status = p._2.get
          val cellStyle = getStyle(status, normalStyle, abnormalStyles)
          cell.setCellStyle(cellStyle)
        } else {
          cell.setCellValue("-")
        }
      }

      def fillOpt(r: Int, vOpt: Option[Float], idx: Int) {
        vOpt.fold({
          sheet.getRow(r).getCell(idx).setCellValue("-")
          sheet.getRow(r + 2).getCell(idx).setCellValue("-")
        })(v => {
          sheet.getRow(r).getCell(idx).setCellValue(v)
          sheet.getRow(r + 2).getCell(idx).setCellValue(covertDegToDir(v))
        })
      }
      {
        val stat = myMap(mt._1)._2
        fillOpt(row, stat.min, 26)
        fillOpt(row, stat.max, 27)
        fillOpt(row, stat.avg, 28)
      }

      for {
        hr <- hours.zipWithIndex
        col = hr._2 + 2
        cell = sheet.getRow(row + 1).getCell(col)
      } {
        val vOpt = epaMap(mt._1)._1.get(hr._1)
        if (vOpt.isEmpty) {
          cell.setCellValue("-")
        } else {
          val epaRecord = vOpt.get
          if (mt._1 == MonitorType.WD_DIR) {
            val cellDir = sheet.getRow(row + 3).getCell(col)
            cell.setCellValue(epaRecord.value)
            cellDir.setCellValue(covertDegToDir(epaRecord.value))
          } else
            cell.setCellValue(epaRecord.value)

          cell.setCellStyle(normalStyle)
        }
      }

      {
        val stat = epaMap(mt._1)._2
        fillOpt(row + 1, stat.min, 26)
        fillOpt(row + 1, stat.max, 27)
        fillOpt(row + 1, stat.avg, 28)
      }

      if (mt._1 == MonitorType.WD_DIR)
        row += 4
      else
        row += 2
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  import Calibration._

  def calibrationDailyReport(title: String, reportDate: DateTime, report: List[CalibrationItem], displayDate: Boolean = false)(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("calibration_daily.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(0).getCell(0).setCellValue(title)
    sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY/MM/dd"))

    val internalStyle = wb.getSheetAt(0).getRow(2).getCell(9).getCellStyle
    val lawStyle = wb.getSheetAt(0).getRow(2).getCell(10).getCellStyle

    val styles =
      for (row <- 0 to 1) yield {
        for (col <- 0 to 2)
          yield wb.getSheetAt(1).getRow(row).getCell(col).getCellStyle
      }

    var styleIdx = -1
    var currentMonitor: Option[Monitor.Value] = None
    var offset = -1
    def adjustStyleIdx(m: Monitor.Value, row: Int) = {
      if (currentMonitor != Some(m)) {
        currentMonitor = Some(m)
        styleIdx += 1
        styleIdx %= 2
        if (offset < 0) {
          offset = 0
        } else {
          copyHeaderRowTo(row + offset)
          offset += 1
        }
      }
    }

    def copyHeaderRowTo(row: Int) {
      val headerRow = sheet.getRow(3)
      val newRow = sheet.createRow(row)
      for (col <- 0 to 11) {
        val headerCell = headerRow.getCell(col)
        val cell = newRow.createCell(col)
        cell.setCellStyle(headerCell.getCellStyle)
        cell.setCellValue(headerCell.getStringCellValue)
      }
    }
    def fillCell(cell: XSSFCell, v: String, idx: Int) {
      cell.setCellValue(v)
      cell.setCellStyle(styles(styleIdx)(idx))
    }

    def fillCellF(cell: XSSFCell, vOpt: Option[Float], idx: Int) {
      vOpt.fold(cell.setCellValue("-"))(v => {
        cell.setCellValue(v)
        cell.setCellStyle(styles(styleIdx)(idx))
      })
    }

    for {
      row <- 4 to (4 + report.length - 1)
      item = report(row - 4)
      mt = item.monitorType
    } {
      adjustStyleIdx(item.monitor, row)
      val newRow = sheet.createRow(row + offset)
      fillCell(newRow.createCell(0), Monitor.map(item.monitor).name, 0)
      if (!displayDate)
        fillCell(newRow.createCell(1), item.startTime.toString("HH:mm"), 0)
      else
        fillCell(newRow.createCell(1), item.startTime.toString("YYYY-M-d HH:mm"), 0)

      fillCell(newRow.createCell(2), MonitorType.map(item.monitorType).desp, 0)

      if (!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_law))
        fillCellF(newRow.createCell(3), item.z_val, 2)
      else if (!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_internal))
        fillCellF(newRow.createCell(3), item.z_val, 1)
      else
        fillCellF(newRow.createCell(3), item.z_val, 0)

      fillCellF(newRow.createCell(4), MonitorType.map(item.monitorType).zd_internal, 0)
      fillCellF(newRow.createCell(5), MonitorType.map(item.monitorType).zd_law, 0)

      fillCellF(newRow.createCell(6), item.s_std, 0)
      fillCellF(newRow.createCell(7), item.s_sval, 0)
      if (!passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_law)) {
        fillCellF(newRow.createCell(8), item.sd_pnt, 2)
      } else if (!passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_internal)) {
        fillCellF(newRow.createCell(8), item.sd_pnt, 1)
      } else
        fillCellF(newRow.createCell(8), item.sd_pnt, 0)

      fillCellF(newRow.createCell(9), MonitorType.map(item.monitorType).sd_internal, 0)
      fillCellF(newRow.createCell(10), MonitorType.map(item.monitorType).sd_law, 0)

      if (!passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_law) ||
        !passStandard(item.z_val, MonitorType.map(item.monitorType).zd_law)) {
        fillCell(newRow.createCell(11), "失敗", 2)
      } else {
        if (!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_internal) ||
          !passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_internal)) {
          fillCell(newRow.createCell(11), "成功", 1)
        } else
          fillCell(newRow.createCell(11), "成功", 0)
      }
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def calibrationMonthlyAllReport(monitor: Monitor.Value, reportDate: DateTime, map: Map[MonitorType.Value, Map[String, Calibration.CalibrationItem]], nDays: Int)(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("all_calibration_monthly.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    def fillMonitorTypeReport(monitorType: MonitorType.Value, map: Map[String, Calibration.CalibrationItem], sheetIdx: Int) = {
      val sheet = wb.getSheetAt(sheetIdx)
      sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
      sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY/MM"))
      sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)

      val internalStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle
      val lawStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle
      def fillCellF(cell: XSSFCell, vOpt: Option[Float]) {
        vOpt.fold(cell.setCellValue("-"))(v => {
          cell.setCellValue(v)
        })
      }

      for {
        row <- 4 to (4 + nDays - 1)
        itemOpt = map.get((row - 3).toString)
      } {
        if (itemOpt.isDefined) {
          val item = itemOpt.get
          sheet.getRow(row).getCell(1).setCellValue(item.startTime.toString("HH:mm"))
          sheet.getRow(row).getCell(2).setCellValue(MonitorType.map(item.monitorType).desp)
          if (item.z_val.isDefined && MonitorType.map(item.monitorType).zd_law.isDefined
            && MonitorType.map(item.monitorType).zd_internal.isDefined) {
            val z_val = item.z_val.get
            if (z_val > MonitorType.map(item.monitorType).zd_law.get) {
              sheet.getRow(row).getCell(3).setCellStyle(lawStyle)
            } else if (z_val > MonitorType.map(item.monitorType).zd_internal.get) {
              sheet.getRow(row).getCell(3).setCellStyle(internalStyle)
            }
          }
          fillCellF(sheet.getRow(row).getCell(3), item.z_val)
          fillCellF(sheet.getRow(row).getCell(4), MonitorType.map(item.monitorType).zd_internal)
          fillCellF(sheet.getRow(row).getCell(5), MonitorType.map(item.monitorType).zd_law)
          fillCellF(sheet.getRow(row).getCell(6), item.s_std)
          fillCellF(sheet.getRow(row).getCell(7), item.s_sval)
          if (item.sd_pnt.isDefined && 
              MonitorType.map(item.monitorType).sd_law.isDefined && 
              MonitorType.map(item.monitorType).sd_internal.isDefined) {
            val sd_pnt = item.sd_pnt.get
            if (sd_pnt > MonitorType.map(item.monitorType).sd_law.get) {
              sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
            } else if (sd_pnt > MonitorType.map(item.monitorType).sd_internal.get) {
              sheet.getRow(row).getCell(8).setCellStyle(internalStyle)
            }
          }
          fillCellF(sheet.getRow(row).getCell(8), item.sd_pnt)
          fillCellF(sheet.getRow(row).getCell(9), MonitorType.map(item.monitorType).sd_internal)
          fillCellF(sheet.getRow(row).getCell(10), MonitorType.map(item.monitorType).sd_law)

          if ((!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_law)) ||
            (!passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_law))) {
            sheet.getRow(row).getCell(11).setCellStyle(lawStyle)
            sheet.getRow(row).getCell(11).setCellValue("失敗")
          } else {
            if ((!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_internal)) ||
              (!passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_internal))) {
              sheet.getRow(row).getCell(11).setCellStyle(internalStyle)
            }
            sheet.getRow(row).getCell(11).setCellValue("成功")
          }
        }
      }
      sheet.getRow(36).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})零點校正趨勢圖")
      sheet.getRow(37).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅校正趨勢圖")
      //sheet.getRow(38).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅讀值趨勢圖")
    }

    var first_mtidx = -1
    for ((mt, idx) <- MonitorType.calibrationList.zipWithIndex) {
      val mtMap = map.get(mt)
      if (mtMap.isDefined && mtMap.get.size > 0) {
        fillMonitorTypeReport(mt, mtMap.get, idx)
        if (first_mtidx == -1) {
          first_mtidx = idx
        }
      } else {
        wb.setSheetHidden(idx, true)
      }
    }
    if (first_mtidx != -1)
      wb.setActiveSheet(first_mtidx)

    finishExcel(reportFilePath, pkg, wb)
  }

  def calibrationMonthlyReport(monitor: Monitor.Value, monitorType: MonitorType.Value, reportDate: DateTime, map: Map[String, Calibration.CalibrationItem], nDays: Int)(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("calibration_monthly.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(11).setCellValue("查詢日期:" + DateTime.now.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(11).setCellValue("資料日期:" + reportDate.toString("YYYY/MM"))
    sheet.getRow(2).getCell(0).setCellValue("測站:" + Monitor.map(monitor).name)

    val internalStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle
    val lawStyle = wb.getSheetAt(0).getRow(2).getCell(8).getCellStyle
    def fillCellF(cell: XSSFCell, vOpt: Option[Float]) {
      vOpt.fold(cell.setCellValue("-"))(v => {
        cell.setCellValue(v)
      })
    }

    for {
      row <- 4 to (4 + nDays - 1)
      itemOpt = map.get((row - 4).toString)
    } {
      if (itemOpt.isDefined) {
        val item = itemOpt.get
        sheet.getRow(row).getCell(1).setCellValue(item.startTime.toString("HH:mm"))
        sheet.getRow(row).getCell(2).setCellValue(MonitorType.map(item.monitorType).desp)
        if (!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_law)) {
          sheet.getRow(row).getCell(3).setCellStyle(lawStyle)
        } else if (!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_internal)) {
          sheet.getRow(row).getCell(3).setCellStyle(internalStyle)
        }

        fillCellF(sheet.getRow(row).getCell(3), item.z_val)
        fillCellF(sheet.getRow(row).getCell(4), MonitorType.map(item.monitorType).zd_internal)
        fillCellF(sheet.getRow(row).getCell(5), MonitorType.map(item.monitorType).zd_law)
        fillCellF(sheet.getRow(row).getCell(6), item.s_std)
        fillCellF(sheet.getRow(row).getCell(7), item.s_sval)
        if (!passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_law)) {
          sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
        } else if (!passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_internal)) {
          sheet.getRow(row).getCell(8).setCellStyle(lawStyle)
        }
        fillCellF(sheet.getRow(row).getCell(8), item.sd_pnt)
        fillCellF(sheet.getRow(row).getCell(9), MonitorType.map(item.monitorType).sd_internal)
        fillCellF(sheet.getRow(row).getCell(10), MonitorType.map(item.monitorType).sd_law)
        if (!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_law) ||
          !passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_law)) {
          sheet.getRow(row).getCell(11).setCellStyle(lawStyle)
          sheet.getRow(row).getCell(11).setCellValue("失敗")
        } else {
          if (!passStandard(item.z_val, MonitorType.map(item.monitorType).zd_internal) ||
            !passStandard(item.sd_pnt, MonitorType.map(item.monitorType).sd_internal)) {
            sheet.getRow(row).getCell(11).setCellStyle(internalStyle)
          }
          sheet.getRow(row).getCell(11).setCellValue("成功")
        }

      }
    }
    sheet.getRow(36).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})零點校正趨勢圖")
    sheet.getRow(37).getCell(0).setCellValue(s"${Monitor.map(monitor).name} (${MonitorType.map(monitorType).desp})全幅校正趨勢圖")

    finishExcel(reportFilePath, pkg, wb)
  }
  
  val exportStatus = false

  def exportChartData(chart: HighchartData, monitorTypes: Array[MonitorType.Value],
                      templateFileName: String = "chart_export.xlsx"): File = {
    val precArray = monitorTypes.map { mt => MonitorType.map(mt).prec }
    exportChartData(chart, precArray, templateFileName)
  }

  def exportChartData(chart: HighchartData, precArray: Array[Int],
                      templateFileName: String) = {
    val (reportFilePath, pkg, wb) = prepareTemplate(templateFileName)
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val sheet = wb.getSheetAt(0)
    val headerRow = sheet.createRow(0)
    headerRow.createCell(0).setCellValue("時間")

    var pos = 0
    for {
      col <- 1 to chart.series.length
      series = chart.series(col - 1)
    } {
      headerRow.createCell(pos + 1).setCellValue(series.name)
      pos += 1
      if (series.status.isDefined && exportStatus) {
        headerRow.createCell(pos + 1).setCellValue("狀態碼")
        pos += 1
      }
    }

    val styles = precArray.map { prec =>
      val format_str = "0." + "0" * prec
      val style = wb.createCellStyle();
      style.setDataFormat(format.getFormat(format_str))
      style
    }

    // Categories data
    if (chart.xAxis.categories.isDefined) {
      val timeList = chart.xAxis.categories.get
      for (row <- timeList.zipWithIndex) {
        val rowNo = row._2 + 1
        val thisRow = sheet.createRow(rowNo)
        thisRow.createCell(0).setCellValue(row._1)

        for {
          col <- 1 to chart.series.length
          series = chart.series(col - 1)
        } {
          val cell = thisRow.createCell(col)
          if (styles.length != 0)
            cell.setCellStyle(styles((col - 1) % styles.length))

          val pair = series.data(rowNo - 1)
          if (pair.length == 2 && pair(1).isDefined) {
            cell.setCellValue(pair(1).get)
          }
          //val pOpt = series.data(rowNo-1)
          //if(pOpt.isDefined){
          //  cell.setCellValue(pOpt.get)
          //}

        }
      }
    } else {
      val rowMax = chart.series.map(s => s.data.length).max
      for (row <- 1 to rowMax) {
        val thisRow = sheet.createRow(row)
        val timeCell = thisRow.createCell(0)
        pos = 0
        for {
          col <- 1 to chart.series.length
          series = chart.series(col - 1)
        } {
          val cell = thisRow.createCell(pos + 1)
          pos += 1
          if (styles.length != 0)
            cell.setCellStyle(styles((col - 1) % styles.length))

          val pair = series.data(row - 1)
          if (col == 1) {
            val dt = new DateTime(pair(0).get.toLong)
            timeCell.setCellValue(dt.toString("YYYY/MM/dd HH:mm"))
          }
          if (pair(1).isDefined) {
            cell.setCellValue(pair(1).get)
          }

          if (series.status.isDefined && exportStatus) {
            val statusCell = thisRow.createCell(pos + 1)
            pos += 1
            val statusOpt = series.status.get(row - 1)
            if (statusOpt.isDefined) {
              statusCell.setCellValue(statusOpt.get)
            }
          }
        }
      }
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def exportWeekForm(ticket: Ticket, usrMap: Map[Int, User], oldTicketOpt: Option[Ticket] = None) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("weekMaintance.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm
    val oldForm = oldTicketOpt.map { t => t.getForm }
    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(42).getCell(1).setCellValue(monitorName)
    sheet.getRow(75).getCell(1).setCellValue(monitorName)
    sheet.getRow(113).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(42).getCell(5).setCellValue(dateStr)
    sheet.getRow(75).getCell(5).setCellValue(dateStr)
    sheet.getRow(113).getCell(5).setCellValue(dateStr)
    oldTicketOpt.map {
      old =>
        val dateStr = old.executeDate.toString("YY/MM/d")
        sheet.getRow(1).getCell(3).setCellValue(dateStr)
    }

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(43).getCell(5).setCellValue(usrName)
    sheet.getRow(76).getCell(5).setCellValue(usrName)
    sheet.getRow(114).getCell(5).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(4).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(6).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(7).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    oldForm.map { old => old.strIdx = form.strIdx }
    sheet.getRow(8).getCell(2).setCellValue(form.getStrSeq)
    oldForm.map { old => sheet.getRow(8).getCell(5).setCellValue(old.getStrSeq) }

    sheet.getRow(8).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(10).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 12 to 22) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(23).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(25).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 27 to 36) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(37).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    //Page 2
    sheet.getRow(44).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 46 to 51) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(52).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(54).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 56 to 59) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(60).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(62).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 64 to 66) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(68).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(69).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(70).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    //Page 3
    sheet.getRow(77).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    for (row <- 78 to 78) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("☑", "□"))
    }
    sheet.getRow(79).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(80).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(81).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(83).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(84).getCell(2).setCellValue("溫度：" + form.getStrSeq)
    sheet.getRow(84).getCell(3).setCellValue("濕度：" + form.getStrSeq)
    oldForm.map { old => sheet.getRow(84).getCell(5).setCellValue(old.getStrSeq + "/" + old.getStrSeq) }
    sheet.getRow(84).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(85).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    sheet.getRow(89).getCell(2).setCellValue(form.getStrSeq)
    oldForm.map { old => sheet.getRow(89).getCell(5).setCellValue(old.getStrSeq) }
    sheet.getRow(89).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    //sheet.getRow(90).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    //sheet.getRow(91).getCell(2).setCellValue("用電量：" + form.getStrSeq)
    //oldForm.map { old => sheet.getRow(91).getCell(5).setCellValue(old.getStrSeq) }
    //sheet.getRow(91).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(92).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(93).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    //sheet.getRow(94).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(95).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(96).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    sheet.getRow(98).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(99).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(100).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(101).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(102).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 104 to 105) {
      sheet.getRow(row).getCell(2).setCellValue(form.getStrSeq)
      oldForm.map { old => sheet.getRow(row).getCell(5).setCellValue(old.getStrSeq) }
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 106 to 108) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    //Page 4
    for (row <- 116 to 119) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 121 to 126) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    sheet.getRow(131).getCell(1).setCellValue(form.getComment(0))
    sheet.getRow(139).getCell(4).setCellValue(usrMap(ticket.owner_id).name)

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportBiWeekForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("biweekForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(11).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(11).setCellValue(usrName)
    sheet.getRow(36).getCell(8).setCellValue(usrName)

    for (row <- 4 to 10) {
      sheet.getRow(row).getCell(1).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(3).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 4 to 10) {
      sheet.getRow(row).getCell(4).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(5).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(6).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(7).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
      sheet.getRow(row).getCell(8).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(9).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(10).setCellValue(form.getStrSeq)
      sheet.getRow(row).getCell(11).setCellValue(form.getStrSeq)
    }

    for (row <- 12 to 18) {
      sheet.getRow(row).getCell(7).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    sheet.getRow(20).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportMonthForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("monthForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(35).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(35).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(36).getCell(5).setCellValue(usrName)
    sheet.getRow(72).getCell(4).setCellValue(usrName)

    for (row <- 3 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 9 to 10) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 12 to 14) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 17 to 19) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 22 to 24) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 27 to 29) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 37 to 40) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 43 to 44) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 47 to 50) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 60 to 62) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    sheet.getRow(64).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportQuarterForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("quarterForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(40).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(40).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(41).getCell(5).setCellValue(usrName)
    sheet.getRow(65).getCell(4).setCellValue(usrName)

    sheet.getRow(5).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(7).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 9 to 14) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 16 to 19) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 21 to 24) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 26 to 30) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 32 to 35) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 42 to 46) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    sheet.getRow(48).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(50).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 52 to 55) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    sheet.getRow(57).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportHalfYearForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("halfYearForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)
    sheet.getRow(27).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)
    sheet.getRow(27).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(28).getCell(5).setCellValue(usrName)
    sheet.getRow(55).getCell(4).setCellValue(usrName)

    //sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 6 to 7) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 10 to 11) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 13 to 14) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 16 to 17) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 19 to 22) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 30 to 33) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 35 to 37) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 39 to 42) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 44 to 45) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    sheet.getRow(47).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportYearForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("YearForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YY/MM/d")
    sheet.getRow(1).getCell(5).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(2).getCell(5).setCellValue(usrName)
    sheet.getRow(28).getCell(5).setCellValue(usrName)
    sheet.getRow(42).getCell(4).setCellValue(usrName)

    sheet.getRow(3).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    sheet.getRow(5).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))

    for (row <- 7 to 9) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 11 to 13) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 15 to 16) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 18 to 20) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 22 to 24) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    for (row <- 26 to 28) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }
    for (row <- 30 to 32) {
      sheet.getRow(row).getCell(4).setCellValue(form.getBoolSeq("YES☑  NO□", "YES□ NO☑"))
    }

    sheet.getRow(34).getCell(1).setCellValue(form.getComment(0))

    finishExcel(reportFilePath, pkg, wb)
  }

  def exportRepairForm(ticket: Ticket, usrMap: Map[Int, User]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("repairForm.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val format = wb.createDataFormat();

    val form = ticket.getRepairForm

    val sheet = wb.getSheetAt(0)
    val monitorName = Monitor.map(ticket.monitor).name
    sheet.getRow(1).getCell(1).setCellValue(monitorName)

    val dateStr = ticket.executeDate.toString("YYYY/MM/dd")
    sheet.getRow(1).getCell(8).setCellValue(dateStr)

    val usrName = usrMap(ticket.owner_id).name
    sheet.getRow(1).getCell(10).setCellValue(usrName)
    sheet.getRow(37).getCell(1).setCellValue(usrName)

    sheet.getRow(5).getCell(5).setCellValue(ticket.submit_date.toString("YYYY/MM/dd"))
    sheet.getRow(6).getCell(5).setCellValue(form.start)
    sheet.getRow(5).getCell(7).setCellValue(form.end)

    val equipOpt = Equipment.getEquipment(form.equipmentId)
    if (equipOpt.isDefined) {
      val equip = equipOpt.get
      sheet.getRow(1).getCell(4).setCellValue(s"儀器設備：${equip.name}(${equip.id})")
      sheet.getRow(2).getCell(5).setCellValue(equip.brand)
      sheet.getRow(3).getCell(5).setCellValue(equip.name)
      sheet.getRow(4).getCell(5).setCellValue(equip.model)
    }
    var partRow = 24
    for (p <- form.parts) {
      val partOpt = Part.getPart(p.id)
      if (partOpt.isDefined) {
        val part = partOpt.get
        sheet.getRow(partRow).getCell(0).setCellValue(part.equipment)
        sheet.getRow(partRow).getCell(1).setCellValue(part.name)
        sheet.getRow(partRow).getCell(4).setCellValue(part.id)
        sheet.getRow(partRow).getCell(5).setCellValue(p.source)
        sheet.getRow(partRow).getCell(6).setCellValue(
          if (p.charged)
            "Yes"
          else
            "No")
        sheet.getRow(partRow).getCell(8).setCellValue(p.unit_price)
        sheet.getRow(partRow).getCell(9).setCellValue(p.amount)
        sheet.getRow(partRow).getCell(10).setCellValue(p.total)
        partRow += 1
      }
    }
    sheet.getRow(8).getCell(0).setCellValue(form.explain)
    sheet.getRow(8).getCell(5).setCellValue(form.result)

    sheet.getRow(6).getCell(1).setCellValue(form.getStr(0))
    sheet.getRow(4).getCell(10).setCellValue(form.getBoolStr(2, "☑", "□"))
    sheet.getRow(4).getCell(9).setCellValue(form.getStr(1))
    sheet.getRow(5).getCell(10).setCellValue(form.getBoolStr(1, "☑", "□"))
    sheet.getRow(6).getCell(10).setCellValue(form.getBoolStr(0, "☑", "□"))
    sheet.getRow(20).getCell(6).setCellValue(form.getBoolStr(3, "☑", "□") + "已修好")
    sheet.getRow(20).getCell(7).setCellValue(form.getBoolStr(4, "☑", "□") + "未修好")
    sheet.getRow(20).getCell(8).setCellValue(form.getBoolStr(5, "☑", "□") + "待料")

    //Attach photo
    val helper = wb.getCreationHelper();
    val anchor = helper.createClientAnchor();
    anchor.setCol1(1);
    anchor.setRow1(2);

    /*
    Ticket.getTicketPhoto(ticket.id).map { params =>
      for {
        photo_idx <- params.photos.zipWithIndex
        blobOpt = photo_idx._1 if blobOpt.isDefined
        idx = photo_idx._2
        blob = blobOpt.get
        photoSheet = wb.createSheet(s"照片$idx")
      } {
        import org.apache.commons.io._
        val is = blob.getBinaryStream
        val bytes = IOUtils.toByteArray(is)
        is.close
        val pictureIdx =
          if (bytes(0) == 0x89.toByte)
            wb.addPicture(bytes, Workbook.PICTURE_TYPE_PNG);
          else
            wb.addPicture(bytes, Workbook.PICTURE_TYPE_JPEG);

        val drawing = photoSheet.createDrawingPatriarch();
        val pict = drawing.createPicture(anchor, pictureIdx);
        pict.resize()
      }
    }
    * 
    */

    finishExcel(reportFilePath, pkg, wb)
  }

  def monitorAbnormalReport(date: DateTime, report: Seq[AbnormalEntry])(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("abnormalReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(4).setCellValue("印表日期:" + DateTime.now.toString("YYYY年MM月dd日"))
    sheet.getRow(2).getCell(4).setCellValue("資料日期:" + date.toString("YYYY年MM月dd日"))

    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)

    for (r <- report.reverse) {
      sheet.shiftRows(4, sheet.getLastRowNum, 1)
      val row = sheet.createRow(4)
      def fillCell(i: Int, v: String) = {
        row.createCell(i).setCellStyle(style)
        row.getCell(i).setCellValue(v)
      }

      fillCell(0, Monitor.map(r.monitor).name)
      fillCell(1, MonitorType.map(r.monitorType).desp)
      fillCell(2, date.toString("MM/dd"))
      fillCell(3, r.invalidHours)
      fillCell(4, r.explain)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def monitorAggregateReport(date: DateTime, report: Seq[MonitorSummary]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("aggregateReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(3).setCellValue("印表日期:" + DateTime.now.toString("YYYY年MM月dd日"))
    sheet.getRow(2).getCell(3).setCellValue("資料日期:" + date.toString("YYYY年MM月dd日"))

    val style = wb.createCellStyle();
    val format = wb.createDataFormat();
    // Create a new font and alter it.
    val font = wb.createFont();
    font.setFontHeightInPoints(10);
    font.setFontName("標楷體");

    style.setFont(font)

    for (m <- report.zipWithIndex) {
      val r = m._1
      val idx = m._2

      def fillCell(i: Int, v: String) = {
        val row = sheet.getRow(idx + 4)
        row.getCell(i).setCellValue(v)
      }

      fillCell(0, Monitor.map(r.monitor).name)
      fillCell(1, r.desc)
      fillCell(2, r.explain)
    }

    finishExcel(reportFilePath, pkg, wb)
  }

  def monitorJournalReport(report: MonitorJournal, invalidHourList: List[(MonitorType.Value, List[MonitorInvalidHour])], userList: List[User])(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("monitorJournal.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val sheet = wb.getSheetAt(0)
    sheet.getRow(1).getCell(0).setCellValue("測站名稱:" + Monitor.map(report.monitor).name)
    sheet.getRow(1).getCell(6).setCellValue("印表日期:" + DateTime.now.toString("YYYY年MM月dd日"))
    sheet.getRow(2).getCell(0).setCellValue("到站日期:" + report.date.toString("YYYY年MM月dd日"))
    if (report.operator_id.isDefined) {
      val operatorOpt = userList.find { u => u.id == report.operator_id }
      if (operatorOpt.isDefined) {
        val operator = operatorOpt.get
        sheet.getRow(3).getCell(1).setCellValue(operator.name)
      }
    }

    sheet.getRow(3).getCell(6).setCellValue(report.enter_time.toString())
    sheet.getRow(3).getCell(7).setCellValue(report.out_time.toString())
    sheet.getRow(5).getCell(0).setCellValue(report.routine_desc)
    sheet.getRow(11).getCell(0).setCellValue(report.abnormal_desc)
    sheet.getRow(17).getCell(0).setCellValue(report.event_desc)

    val style = wb.createCellStyle();
    val font = wb.createFont();
    font.setFontHeightInPoints(12);
    font.setFontName("標楷體");

    style.setFont(font)

    var startRow = 24
    for {
      mt <- invalidHourList
      ih <- mt._2
    } {
      val row = sheet.createRow(startRow)
      def fillCell(i: Int, v: String) = {
        row.createCell(i).setCellStyle(style)
        row.getCell(i).setCellValue(v)
      }
      fillCell(0, MonitorType.map(mt._1).desp)
      fillCell(1, report.date.toString("MM/dd"))
      fillCell(2, ih.invalidHour)
      fillCell(3, ih.status)

      startRow += 1
    }
    finishExcel(reportFilePath, pkg, wb)
  }

  def minMonthlyReport(monitors: List[Monitor.Value], start: DateTime, callback: (Int) => Unit)(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("minMonthlyReport.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    val fgColors =
      {
        val sheet0 = wb.getSheetAt(0)
        val seqColors =
          for (col <- 1 to 5)
            yield sheet0.getRow(1).getCell(col).getCellStyle.getFillForegroundXSSFColor
        seqColors.toArray
      }

    callback(20)
    for ((m, idx) <- monitors.zipWithIndex) {
      val minRecords = Record.getMinRecords(m, start, start + 1.month)
      val sheet = wb.createSheet(Monitor.map(m).name)
      sheet.createRow(0).createCell(0).setCellValue("時間")
      val timeSeries = minRecords.map { Record.timeProjection }
      for { (time, time_idx) <- timeSeries.zipWithIndex } {
        val row = sheet.createRow(time_idx + 1)
        val time_cell = row.createCell(0)
        time_cell.setCellValue(time.toString("YYYY/MM/dd HH:mm"))
      }

      for {
        (mt, mt_idx) <- Monitor.map(m).monitorTypes.zipWithIndex
        unit1 = sheet.getRow(0).createCell(mt_idx * 2 + 1).setCellValue(MonitorType.map(mt).desp)
        unit2 = sheet.getRow(0).createCell(mt_idx * 2 + 2).setCellValue("狀態碼")
        mtRecords = minRecords.map { Record.monitorTypeProject2(mt) }
        normalStyle = createStyle(mt)(wb)
        abnormalStyles = createColorStyle(fgColors, mt)(wb)
      } {

        val progress = 20 + 80 * (mt_idx + 1) / Monitor.map(m).monitorTypes.length
        callback(progress)

        for {
          (rec, rec_idx) <- mtRecords.zipWithIndex
        } {
          val row = sheet.getRow(rec_idx + 1)
          val vCell = row.createCell(mt_idx * 2 + 1)
          val sCell = row.createCell(mt_idx * 2 + 2)
          val valueOpt = rec._1
          val statusOpt = rec._2

          if (valueOpt.isEmpty || statusOpt.isEmpty) {
            vCell.setCellValue("-")
            sCell.setCellValue("-")
          } else {
            val value = valueOpt.get
            val status = statusOpt.get
            vCell.setCellValue(value)
            sCell.setCellValue(status)
            val cellStyle = getStyle(status, normalStyle, abnormalStyles)
            vCell.setCellStyle(cellStyle)
          }
        }
      }
    }
    wb.setActiveSheet(1)
    finishExcel(reportFilePath, pkg, wb)
  }

  def epbNotification(tickets: List[Ticket]) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("epb.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()

    var sheetN = 0
    for (t <- tickets) {
      if (sheetN != 0) {
        wb.cloneSheet(0)
      }
      val sheet = wb.getSheetAt(sheetN)
      //發生時間
      if (t.ticketType == TicketType.repair) {
        sheet.getRow(3).getCell(1).setCellValue(t.submit_date.toString("YYYY/MM/dd HH:mm"))
      } else {
        sheet.getRow(3).getCell(1).setCellValue(t.executeDate.toString("YYYY/MM/dd") + " 9:00:00 AM")
      }
      //發生地點
      sheet.getRow(4).getCell(1).setCellValue(s"台塑空氣品質監測站-${Monitor.map(t.monitor).name}")

      //事故說明 
      sheet.getRow(10).getCell(1).setCellValue(s"執行空氣品質監測站 - ${TicketType.map(t.ticketType)}")

      //結束時間
      if (t.ticketType == TicketType.repair) {
        sheet.getRow(12).getCell(1).setCellValue((t.submit_date + 8.hour).toString("YYYY/MM/dd HH:mm"))
      } else {
        sheet.getRow(12).getCell(1).setCellValue(t.executeDate.toString("YYYY/MM/dd") + " 5:00:00 PM")
      }
      sheetN += 1
    }
    wb.setActiveSheet(0)
    finishExcel(reportFilePath, pkg, wb)
  }

  def equipmentHistoryReport(tickets: List[Ticket], start: DateTime, end: DateTime)(implicit messages: Messages) = {
    val (reportFilePath, pkg, wb) = prepareTemplate("equipHistory.xlsx")
    val evaluator = wb.getCreationHelper().createFormulaEvaluator()
    val sheet = wb.getSheetAt(0)

    sheet.getRow(1).getCell(6).setCellValue("起始日期:" + start.toString("YYYY/MM/dd"))
    sheet.getRow(2).getCell(6).setCellValue("結束日期:" + end.toString("YYYY/MM/dd"))

    for (tz <- tickets.zipWithIndex) {
      val t = tz._1
      val rowN = tz._2 + 4
      val row = sheet.getRow(rowN)
      row.getCell(0).setCellValue(Monitor.map(t.monitor).name)
      row.getCell(1).setCellValue(MonitorType.map(t.monitorType.get).desp)
      row.getCell(2).setCellValue(t.getRepairForm.start)
      row.getCell(3).setCellValue(t.getRepairForm.end)
      row.getCell(4).setCellValue(t.getRepairForm.explain + t.getRepairForm.result)
      row.getCell(5).setCellValue(t.getRepairForm.equipmentId)
      if (t.getRepairForm.parts.length == 0) {
        row.getCell(6).setCellValue("無")
      } else {
        row.getCell(6).setCellValue(t.getRepairForm.parts.map(_.id).mkString(","))
      }

    }
    finishExcel(reportFilePath, pkg, wb)
  }
}

/**
 * @author user
 */
class ExcelUtility @Inject() (val messagesApi: MessagesApi) extends I18nSupport {
  import ExcelUtility._
}