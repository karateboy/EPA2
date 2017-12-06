package controllers
import akka.actor._
import play.api._
import play.api.mvc._ 
import com.github.nscala_time.time.Imports._
import models._
import javax.inject._
import play.api.i18n._
import models.ExcelUtility

object MinMonthlyReportWorker {
  def props(out: ActorRef)(implicit messages:Messages) = Props(new MinMonthlyReportWorker(out))
}
 
class MinMonthlyReportWorker (out: ActorRef)(implicit messages:Messages) extends Actor {
  object CmdType extends Enumeration{
    val start = Value
    val progress = Value
    val finish = Value
  }
  
  var progress: Int = _
  
  def parseStartCmd(msg:String)={
    val param = msg.split(":")
    (CmdType.withName(param(0)), Monitor.withName(param(1)), DateTime.parse(param(2)))
  }
  
  def progressCallback(percent:Int)={
    val msg = s"${CmdType.progress}:${percent}"
    out ! msg
  }
  
  def receive = {
    case msg: String => 
      val (cmd, monitor, startDate) = parseStartCmd(msg)
      val file = ExcelUtility.minMonthlyReport(List(monitor), startDate, progressCallback)
      val n = SystemConfig.getFreeDownloadLink()
      SystemConfig.setDownloadLink(n, file)
      val outMsg = s"${CmdType.finish}:${n}"
      out ! outMsg
  }
}