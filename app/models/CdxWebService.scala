package models
import com.wecc.cdx

object CdxWebService {

  val service = {
    val ctrl = new cdx.NodeFileCtrl
    ctrl.getNodeFileCtrlSoap
  }
}