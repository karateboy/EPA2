package models

/**
 * @author user
 */
case class EpaMonitor(name: String, id: Int, lat: Double, lng: Double)
object EpaMonitor extends Enumeration {
  val m1 = Value("屏東")
  val m2 = Value("潮州")
  val m3 = Value("大寮")
  val m4 = Value("林園")

  val map = Map(
    m1 -> EpaMonitor("屏東", 59, 22.673081, 120.488033),
    m2 -> EpaMonitor("潮州", 60, 22.523108, 120.561175),
    m3 -> EpaMonitor("大寮", 51, 22.564136, 120.425311),
    m4 -> EpaMonitor("林園", 52, 22.4795, 120.411750))

  val idMap = map.map(r => (r._2.id, r._1))
  val nameMap = map.map(r => (r._2.name, r._1))

  val epaList = values.toList.sorted
}