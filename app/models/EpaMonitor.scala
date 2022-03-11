package models

/**
 * @author user
 */
case class EpaMonitor(name: String, id: Int, lat: Double, lng: Double)
object EpaMonitor extends Enumeration {
  val m1 = Value("臺東")
  val m2 = Value("關山")

  val map = Map(
    m1 -> EpaMonitor("臺東", 62, 22.755358, 121.15045),
    m2 -> EpaMonitor("關山", 80, 23.045083, 121.161933)
)

  val idMap = map.map(r => (r._2.id, r._1))
  val nameMap = map.map(r => (r._2.name, r._1))

  val epaList = values.toList.sorted
}