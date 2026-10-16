import scala.collection.mutable

@main def main =
  val myGreeter = new Greeter("Alex")
  println("Hello world!")
  println(myGreeter.hi)
  println(myGreeter.bye)

class SensorNetworkManager:
  private var _sensorMap = mutable.Map[String, Sensor]()
  private var _sensorLog = mutable.ArrayBuffer[Reading]()

  def sensorMap = _sensorMap
  def sensorLog = _sensorLog

  def sensorCount(): Int =
    Analytics.sensorCount(sensorMap)

  def clear(): Unit =
    sensorMap.clear()
    sensorLog.clear()

  def registerSensor(sensor: Sensor): Unit =
    if sensorMap contains sensor.id
      then println(s"Sensor with ID ${sensor.id} already exists.")
    else
      sensorMap(sensor.id) = sensor

  def addReading(reading: Reading): Boolean =
    Analytics.addReading(reading, sensorMap, sensorLog)

  def alertsFor(id: String): Option[Vector[Alert]] =
    Analytics.alertsFor(id, sensorLog, sensorMap)

  def averageFor(id: String): Option[Double] =
    Analytics.averageFor(id, sensorLog)

  def minMax(id: String): Option[(Double, Double)] =
    Analytics.minMax(id, sensorLog, sensorMap)

  def logsFor(id: String): Option[Vector[Reading]] =
    Analytics.logsFor(id, sensorLog)

  def groupedBySensor(): Map[String, Vector[Reading]] =
    Analytics.groupedBySensor(sensorLog)

  def minuteAverages(sensorId: String): Map[Long, Double] =
    Analytics.minuteAverages(sensorId, sensorLog)