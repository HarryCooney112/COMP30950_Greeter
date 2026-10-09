import scala.collection.mutable

@main def main =
  val myGreeter = new Greeter("Alex")
  println("Hello world!")
  println(myGreeter.hi)
  println(myGreeter.bye)

case class Reading(val sensorId: String, val timeStamp: Long, val value: Double):

  override def toString(): String =
    s"Logged at $timeStamp, Reading: $value"

case class OperatingRange(val min: Double, val max: Double):
  override def toString(): String =
    s"Min: $min, Max: $max"

case class Sensor(val id: String, val location: String, val validRange: OperatingRange):
  override def toString(): String =
    s"Id: $id, Location: $location"

case class Alert(val reading: Reading, val reason: String):
  override def toString(): String =
    s"This is working"


class SensorNetworkManager:
  var sensorMap = mutable.Map[String, Sensor]()
  var sensorLog = mutable.Map[String, mutable.ArrayBuffer[String]]()

  def sensorCount(): Int =
    sensorMap.size

  def clear(): Unit =
    sensorMap.clear()
    sensorLog.clear()

  def registerSensor(sensor: Sensor): Unit =
    if sensorMap contains sensor.id
      then println(s"Sensor with ID ${sensor.id} already exists.")
    else
      sensorMap(sensor.id) = sensor
      sensorLog(sensor.id) = mutable.ArrayBuffer()

  def addReading(reading: Reading): Boolean =
    if !sensorMap.contains(reading.sensorId)
      then return false
    else
      sensorLog(reading.sensorId) += reading.toString()
    true

  def viewSensorLog(id: String): Unit =
    sensorLog.get(id) match
      case Some(log) => log.foreach(println(_))
      case None => println(s"Sensor with ID $id does not exist in logging system.")
