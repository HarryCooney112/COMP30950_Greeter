import scala.collection.mutable

@main def main =
  val myGreeter = new Greeter("Alex")
  println("Hello world!")
  println(myGreeter.hi)
  println(myGreeter.bye)

case class Reading(val sensorId: String, val timeStamp: Long, val value: Double):

  override def toString(): String =
    s"Sensor $sensorId: Logged at $timeStamp, Reading: $value"

case class OperatingRange(val min: Double, val max: Double):
  override def toString(): String =
    s"[$min, $max]"

case class Sensor(val id: String, val location: String, val validRange: OperatingRange):
  override def toString(): String =
    s"Id: $id, Location: $location"

case class Alert(val reading: Reading, val reason: String):
  override def toString(): String =
    s"${reading.value} $reason"


class SensorNetworkManager:
  var sensorMap = mutable.Map[String, Sensor]()
  var sensorLog = mutable.Map[String, mutable.ArrayBuffer[Reading]]()

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
      sensorLog(reading.sensorId) += reading
    true

  def alertsFor(id: String): Array[Alert] =
    val sensor = sensorMap(id)
    val alertsFromLogs = sensorLog(id).filter(reading =>
        reading.value < sensor.validRange.min || reading.value > sensor.validRange.max).toArray
    alertsFromLogs.map(reading =>
        Alert(reading, s"Value ${reading.value} out of range ${sensor.validRange}"))

  def averageFor(id: String): Option[Double] =
    sensorLog get id match
      case Some(logs) =>
        val logsLength = logs.length.toDouble
        if logsLength == 0 then return None

        val readingValueSum = logs.map( reading => reading.value ).reduce((a, b) => (a + b))
        Some(readingValueSum / logsLength)
      case None => None
      

  def logsFor(id: String): Option[Array[String]] =
    sensorLog.get(id) match
      case Some(log) => 
        val strings = log.map(reading => reading.toString()).toArray
        Some(strings)
      case None => None

