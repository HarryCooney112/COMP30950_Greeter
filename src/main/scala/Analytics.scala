
import scala.collection.mutable

object Analytics:
  def sensorCount(sensorMap: mutable.Map[String, Sensor]): Int =
    sensorMap.size

  def logsFor(id: String, logs: mutable.ArrayBuffer[Reading]): Option[Vector[Reading]] =
    val logsForSensor = logs.filter( reading => reading.sensorId == id).toVector
    if logsForSensor.length == 0 then None else Some(logsForSensor)

  def addReading(reading: Reading, sensorMap: mutable.Map[String, Sensor], sensorLog: mutable.ArrayBuffer[Reading]): Boolean =
    if !sensorMap.contains(reading.sensorId)
    then return false
    else
      sensorLog += reading
    true

  def alertsFor(sensorId: String, sensorLog: mutable.ArrayBuffer[Reading], sensorMap: mutable.Map[String, Sensor]): Option[Vector[Alert]] =
    val sensor = sensorMap.get(sensorId) match
      case Some(sensor) => sensor
      case None => return None
    logsFor(sensorId, sensorLog) match
      case Some(logs) =>
        val alerts = logs.filter( reading => reading.value < sensor.validRange.min || reading.value > sensor.validRange.max)
          .map( reading => Alert(reading, s"Value ${reading.value} out of range ${sensor.validRange.toString}"))
        if alerts.length == 0 then None else Some(alerts)
      case None => None

  def minMax(sensorId: String, sensorLog: mutable.ArrayBuffer[Reading], sensorMap: mutable.Map[String, Sensor]): Option[(Double, Double)] =
    val sensor = sensorMap.get(sensorId) match
      case Some(sensor) => sensor
      case None => return None
    logsFor(sensorId, sensorLog) match
      case Some(logs) =>
        val logValues = logs.map( reading => reading.value)
        if logValues.length == 0 then return None
        Some((logValues.min, logValues.max))
      case None => None

  def averageFor(sensorId: String, sensorLog: mutable.ArrayBuffer[Reading]): Option[Double] =
    logsFor(sensorId, sensorLog) match
      case Some(logs) =>
        val logValues = logs.map( reading => reading.value)
        if logValues.length == 0 then return None
        Some(logValues.sum / logValues.length)
      case None => None

  def groupedBySensor(sensorLog: mutable.ArrayBuffer[Reading]): Map[String, Vector[Reading]] =
    var readingMap = mutable.Map[String, mutable.ArrayBuffer[Reading]]()
    sensorLog.foreach( (reading) =>
      val sensorId = reading.sensorId
      if !readingMap.contains(sensorId) then
        readingMap(sensorId) = mutable.ArrayBuffer[Reading]()
        readingMap(sensorId) += reading
      else readingMap(sensorId) += reading
    )
    readingMap.map( (a, b) => (a -> b.toVector)).toMap

  def minuteAverages(sensorId: String, sensorLog: mutable.ArrayBuffer[Reading]): Map[Long, Double] =
    var readingMap = mutable.Map[Long, mutable.ArrayBuffer[Double]]()
    val log = logsFor(sensorId, sensorLog) match
      case Some(log) => log
      case None => return Map[Long, Double]()

    log.foreach( reading =>
       val readingTime = reading.timeStamp / 60000
       val readingVal = reading.value

       if !readingMap.contains(readingTime) then
         readingMap(readingTime) = mutable.ArrayBuffer[Double]()
         readingMap(readingTime) += readingVal
       else readingMap(readingTime) += readingVal
    )
    readingMap.map( (key, values) => (key -> calculateAverage(values))).toMap

  def calculateAverage(readingVals: mutable.ArrayBuffer[Double]): Double =
    if readingVals.length == 0 then return 0
    readingVals.sum / readingVals.length.toFloat