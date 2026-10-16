case class Reading(val sensorId: String, val timeStamp: Long, val value: Double):

  override def toString(): String =
    s"Sensor $sensorId: Logged at $timeStamp, Reading: $value"
