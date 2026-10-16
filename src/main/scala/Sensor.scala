case class Sensor(val id: String, val location: String, val validRange: OperatingRange):
  override def toString(): String =
    s"Id: $id, Location: $location"
