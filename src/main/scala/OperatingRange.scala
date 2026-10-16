case class OperatingRange(val min: Double, val max: Double):
  override def toString(): String =
    s"[$min, $max]"
