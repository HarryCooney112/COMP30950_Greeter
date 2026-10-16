case class Alert(val reading: Reading, val reason: String):
  override def toString(): String =
    s"${reading.value} $reason"