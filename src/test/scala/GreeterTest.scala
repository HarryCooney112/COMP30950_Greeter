
import org.scalatest.funsuite.AnyFunSuite

class GreeterSuite extends AnyFunSuite {

  test("Reading toString returns correct string") {
    val s1 = Sensor("123", "London", OperatingRange(1, 5))
    val reading = Reading(s1.id, 10, 3)
    assert(reading.toString() == "Sensor 123: Logged at 10, Reading: 3.0")
  }

  test("Sensor is not registered") {
    val s1 = Sensor("1235", "Dublin", OperatingRange(2, 6))
    var network = SensorNetworkManager()
    assert(!network.sensorMap.contains(s1.id))
  }

  test("Sensor is registered") {
    val s1 = Sensor("1234", "Dublin", OperatingRange(2, 6))
    var network = SensorNetworkManager()
    assert(!network.sensorMap.contains(s1.id))
    network.registerSensor(s1)
    assert(network.sensorMap.contains(s1.id))
  }

  test("Reading maps to corresponding sensor") {
    val s1 = Sensor("1235", "Dublin", OperatingRange(2, 6))
    var network = SensorNetworkManager()
    network.registerSensor(s1)

    val reading = Reading(s1.id, 3, 10)
    network.addReading(reading)

    val logs = network.logsFor("1235")
    logs match
      case Some(l) => assert(l(0) == "Sensor 1235: Logged at 3, Reading: 10.0")
      case None => false
  }

  test("forLogs returns None if sensor is not in system") {
    var network = SensorNetworkManager()
    val logs = network.logsFor("1235")
    assert(logs == None)
  }

  test("Alerts collects alerts properly") {
    var network = SensorNetworkManager()
    val s1 = Sensor("123", "Dublin", OperatingRange(1, 5))
    network.registerSensor(s1)

    val readings = Array(
      Reading(s1.id, 3, 10), //Out of range
      Reading(s1.id, 3, 3),
      Reading(s1.id, 3, 2),
      Reading(s1.id, 3, -1), //Out of range
      Reading(s1.id, 2, 7) //Out of range
    )

    readings.foreach(network.addReading(_))
    val alerts = network.alertsFor("123")
    assert(alerts(0).reason == "Value 10.0 out of range [1.0, 5.0]" && alerts(0).reading == readings(0))
    assert(alerts(1).reason == "Value -1.0 out of range [1.0, 5.0]" && alerts(1).reading == readings(3))
    assert(alerts(2).reason == "Value 7.0 out of range [1.0, 5.0]" && alerts(2).reading == readings(4))
  }
}
