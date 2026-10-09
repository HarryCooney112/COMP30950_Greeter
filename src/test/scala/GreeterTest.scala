
import org.scalatest.funsuite.AnyFunSuite

class GreeterSuite extends AnyFunSuite {
  test("hi returns correct string") {
    val greeter = Greeter("Alex")
    val hiMsg = greeter.hi
    assert(hiMsg === "hi Alex")
  }

  test("bye returns correct string") {
    val greeter = Greeter("Aoife")
    val hiMsg = greeter.bye
    assert(hiMsg === "bye Aoife")
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
}
