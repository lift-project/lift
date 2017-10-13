package exploration.detection

import exploration.detection.DetectReuseWithinThread._
import org.junit.Assert._
import org.junit.Test

class TestDetectReuseWithinThread {

  @Test
  def partialReduceWithReorderNoRace(): Unit = {
    val locations = findStrategicLocations(gemvPartialReduceWithReorderNoRace)
    assertEquals(0, locations.length)
  }

  @Test
  def introduceReuse(): Unit = {
    val locations = findStrategicLocations(gemvIntroduceReuse)
    assertEquals(0, locations.length)
  }

  @Test
  def oneDRegBlock(): Unit = {
    val locations = findStrategicLocations(mm1DBlocked)
    assertEquals(1, locations.length)
  }

  @Test
  def twoDRegBlock(): Unit = {
    val locations = findStrategicLocations(mm2DBlocked)
    assertEquals(4, locations.length)
  }

  @Test
  def plainTiling(): Unit = {
    val locations = findStrategicLocations(mmTiled)
    assertEquals(0, locations.length)
  }

  @Test
  def tilingAndOneDRegBlock(): Unit = {
    val locations = findStrategicLocations(mmTiled1DBlocked)
    assertEquals(1, locations.length)
  }

  @Test
  def tilingAndTwoDRegBlock(): Unit = {
    val locations = findStrategicLocations(mmTiled2DBlocked)
    assertEquals(4, locations.length)
  }
}
