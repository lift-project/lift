package exploration.detection

import exploration.MemoryMappingRewrite
import exploration.detection.DetectReuseAcrossThreads._
import org.junit.Assert._
import org.junit.Test
import rewriting.rules.OpenCLRules

class TestDetectReuseAcrossThreads {

  @Test
  def partialReduceWithReorderNoRace(): Unit = {
    val locations = findStrategicLocations(gemvPartialReduceWithReorderNoRace)
    assertEquals(0, locations.length)
  }

  @Test
  def introduceReuse(): Unit = {
    val locations = findStrategicLocations(gemvIntroduceReuse)
    assertEquals(1, locations.length)
  }

  @Test
  def mmPlainTiling(): Unit = {
    val locations = findStrategicLocations(mmTiled)
    assertEquals(3, locations.length)
  }

  @Test
  def mmTATiled1DBlocked(): Unit = {
    val locations = findStrategicLocations(mmTiled1DBlocked)
    assertEquals(3, locations.length)
  }

  @Test
  def mmTATiled2DBlocked(): Unit = {
    val locations = findStrategicLocations(mmTiled2DBlocked)
    assertEquals(3, locations.length)
  }

  @Test
  def reuseMappingCount(): Unit = {
    val f = mmTiled2DBlocked
    val strategicLocationsMarked =
      MemoryMappingRewrite.addIdsForPrivate(MemoryMappingRewrite.addIdsForLocal(f))

    val blabla = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
      DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
      distinct.
      filter(x => OpenCLRules.localMemory.isDefinedAt(x._1))

    val lambdas = ImplementReuse(strategicLocationsMarked, blabla, OpenCLRules.localMemory)

    val cleanedLambdas = lambdas.map(MemoryMappingRewrite.cleanup) :+ f


    val cleanedWithPrivate = cleanedLambdas.flatMap(lowered => {
      val strategicLocationsMarked =
        MemoryMappingRewrite.addIdsForPrivate(lowered)

      val blabla = (DetectReuseWithinThread.getCandidates(strategicLocationsMarked) ++
        DetectReuseAcrossThreads.getCandidates(strategicLocationsMarked)).
        distinct.
        filter(x => OpenCLRules.privateMemory.isDefinedAt(x._1))

      val lambdas = ImplementReuse(strategicLocationsMarked, blabla, OpenCLRules.privateMemory)

      lambdas.map(MemoryMappingRewrite.cleanup)
    }) :+ f

    assertTrue(cleanedWithPrivate.length > 1)
  }
}
