package exploration

import org.junit.Assert._
import org.junit.Test


class TestUtils {
  @Test def testListPossibilities() {

    val oriList = List(1, 2, 3, 4)
    val optionsList = List(List(), List(5, 6), List(7, 8), List(9))

    val testResults = Utils.listPossiblities(oriList, optionsList)
    val expectedResults = List(List(1, 5, 3, 4), List(1, 6, 3, 4), List(1, 2, 7, 4), List(1, 2, 8, 4), List(1, 2, 3, 9))

    assertEquals(testResults, expectedResults)
  }
}
