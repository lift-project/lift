package rewriting.rules

import org.junit.Test
import org.junit.Assert._

class TestRuleIdeas {

  private val size = 128
  private val a = Array.tabulate(size)(_ => util.Random.nextInt(5))
  private val b = Array.tabulate(size)(_ => util.Random.nextInt(5))

  private val A = Array.tabulate(size, size)((x, y) => x*size + y)
  private val B = Array.fill(size, size, size)(util.Random.nextInt(size))

  @Test
  def moveSplitFromZip(): Unit = {
    // Split $ Zip( ... ) => Zip( Split $ ... )
    val gold = (a, b).zipped.map(_ * _)
    val test = (a, b).zipped.toArray.grouped(16).map(_.map(x => x._1 * x._2)).flatten.toArray
    val test2 = (a.grouped(16).toArray, b.grouped(16).toArray).zipped.map((x, y) => (x, y).zipped.map(_ * _)).flatten

    assertArrayEquals(gold, test)
    assertArrayEquals(gold, test2)
  }

  @Test
  def reduceToPartialReduce(): Unit = {
    // Reduce => Reduce() o Reduce( ... $ Zip( ... ) )
    val gold2 = a.sum
    val test4 = a.grouped(16).toArray.reduce((x, y) => (x, y).zipped.map(_ + _)).sum

    assertEquals(gold2, test4, 0.0f)
  }

  @Test
  def moveReorderFromZip(): Unit = {
    // Reorder $ Zip( ... ) => Zip( Reorder $ ... )
    val goldReorderZip = (a, b).zipped.toArray.reverse
    val testReorderZip = (a.reverse, b.reverse).zipped.toArray

    assertArrayEquals(goldReorderZip.map(_._1), testReorderZip.map(_._1))
    assertArrayEquals(goldReorderZip.map(_._2), testReorderZip.map(_._2))
  }

  @Test
  def mapReduceInterchange(): Unit = {
    // Map-Reduce interchange
    val goldSwapMapReduce = A.map(row => Array(row.sum))
    val testSwapMapReduce = Array(A.transpose.reduce((x, y) => (x, y).zipped.map(_ + _))).transpose

    assertArrayEquals(goldSwapMapReduce.flatten, testSwapMapReduce.flatten)
  }

  @Test
  def mapMapInterchangeZipInside(): Unit = {
    // Map-Map transpose, pulling zip out
    val goldMapMapPullZip = A.map(a => (a, b).zipped.map(_ * _))
    val testMapMapPullZip = (A.transpose, b).zipped.map((a, bElem) => a.map(_ * bElem)).transpose

    assertArrayEquals(goldMapMapPullZip.flatten, testMapMapPullZip.flatten)
  }

  @Test
  def mapMapInterchangeZipOutside(): Unit = {
    // Map-Map transpose, pushing zip in
    val goldMapMapPushZip = (A, b).zipped.map((a, bElem) => a.map(_ * bElem))
    val testMapMapPushZip = A.transpose.map(a => (a, b).zipped.map(_ * _)).transpose

    assertArrayEquals(goldMapMapPushZip.flatten, testMapMapPushZip.flatten)
  }

  @Test
  def moveSplitOverTranspose(): Unit = {
    // map(split) o transpose => transpose o map(transpose) o split
    val goldMapSplitTranspose = A.transpose.map(_.grouped(16).toArray)
    val testMapSplitTranspose = A.grouped(16).toArray.map(_.transpose).transpose

    assertArrayEquals(goldMapSplitTranspose.flatten.flatten, testMapSplitTranspose.flatten.flatten)
  }

  @Test
  def moveSplitOverTranspose2(): Unit = {
    // split o transpose => map(transpose) o transpose o map(split)
    val miscGold3 = A.transpose.grouped(16).toArray
    val miscTest3 = A.map(_.grouped(16).toArray).transpose.map(_.transpose)

    assertArrayEquals(miscGold3.flatten.flatten, miscTest3.flatten.flatten)
  }

  @Test
  def moveTransposeOverSplit(): Unit = {
    // map(transpose) o split =>  transpose o map(split) o transpose
    val miscGold = A.grouped(16).toArray.map(_.transpose)
    val miscTest = A.transpose.map(_.grouped(16).toArray).transpose

    assertArrayEquals(miscGold.flatten.flatten, miscTest.flatten.flatten)
  }

  @Test
  def moveTransposeOverSplit2(): Unit = {
    // transpose o map(split) => map(transpose) o split o transpose
    val miscGold2 = A.map(_.grouped(16).toArray).transpose
    val miscTest2 = A.transpose.grouped(16).toArray.map(_.transpose)

    assertArrayEquals(miscGold2.flatten.flatten, miscTest2.flatten.flatten)
  }

  @Test
  def moveSplit(): Unit = {
    // macro rule join-split and split-join-id
    // split o map(split()) => map(map(split)) o split()
    val miscGold4 = A.map(_.grouped(16).toArray).grouped(16).toArray
    val miscTest4 = A.grouped(16).toArray.map(_.map(_.grouped(16).toArray))

    assertArrayEquals(miscGold4.flatten.flatten.flatten, miscTest4.flatten.flatten.flatten)
  }

  @Test
  def transposeBothSides(): Unit = {
    val gold5 = B.map(_.map(_.grouped(16).toArray.map(_.sum).sum))
    val test5 = B.transpose.map(_.map(_.grouped(16).toArray.map(_.sum).sum)).transpose

    assertArrayEquals(gold5.flatten, test5.flatten)
  }

  @Test
  def reorganiseTransposes(): Unit = {
    // map(transpose) o transpose o map(transpose) == transpose o map(transpose) o transpose
    val gold6 = B.transpose.map(_.transpose).transpose
    val test6 = B.map(_.transpose).transpose.map(_.transpose)

    assertArrayEquals(gold6.flatten.flatten, test6.flatten.flatten)
  }

  @Test
  def mapReducePartialReduce(): Unit = {
    // map(reduce(f, init) o join o map(reduce(f, init2)) =>
    // reduce(acc, a => map(acc, a => reduce(f, acc) $ a ) o zip(acc, a) , array(init)) o transpose
    val gold7 = B.map(_.map(_.sum).sum)
    val misc7 = B.transpose.foldLeft(Array.fill(size)(0))((acc, a) => (acc, a).zipped.map((acc, a) => a.foldLeft(acc)((a, b) => a+b)))

    assertArrayEquals(gold7, misc7)
  }

  @Test
  def moveReorderOverTranspose(): Unit = {
    val gold8 = A.reverse.transpose
    val test8 = A.transpose.map(_.reverse)

    assertArrayEquals(gold8.flatten, test8.flatten)
  }

  @Test
  def moveReorderOverSeveralTransposes(): Unit = {
    val gold9 = B.transpose.map(_.transpose.reverse)
    val test9 = B.map(_.map(_.reverse)).transpose.map(_.transpose)

    assertArrayEquals(gold9.flatten.flatten, test9.flatten.flatten)
  }

  //noinspection MapFlatten
  @Test
  def slideTilingMaybe(): Unit = {
    // slide(n, s) => join() o map(slide(n, s)) o slide(u, v)
    val slideGold = a.sliding(3,1).toArray
    val slideTest = a.sliding(5,3).toArray.map(_.sliding(3,1).toArray).flatten

    assertArrayEquals(slideGold.flatten, slideTest.flatten)
  }

  @Test
  def moveJoin(): Unit = {
    // map(id) o join => join o map(map(id))
    val gold10 = A.flatten.map(x => x)
    val test10 = A.flatMap(_.map(x => x))

    assertArrayEquals(gold10, test10)

    // split o map(transpose) =>
    // transpose o split =>
  }
}
