package ir.view

import ir._
import ir.ast._
import lift.arithmetic._
import opencl.executor.Compile
import core.generator.GenericAST._
import opencl.ir._
import opencl.ir.pattern.{MapGlb, MapSeq, toGlobal}
import org.junit.Assert._
import org.junit.Test

class ViewTest {

  @Test
  def testUserFuns(): Unit = {

    val mapSeqId1 = MapSeq(id)
    val mapSeqId2 = MapSeq(id)

    val f = fun(
      ArrayTypeWSWC( ArrayTypeWSWC(Float, SizeVar("N")), SizeVar("M")),
      input => MapGlb(mapSeqId1 o mapSeqId2) $ input
    )

    Compile(f)
  }

  @Test
  def testMapScatter1(): Unit = {

    val f = fun(
      ArrayTypeWSWC(Float, SizeVar("N")),
      input => Scatter(reverse) o MapSeq(id) $ input
    )

    Compile(f)
  }

  @Test
  def testMapScatter2(): Unit = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float,SizeVar("N")), SizeVar("M")),
      input => MapSeq(Scatter(reverse)) o MapSeq(MapSeq(id)) $ input
    )

    Compile(f)
  }

  @Test
  def testMapScatter3(): Unit = {

    val p = Param(Float)
    val fcId =  FunCall(id, p)
    val lambdaId = Lambda(Array(p), fcId)
    //val idFunCall = fun(x => lambda $ x)

    val N = SizeVar("N")
    val M = SizeVar("M")

    val mapId = MapSeq(lambdaId)
    val mapMapId = MapSeq(mapId)

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M),
      input => toGlobal(MapSeq( fun ( p=> Scatter(reverse) $ p )  ) o mapMapId) $ input
    )

    Compile(f)

    assertEquals(VarIdxRef(f.body.mem.variable, None, Some(ArithExpression(N - 1 -
      mapId.loopVar + N*mapMapId.loopVar))),
                 ViewPrinter.emit(fcId.outputView))
  }

  @Test
  def testMapScatter4(): Unit = {

    val N = SizeVar("N")

    val f = fun(
      ArrayTypeWSWC(Float, N),
      ArrayTypeWSWC(Float, N),
      (in1,in2) => toGlobal(MapSeq(tf_id)) o Scatter(reverse) $ Zip(MapSeq(id) $ in1, MapSeq(id) $ in2)
      )

    Compile(f)
  }


  @Test
  def test1(): Unit = {

    val va = Var("a")
    val vB = Var("B")

    val a = View(ArrayTypeWSWC(Int, 8), va)
    val B = View(ArrayTypeWSWC(ArrayTypeWSWC(Int, 8), 8), vB)

    // map(b => zip(a,b)) o B
    val var_i = Var("i", RangeUnknown)
    val b = B.access(var_i)
    val zip_ab = View.tuple(a, b).zip()
    val map_zip_ab = ViewMap(zip_ab, var_i, ArrayTypeWSWC(ArrayTypeWSWC(TupleType(Int, Int), 8), 8))

    // map(map(f)) o ...
    val var_j = Var("j", RangeUnknown)
    val mapf = map_zip_ab.access(var_j)
    val var_k = SizeVar("K")
    val map_mapf = mapf.access(var_k)

    val map_mapf0 = map_mapf.get(0)
    val map_mapf1 = map_mapf.get(1)

    assertEquals(VarIdxRef(va, None, Some(ArithExpression(var_k))), ViewPrinter
      .emit(map_mapf0))
    assertEquals(VarIdxRef(vB, None, (Some(ArithExpression(var_j*8 + var_k)))),
      ViewPrinter.emit(map_mapf1))
  }

  @Test
  def test2(): Unit = {
    val vA = Var("A")
    val vB = Var("B")
    val A = View(ArrayTypeWSWC(ArrayTypeWSWC(Int, 8), 8), vA)
    val B = View(ArrayTypeWSWC(ArrayTypeWSWC(Int, 8), 8), vB)

    //  map(map(map(f))) o map(a => map(b => zip(a,b) o B) o A equivalent to
    // map(a => map(b => map(f) $ zip(a,b)) o B) o A

    // map(a => ... ) $ A
    val var_i =  Var("i", RangeUnknown)
    val a = A.access(var_i)
    // ... map(b => ...) $ B ...
    val var_j = Var("j", RangeUnknown)
    val b = B.access(var_j)
    // ... $ zip(a, b) ...
    val zip_ab = View.tuple(a, b).zip()
    val map_zip_ab = ViewMap(zip_ab, var_j, ArrayTypeWSWC(ArrayTypeWSWC(TupleType(Int, Int), 8), 8))
    val map_map_zip_ab = ViewMap(map_zip_ab, var_i, ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(TupleType(Int, Int), 8), 8), 8))

    // ... map(f) $ ...

    // map(map (f)) o ...
    val var_k = Var("k", RangeUnknown)
    val var_l = Var("l", RangeUnknown)
    val map_f = map_map_zip_ab.access(var_k)
    val map_map_f = map_f.access(var_l)

    val map_map_map_f = map_map_f.access(9)

    val map_map_map_f0 = map_map_map_f.get(0)
    val map_map_map_f1 = map_map_map_f.get(1)

    assertEquals(VarIdxRef(vA, None, Some(ArithExpression(8*var_k + 9))),
      ViewPrinter.emit(map_map_map_f0))
    assertEquals(VarIdxRef(vB, None, Some(ArithExpression(8*var_l + 9))),
      ViewPrinter.emit(map_map_map_f1))
  }

  @Test
  def test3(): Unit = {
    val vA = Var("A")
    val vB = Var("B")
    val A = View(ArrayTypeWSWC(ArrayTypeWSWC(Int, 8), 8), vA)
    val B = View(ArrayTypeWSWC(ArrayTypeWSWC(Int, 8), 8), vB)

    // map(a => map(b => map(fun(t => Get(t, 0) * Get(t, 1))) o zip(a,b)) o B) o A
    val var_i = Var("i", RangeUnknown)
    val var_j = Var("j", RangeUnknown)
    val a = A.access(var_i)
    val b = B.access(var_j)
    val zip_ab = View.tuple(a, b).zip()

    val zip_ab_3 = zip_ab.access(3)

    val zip_ab_3_0 = zip_ab_3.get(0)
    val zip_ab_3_1 = zip_ab_3.get(1)

    assertEquals(VarIdxRef(vA, None, Some(ArithExpression(8*var_i + 3))),
      ViewPrinter.emit(zip_ab_3_0))
    assertEquals(VarIdxRef(vB, None, Some(ArithExpression(8*var_j + 3))),
      ViewPrinter.emit(zip_ab_3_1))
  }

  @Test
  def testSplit(): Unit = {

    val vA = Var("A")
    val A = View(ArrayTypeWSWC(Int, 8), vA)

    // split-2 o A
    val split2A = A.split(2)
    val var_i = Var("i", RangeUnknown)
    val var_j = Var("j", RangeUnknown)

    val split2A_i = split2A.access(var_i)
    val split2A_i_j = split2A_i.access(var_j)

    assertEquals(VarIdxRef(vA, None, Some(ArithExpression(2*var_i + var_j))),
      ViewPrinter.emit(split2A_i_j))
  }

  @Test
  def testReorder(): Unit = {

    val vA = Var("A")
    val A = View(ArrayTypeWSWC(Int, SizeVar("N")), vA)

    // reorder o A
    val reorder_A = A.reorder((idx) => 40-idx)

    // split o reorder o A
    val split_reorder_A = reorder_A.split(4)

    val reorder_split_reorder_A_1 = split_reorder_A.access(1)
    val reorder_split_reorder_A_1_3 = reorder_split_reorder_A_1.access(3)

    assertEquals(VarIdxRef(vA, None, Some(ArithExpression(Cst(33)))), ViewPrinter
      .emit(reorder_split_reorder_A_1_3))
  }

  @Test
  def transposeWrite(): Unit = {
    // Map(Map(g)) o Split() o Scatter(transpose) o Join() o Map(Map(f))
    // Write view for f
    val N = SizeVar("N")
    val M = SizeVar("M")

    val origArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N)
    val transposedArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M)

    val i = Var("i", ContinuousRange(0, N))
    val j = Var("j", ContinuousRange(0, M))

    val v = Var()

    val goal = View(transposedArray, v).access(j).access(i)

    val reality = View(transposedArray, v).join(N).
      reorder(i => transpose(i, origArray)).split(M).access(i).access(j)

    assertEquals(ViewPrinter.emit(goal), ViewPrinter.emit(reality))
  }

  @Test
  def joinScatter(): Unit = {
    // Map(g) o Scatter() o Join() o Map(Map(f))
    // Write view for f
    val N = SizeVar("N")
    val M = SizeVar("M")

    val origArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N)
    val finalArray = ArrayTypeWSWC(Float, M*N)
    val transposedArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M)

    val i = Var("i", ContinuousRange(0, N))
    val j = Var("j", ContinuousRange(0, M))

    val v = Var()

    val goal = View(transposedArray, v).access(j).access(i)

    val view = View(finalArray, v).
      reorder(i => transpose(i, origArray)).split(M).access(i).access(j)

    assertEquals(ViewPrinter.emit(goal), ViewPrinter.emit(view))
  }

  @Test
  def transposeWriteJoin(): Unit = {
    // Map(f) o Join() o Split() o Scatter(transpose) o Join() o Map(Map(g))
    val N = SizeVar("N")
    val M = SizeVar("M")

    val origArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N)
    val transposedArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M)
    val finalArray = ArrayTypeWSWC(Float, M*N)

    val i = Var("i", ContinuousRange(0, N))
    val j = Var("j", ContinuousRange(0, M))

    val v = Var()

    // Write for g
    val goal = View(transposedArray, v).access(j).access(i)

    val view = View(finalArray, v).
      split(N).join(N).reorder(i => transpose(i, origArray)).
      split(M).access(i).access(j)

    assertEquals(ViewPrinter.emit(goal), ViewPrinter.emit(view))
  }

  @Test
  def transposeWriteSplitJoin(): Unit = {
    // Map(Map(f)) o Split() o Join() o Split() o Scatter(transpose) o Join() o Map(Map(g))
    val N = SizeVar("N")
    val M = SizeVar("M")

    val origArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N)
    val transposedArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M)
    val finalArray = ArrayTypeWSWC(ArrayTypeWSWC(Float, N), M)

    val i = Var("i", ContinuousRange(0, N))
    val j = Var("j", ContinuousRange(0, M))

    val v = Var()

    // Write for g
    val goal = View(transposedArray, v).access(j).access(i)

    val view = View(finalArray, v).
      join(N).split(N).join(N).reorder(i => transpose(i, origArray)).
      split(M).access(i).access(j)

    val accGoal = ViewPrinter.emit(goal)
    val accView = ViewPrinter.emit(view)
    assertEquals(accGoal, accView)
  }

  @Test
  def twiceTransposeWrite(): Unit = {
    // Split() o Scatter(transpose) o Join() o Map(Split() o
    // Scatter(transpose) o Join() o Map(Map(f)))
    val N = SizeVar("N")
    val M = SizeVar("M")
    val L = SizeVar("L")

    val i = Var("i", ContinuousRange(0, N))
    val j = Var("j", ContinuousRange(0, M))
    val k = Var("k", ContinuousRange(0, L))

    // origArray = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, L), M), N)
    val middleArray = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), L), N)
    val finalArray = ArrayTypeWSWC(ArrayTypeWSWC(ArrayTypeWSWC(Float, M), N), L)

    val v = Var()

    val goal = View(finalArray, v).access(k).access(i).access(j)

    val midGoal = View(middleArray, v).access(i).access(k).access(j)

    val midPoint = View(middleArray, v).access(i).join(M).
      reorder(i => transpose(i, ArrayTypeWSWC(ArrayTypeWSWC(Float, L), M))).split(L).
      access(j).access(k)

    val view = View(finalArray, v).join(N).
      reorder(i => transpose(i, middleArray)).split(L).access(i).
      join(M).reorder(i => transpose(i, ArrayTypeWSWC(ArrayTypeWSWC(Float, L), M))).split(L).
      access(j).access(k)

    assertEquals(ViewPrinter.emit(midGoal), ViewPrinter.emit(midPoint))
    assertEquals(ViewPrinter.emit(goal),    ViewPrinter.emit(view))

  }
}
