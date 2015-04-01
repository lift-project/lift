package ir

import org.junit.Test
import org.junit.Assert._

import opencl.ir._

class ViewTest {

  @Test
  def test1() {

    val a = View(ArrayType(Int, 8), "a")
    val B = View(ArrayType(ArrayType(Int, 8), 8), "B")

    // map(b => zip(a,b)) o B
    val var_i = new Var("i", RangeUnknown)
    val b = B.access(var_i)
    val zip_ab = View.tuple(a, b).zip()
    val map_zip_ab = new ViewMap(zip_ab, var_i, ArrayType(ArrayType(TupleType(Int, Int), 8), 8))

    // map(map(f)) o ...
    val var_j = new Var("j", RangeUnknown)
    val mapf = map_zip_ab.access(var_j)
    val var_k = new Var("k")
    val map_mapf = mapf.access(var_k)

    val map_mapf0 = map_mapf.get(0)
    val map_mapf1 = map_mapf.get(1)

    assertEquals(var_k, ExprSimplifier.simplify(ViewPrinter.emit(map_mapf0)))
    assertEquals(var_j*8 + var_k, ExprSimplifier.simplify(ViewPrinter.emit(map_mapf1)))
  }

  @Test
  def test2() {
    val A = View(ArrayType(ArrayType(Int, 8), 8), "A")
    val B = View(ArrayType(ArrayType(Int, 8), 8), "B")

    //  map(map(map(f))) o map(a => map(b => zip(a,b) o B) o A equivalent to
    // map(a => map(b => map(f) $ zip(a,b)) o B) o A

    // map(a => ... ) $ A
    val var_i = new Var("i", RangeUnknown)
    val a = A.access(var_i)
    // ... map(b => ...) $ B ...
    val var_j = new Var("j", RangeUnknown)
    val b = B.access(var_j)
    // ... $ zip(a, b) ...
    val zip_ab = View.tuple(a, b).zip()
    val map_zip_ab = new ViewMap(zip_ab, var_j, ArrayType(ArrayType(TupleType(Int, Int), 8), 8))
    val map_map_zip_ab = new ViewMap(map_zip_ab, var_i, ArrayType(ArrayType(ArrayType(TupleType(Int, Int), 8), 8), 8))

    // ... map(f) $ ...

    // map(map (f)) o ...
    val var_k = new Var("k", RangeUnknown)
    val var_l = new Var("l", RangeUnknown)
    val map_f = map_map_zip_ab.access(var_k)
    val map_map_f = map_f.access(var_l)

    val map_map_map_f = map_map_f.access(9)

    val map_map_map_f0 = map_map_map_f.get(0)
    val map_map_map_f1 = map_map_map_f.get(1)

    assertEquals(8*var_k + 9, ExprSimplifier.simplify(ViewPrinter.emit(map_map_map_f0)))
    assertEquals(8*var_l + 9, ExprSimplifier.simplify(ViewPrinter.emit(map_map_map_f1)))
  }

  @Test
  def test3() {
    val A = View(ArrayType(ArrayType(Int, 8), 8), "A")
    val B = View(ArrayType(ArrayType(Int, 8), 8), "B")

    // map(a => map(b => map(fun(t => Get(t, 0) * Get(t, 1))) o zip(a,b)) o B) o A
    val var_i = new Var("i", RangeUnknown)
    val var_j = new Var("j", RangeUnknown)
    val a = A.access(var_i)
    val b = B.access(var_j)
    val zip_ab = View.tuple(a, b).zip()

    val zip_ab_3 = zip_ab.access(3)

    val zip_ab_3_0 = zip_ab_3.get(0)
    val zip_ab_3_1 = zip_ab_3.get(1)


    assertEquals(8*var_i + 3, ExprSimplifier.simplify(ViewPrinter.emit(zip_ab_3_0)))
    assertEquals(8*var_j + 3, ExprSimplifier.simplify(ViewPrinter.emit(zip_ab_3_1)))
  }

  @Test
  def testSplit() {

    val A = View(ArrayType(Int, 8), "A")

    // split-2 o A
    val split2A = A.split(2)
    val var_i = new Var("i", RangeUnknown)
    val var_j = new Var("j", RangeUnknown)

    val split2A_i = split2A.access(var_i)
    val split2A_i_j = split2A_i.access(var_j)

    assertEquals(ExprSimplifier.simplify(2*var_i + var_j), ExprSimplifier.simplify(ViewPrinter.emit(split2A_i_j)))
  }

  @Test
  def testReorder() {

    val A = View(ArrayType(Int, new Var("N")), "A")

    // reorder o A
    val reorder_A = A.reorder((idx) => 40-idx)

    // split o reorder o A
    val split_reorder_A = reorder_A.split(4)

    val reorder_split_reorder_A_1 = split_reorder_A.access(1)
    val reorder_split_reorder_A_1_3 = reorder_split_reorder_A_1.access(3)

    assertEquals(Cst(33), ExprSimplifier.simplify(ViewPrinter.emit(reorder_split_reorder_A_1_3)))
  }

  @Test
  def transposeWrite(): Unit = {
    // Map(Map(g)) o Split() o Scatter(IndexFunction.transpose) o Join() o Map(Map(f))
    // Write view for f
    val N = new Var("N")
    val M = new Var("M")

    val origArray = ArrayType(ArrayType(Float, M), N)
    val transposedArray = ArrayType(ArrayType(Float, N), M)

    val i = new Var("i", ContinuousRange(0, N))
    val j = new Var("j", ContinuousRange(0, M))

    val goal = View(transposedArray, "").access(j).access(i)

    val reality = View(transposedArray, "").join(N).
      reorder(i => IndexFunction.transpose(i, origArray)).split(M).access(i).access(j)

    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(goal)),
      ExprSimplifier.simplify(ViewPrinter.emit(reality)))
  }

  @Test
  def joinScatter(): Unit = {
    // Map(g) o Scatter() o Join() o Map(Map(f))
    // Write view for f
    val N = new Var("N")
    val M = new Var("M")

    val origArray = ArrayType(ArrayType(Float, M), N)
    val finalArray = ArrayType(Float, M*N)
    val transposedArray = ArrayType(ArrayType(Float, N), M)

    val i = new Var("i", ContinuousRange(0, N))
    val j = new Var("j", ContinuousRange(0, M))

    val goal = View(transposedArray, "").access(j).access(i)

    val view = View(finalArray, "").
      reorder(i => IndexFunction.transpose(i, origArray)).split(M).access(i).access(j)

    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(goal)),
      ExprSimplifier.simplify(ViewPrinter.emit(view)))
  }

  @Test
  def transposeWriteJoin(): Unit = {
    // Map(f) o Join() o Split() o Scatter(IndexFunction.transpose) o Join() o Map(Map(g))
    val N = new Var("N")
    val M = new Var("M")

    val origArray = ArrayType(ArrayType(Float, M), N)
    val transposedArray = ArrayType(ArrayType(Float, N), M)
    val finalArray = ArrayType(Float, M*N)

    val i = new Var("i", ContinuousRange(0, N))
    val j = new Var("j", ContinuousRange(0, M))

    // Write for g
    val goal = View(transposedArray, "").access(j).access(i)

    val view = View(finalArray, "").
      split(N).join(N).reorder(i => IndexFunction.transpose(i, origArray)).
      split(M).access(i).access(j)

    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(goal)),
      ExprSimplifier.simplify(ViewPrinter.emit(view)))
  }

  @Test
  def transposeWriteSplitJoin(): Unit = {
    // Map(Map(f)) o Split() o Join() o Split() o Scatter(IndexFunction.transpose) o Join() o Map(Map(g))
    val N = new Var("N")
    val M = new Var("M")

    val origArray = ArrayType(ArrayType(Float, M), N)
    val transposedArray = ArrayType(ArrayType(Float, N), M)
    val finalArray = ArrayType(ArrayType(Float, N), M)

    val i = new Var("i", ContinuousRange(0, N))
    val j = new Var("j", ContinuousRange(0, M))

    // Write for g
    val goal = View(transposedArray, "").access(j).access(i)

    val view = View(finalArray, "").
      join(N).split(N).join(N).reorder(i => IndexFunction.transpose(i, origArray)).
      split(M).access(i).access(j)

    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(goal)),
      ExprSimplifier.simplify(ViewPrinter.emit(view)))
  }

  @Test
  def twiceTransposeWrite(): Unit = {
    // Split() o Scatter(IndexFunction.transpose) o Join() o Map(Split() o
    // Scatter(IndexFunction.transpose) o Join() o Map(Map(f)))
    val N = new Var("N")
    val M = new Var("M")
    val L = new Var("L")

    val i = new Var("i", ContinuousRange(0, N))
    val j = new Var("j", ContinuousRange(0, M))
    val k = new Var("k", ContinuousRange(0, L))

    // origArray = ArrayType(ArrayType(ArrayType(Float, L), M), N)
    val middleArray = ArrayType(ArrayType(ArrayType(Float, M), L), N)
    val finalArray = ArrayType(ArrayType(ArrayType(Float, M), N), L)

    val goal = View(finalArray, "").access(k).access(i).access(j)

    val midGoal = View(middleArray, "").access(i).access(k).access(j)

    val midPoint = View(middleArray, "").access(i).join(M).
      reorder(i => IndexFunction.transpose(i, ArrayType(ArrayType(Float, L), M))).split(L).
      access(j).access(k)

    val view = View(finalArray, "").join(N).
      reorder(i => IndexFunction.transpose(i, middleArray)).split(L).access(i).
      join(M).reorder(i => IndexFunction.transpose(i, ArrayType(ArrayType(Float, L), M))).split(L).
      access(j).access(k)

    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(midGoal)),
      ExprSimplifier.simplify(ViewPrinter.emit(midPoint)))
    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(goal)),
      ExprSimplifier.simplify(ViewPrinter.emit(view)))

  }
}
