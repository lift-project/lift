package ir

import org.junit.{Ignore, Test}
import org.junit.Assert._

import opencl.ir._

class ViewTest {

  @Test
  @Ignore
  def test1() {

    val a = InputView(Int, "a")
    val B = InputView(ArrayType(Int, 8), "B")

    // TODO: Remove? Explanation below
    // The map below is not valid, zip on primitives would fail during type-checking
    // map(b => zip(a,b)) o B
    val var_i = new Var("i", RangeUnknown)
    val b = B.access(var_i)
    val zip_ab = InputView.zip(a, b)
    val map_zip_ab = new InputViewMap(zip_ab.access(var_i), var_i, TupleType(Int, Int))

    // map (f) o ...
    val var_j = new Var("j", RangeUnknown)
    val mapf = map_zip_ab.access(var_j)

    val mapf0 = mapf.get(0)
    val mapf1 = mapf.get(1)


    assertEquals(Cst(0), ViewPrinter.emit(a))
    assertEquals(var_i, ExprSimplifier.simplify(ViewPrinter.emit(b)))
    assertEquals(Cst(0), ExprSimplifier.simplify(ViewPrinter.emit(mapf0)))
    assertEquals(var_j, ExprSimplifier.simplify(ViewPrinter.emit(mapf1)))
  }

  @Test
  def test2() {

    val A = InputView(ArrayType(ArrayType(Int, 8), 8), "A")
    val B = InputView(ArrayType(ArrayType(Int, 8), 8), "B")

    //  map(map(map(f))) o map(a => map(b => map(zip(a,b)) o B) o A equivalent to
    // map(a => map(b => map(f) $ zip(a,b)) o B) o A

    // map(a => ... ) $ A
    val var_i = new Var("i", RangeUnknown)
    val a = A.access(var_i)
    // ... map(b => ...) $ B ...
    val var_j = new Var("j", RangeUnknown)
    val b = B.access(var_j)
    // ... $ zip(a, b) ...
    val zip_ab = InputView.zip(a, b)
    val map_zip_ab = new InputViewMap(zip_ab, var_i, ArrayType(TupleType(Int, Int), 8))
    val map_map_zip_ab = new InputViewMap(map_zip_ab, var_j, ArrayType(ArrayType(TupleType(Int, Int), 8), 8))

    // ... map(f) $ ...

    // map(map (f)) o ...
    val var_k = new Var("k", RangeUnknown)
    val var_l = new Var("l", RangeUnknown)
    val map_f = map_map_zip_ab.access(var_k)
    val map_map_f = map_f.access(var_l)

    val map_map_f0 = map_map_f.get(0)
    val map_map_f1 = map_map_f.get(1)

    val map_map_f0_9 = map_map_f0.access(9)
    val map_map_f1_7 = map_map_f1.access(7)

    assertEquals(8*var_k + 9, ExprSimplifier.simplify(ViewPrinter.emit(map_map_f0_9)))
    assertEquals(8*var_l + 7, ExprSimplifier.simplify(ViewPrinter.emit(map_map_f1_7)))
  }

  @Test
  def test3() {

    val A = InputView(ArrayType(ArrayType(Int, 8), 8), "A")
    val B = InputView(ArrayType(ArrayType(Int, 8), 8), "B")

    // map(a => map(b => map(fun(t => Get(t, 0) * Get(t, 1))) o zip(a,b)) o B) o A
    val var_i = new Var("i", RangeUnknown)
    val var_j = new Var("j", RangeUnknown)
    val a = A.access(var_i)
    val b = B.access(var_j)
    val zip_ab = InputView.zip(a, b)
    val zip_ab0 = zip_ab.get(0)
    val zip_ab1 = zip_ab.get(1)

    val zip_ab0_3 = zip_ab0.access(3)
    val zip_ab1_7 = zip_ab1.access(7)


    assertEquals(8*var_i + 3, ExprSimplifier.simplify(ViewPrinter.emit(zip_ab0_3)))
    assertEquals(8*var_j + 7, ExprSimplifier.simplify(ViewPrinter.emit(zip_ab1_7)))
  }

  @Test
  def testSplit() {

    val A = InputView(ArrayType(Int, 8), "A")

    // split-2 o A
    val split2A = A.split(2)
    val var_i = new Var("i", RangeUnknown)
    val var_j = new Var("j", RangeUnknown)

    val split2A_i = split2A.access(var_i)
    val split2A_i_j = split2A_i.access(var_j)

    assertEquals(ExprSimplifier.simplify(4*var_i + var_j), ExprSimplifier.simplify(ViewPrinter.emit(split2A_i_j)))
  }

  @Test
  def testReorder() {

    val A = InputView(ArrayType(Int, new Var("N")), "A")

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

    val goal = InputView(transposedArray, "").access(j).access(i)

    val reality = InputView(transposedArray, "").join(N).
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

    val goal = InputView(transposedArray, "").access(j).access(i)

    val view = InputView(finalArray, "").
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
    val goal = InputView(transposedArray, "").access(j).access(i)

    val view = InputView(finalArray, "").
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
    val goal = InputView(transposedArray, "").access(j).access(i)

    val view = InputView(finalArray, "").
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

    val goal = InputView(finalArray, "").access(k).access(i).access(j)

    val midGoal = InputView(middleArray, "").access(i).access(k).access(j)

    val midPoint = InputView(middleArray, "").access(i).join(M).
      reorder(i => IndexFunction.transpose(i, ArrayType(ArrayType(Float, L), M))).split(L).
      access(j).access(k)

    val view = InputView(finalArray, "").join(N).
      reorder(i => IndexFunction.transpose(i, middleArray)).split(L).access(i).
      join(M).reorder(i => IndexFunction.transpose(i, ArrayType(ArrayType(Float, L), M))).split(L).
      access(j).access(k)

    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(midGoal)),
      ExprSimplifier.simplify(ViewPrinter.emit(midPoint)))
    assertEquals(ExprSimplifier.simplify(ViewPrinter.emit(goal)),
      ExprSimplifier.simplify(ViewPrinter.emit(view)))

  }
}
