package ir

import org.junit.Test
import org.junit.Assert._

import opencl.ir._

class ViewTest {

  @Test
  def test1() {

    val a = new PrimitiveView(new InputAccess("a"))
    val B = new ArrayView(Int, new InputAccess("B"))

    // The map below is not valid, zip on primitives would fail during type-checking
    // map(b => zip(a,b)) o B
    val var_i = new Var("i", RangeUnkown)
    val b = B.access(var_i).asInstanceOf[PrimitiveView] // TODO B.get should return the appropriate type (use generics)
    val zip_ab = new TupleView(TupleType(Int, Int), new TupleCreation(List(a, b)))
    val map_zip_ab = new ArrayView(TupleType(Int, Int), new ArrayCreation(zip_ab, Cst(10), var_i))

    // map (f) o ...
    val var_j = new Var("j", RangeUnkown)
    val mapf = map_zip_ab.access(var_j).asInstanceOf[TupleView]

    val mapf0 = mapf.access(0).asInstanceOf[PrimitiveView]
    val mapf1 = mapf.access(1).asInstanceOf[PrimitiveView]


    assertEquals(Cst(0), ViewPrinter.emit(a))
    assertEquals(var_i, ExprSimplifier.simplify(ViewPrinter.emit(b)))
    assertEquals(Cst(0), ExprSimplifier.simplify(ViewPrinter.emit(mapf0)))
    assertEquals(var_j, ExprSimplifier.simplify(ViewPrinter.emit(mapf1)))
  }

  @Test
  def test2() {

    val A = new ArrayView(new ArrayType(Int, 8), new InputAccess("A"))
    val B = new ArrayView(new ArrayType(Int, 8), new InputAccess("B"))

    //  map(map(map(f))) o map(a => map(b => map(zip(a,b)) o B) o A equivalent to
    // map(a => map(b => map(f) $ zip(a,b)) o B) o A

    // map(a => ... ) $ A
    val var_i = new Var("i", RangeUnkown)
    val a = A.access(var_i).asInstanceOf[ArrayView]
    // ... map(b => ...) $ B ...
    val var_j = new Var("j", RangeUnkown)
    val b = B.access(var_j).asInstanceOf[ArrayView]
    // ... $ zip(a, b) ...
    val zip_ab = new TupleView(TupleType(new ArrayType(Int, 8), new ArrayType(Int, 8)), new TupleCreation(List(a, b)))
    val map_zip_ab = new ArrayView(TupleType(new ArrayType(Int, 8), new ArrayType(Int, 8)), new ArrayCreation(zip_ab, Cst(4), var_j))
    val map_map_zip_ab = new ArrayView(new ArrayType(TupleType(new ArrayType(Int, 8), new ArrayType(Int, 8)),4), new ArrayCreation(map_zip_ab, Cst(4), var_i))

    // ... map(f) $ ...

    // map(map (f)) o ...
    val var_k = new Var("k", RangeUnkown)
    val var_l = new Var("l", RangeUnkown)
    val map_f = map_map_zip_ab.access(var_k).asInstanceOf[ArrayView]
    val map_map_f = map_f.access(var_l).asInstanceOf[TupleView]

    val map_map_f0 = map_map_f.access(0).asInstanceOf[ArrayView]
    val map_map_f1 = map_map_f.access(1).asInstanceOf[ArrayView]

    val map_map_f0_9 = map_map_f0.access(9).asInstanceOf[PrimitiveView]
    val map_map_f1_7 = map_map_f1.access(7).asInstanceOf[PrimitiveView]

    assertEquals(8*var_k + 9, ExprSimplifier.simplify(ViewPrinter.emit(map_map_f0_9)))
    assertEquals(8*var_l + 7, ExprSimplifier.simplify(ViewPrinter.emit(map_map_f1_7)))
  }

  @Test
  def test3() {

    val A = new ArrayView(new ArrayType(Int, 8), new InputAccess("A"))
    val B = new ArrayView(new ArrayType(Int, 8), new InputAccess("B"))

    // map(a => map(b => map(fun(t => Get(t, 0) * Get(t, 1))) o zip(a,b)) o B) o A
    val var_i = new Var("i", RangeUnkown)
    val var_j = new Var("j", RangeUnkown)
    val a = A.access(var_i).asInstanceOf[ArrayView]
    val b = B.access(var_j).asInstanceOf[ArrayView]
    val zip_ab = new TupleView(TupleType(new ArrayType(Int, 8), new ArrayType(Int, 8)), new TupleCreation(List(a, b)))
    val zip_ab0 = zip_ab.access(0).asInstanceOf[ArrayView]
    val zip_ab1 = zip_ab.access(1).asInstanceOf[ArrayView]

    val zip_ab0_3 = zip_ab0.access(3).asInstanceOf[PrimitiveView]
    val zip_ab1_7 = zip_ab1.access(7).asInstanceOf[PrimitiveView]


    assertEquals(8*var_i + 3, ExprSimplifier.simplify(ViewPrinter.emit(zip_ab0_3)))
    assertEquals(8*var_j + 7, ExprSimplifier.simplify(ViewPrinter.emit(zip_ab1_7)))
  }

  @Test
  def testSplit() {

    val A = new ArrayView(new ArrayType(Int, 8), new InputAccess("A"))


    // split-2 o A
    val split2A = A.split(2)
    val var_i = new Var("i", RangeUnkown)
    val var_j = new Var("j", RangeUnkown)

    val split2A_i = split2A.access(var_i).asInstanceOf[ArrayView]
    val split2A_i_j = split2A_i.access(var_j).asInstanceOf[ArrayView]

    val split2A_i_j_7 = split2A_i_j.access(7).asInstanceOf[PrimitiveView]

    assertEquals(ExprSimplifier.simplify(8*(var_i*2 + var_j) + 7), ExprSimplifier.simplify(ViewPrinter.emit(split2A_i_j_7)))
  }

  @Test
  def testReorder() {

    val A = new ArrayView(Int, new InputAccess("A"))

    // reorder o A
    val reorder_A = A.reorder((idx) => 40-idx)

    // split o reorder o A
    val split_reorder_A = reorder_A.split(4)

    val reorder_split_reorder_A_1 = split_reorder_A.access(1).asInstanceOf[ArrayView]
    val reorder_split_reorder_A_1_3 = reorder_split_reorder_A_1.access(3).asInstanceOf[PrimitiveView]

    assertEquals(Cst(33), ExprSimplifier.simplify(ViewPrinter.emit(reorder_split_reorder_A_1_3)))
  }
}
