package opencl

import _root_.ir.{ScalarType, TupleType, Type}
import _root_.ir.ast.{UserFun, Value}
import lift.arithmetic.ArithExpr

import scala.language.implicitConversions

package object ir {
  // Generic user functions
  
  def id(ty: Type, name: String = "id"): UserFun =
    UserFun(name, "x", "return x;", ty, ty)
  
  def add(ty: ScalarType, name: String = "add"): UserFun =
    UserFun(name, Array("x", "y"), "return x + y;", Seq(ty, ty), ty)
  
  def equality(ty: Type, name: String = "equality"): UserFun =
    UserFun(name, Array("x", "y"), "return x == y;", Seq(ty, ty), Bool)
  
  def first(leftTy: Type, rightTy: Type, name: String="fst"): UserFun =
    UserFun(name, Array("x", "y"), "return x;", Seq(leftTy, rightTy), leftTy)
  
  def max(ty: ScalarType, name: String="maximum"): UserFun =
    UserFun(name, Array("x", "y"), "return x > y ? x : y;", Seq(ty, ty), ty)

  // commonly used user functions

  val id = UserFun("id", "x", "{ return x; }", Float, Float).
    setScalaFun(x => x.head)

  val t_id = UserFun("tuple_id", "x", "return x;", TupleType(Int, Int), TupleType(Int, Int))
  val tf_id = UserFun("tuple_id", "x", "return x;", TupleType(Float, Float), TupleType(Float, Float))
  val tf4_id = UserFun("tuple4_id", "x", "return x;", TupleType(Float,Float,Float, Float), TupleType(Float, Float,Float,Float))
  val i_id = UserFun("int_id", "x", "return x;", Int, Int)
  val int_add = UserFun("int_add", Array("a", "b"), "return a+b;", Array(Int, Int), Int)

  val idI = UserFun("id", "x", "{ return x; }", Int, Int)

  val idB = UserFun("id", "x", "{ return x; }", Bool, Bool)

  val idfloat = UserFun("idfloat", "x", "{ return x; }", Float, Float)

  val iddouble = UserFun("iddouble", "x", "{ return x; }", Double, Double)

  val idF4 = UserFun("idF4", "x", "{ return x; }", Float4, Float4)
  
  val idFI = UserFun("id", "x", "{ return x; }", TupleType(Float, Int), TupleType(Float, Int))

  val idFF = UserFun("idFF", "x", "{ return x; }", TupleType(Float, Float), TupleType(Float, Float))
  
  val absAndSumUp = UserFun("absAndSumUp", Array("acc", "x"), "{ return acc + fabs(x); }",
                            Seq(Float, Float), Float)

  val sumUp = UserFun("sumUp", Array("acc", "x"), "{ return acc + x; }",
    Seq(Float, Float), Float)

  val subtractUp = UserFun("subtractUp", Array("acc", "x"), "{ return acc - x; }",
    Seq(Float, Float), Float)

  val add = UserFun("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float).
    setScalaFun( xs => xs.head.asInstanceOf[Float] + xs(1).asInstanceOf[Float] )

  val addF4 = UserFun("addF4", Array("x", "y"), "{ return x+y; }", Seq(Float4, Float4), Float4)

  val addI = UserFun("add", Array("x", "y"), "{ return x+y; }", Seq(Int, Int), Int).
    setScalaFun( xs => xs.head.asInstanceOf[Int] + xs(1).asInstanceOf[Int] )

  val adddouble = UserFun("adddouble", Array("x", "y"), "{ return x+y; }", Seq(Double, Double), Double)

  val plusOne = UserFun("plusOne", "x", "{ return x+1; }", Float, Float).
    setScalaFun( xs => xs.head.asInstanceOf[Float] + 1.0f )

  val doubleItAndSumUp = UserFun("doubleItAndSumUp", Array("x", "y"), "{ return x + (y * y); }",
                                 Seq(Float, Float), Float)

  val sqrtIt = UserFun("sqrtIt", "x", "{ return sqrt(x); }", Float, Float)

  val square = UserFun("square", "x", "{ return x*x; }", Float, Float)

  val abs = UserFun("abs", "x", "{ return x >= 0 ? x : -x; }", Float, Float)

  val neg = UserFun("neg", "x", "{ return -x; }", Float, Float).
    setScalaFun( x => - x.head.asInstanceOf[Float] )

  val mult = UserFun("mult", Array("l", "r"), "{ return l * r; }", Seq(Float, Float), Float)

  val subtract = UserFun("subtract", Array("l", "r"), "{ return l - r; }", Seq(Float, Float), Float)

  val multI = UserFun("mult", Array("l", "r"), "{ return l * r; }", Seq(Int, Int), Int)

  val multdouble = UserFun("multdouble", Array("l", "r"), "{ return l * r; }", Seq(Double, Double), Double)

  val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
                             "{ return acc + (l * r); }",
                             Seq(Float, Float, Float), Float)


  val multTuple = UserFun("multTuple", "x", "{return x._0 * x._1;}", TupleType(Float, Float), Float)
  val addTuple = UserFun("addTuple", "x", "{return x._0 + x._1;}", TupleType(Float, Float), Float)
  val subtractTuple = UserFun("subtractTuple", "x", "{return x._0 - x._1;}", TupleType(Float, Float), Float)

  val addPair = UserFun(
                         "pair",
                         Array("x", "y"),
                         "{ x._0 = x._0 + y._0;" +
                         "x._1 = x._1 + y._1;" +
                         "return x; }",
                         Seq(TupleType(Float, Float), TupleType(Float, Float)),
                         TupleType(Float, Float))

  def dividedBy(divisor: ArithExpr) = UserFun("divide_by", "x", "return x/"+divisor.toString()+";", Float, Float)
  
  // Logical
  
  val or: UserFun =
    UserFun("userOr", Array("x", "y"), "return x | y;", Seq(Bool, Bool), Bool)
  
  val not: UserFun =
    UserFun("userNot", "x", "return !x;", Bool, Bool)

  implicit def IntToValue(i: Int): Value = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float): Value = Value(f.toString + "f", opencl.ir.Float)

  implicit def DoubleToValue(d: Double): Value = Value(d.toString, opencl.ir.Double)

  implicit def BooleanToValue(b: Boolean): Value = Value(b.toString, opencl.ir.Bool)

  implicit def Tuple2ToValue[T1, T2](t: (T1, T2)): Value = {
    val tupleType = TupleType(getType(t._1), getType(t._2))
    Value(t.toString().replace('(', '{').replace(')', '}'), tupleType)
  }

  implicit def Tuple3ToValue[T1, T2, T3](t: (T1, T2, T3)): Value = {
    val tupleType = TupleType(getType(t._1), getType(t._2), getType(t._3))
    Value(t.toString().replace('(', '{').replace(')', '}'), tupleType)
  }

  private def getType(a: Any): Type = a match {
    case _: Int => Int
    case _: Float => Float
    case _: Double => Double
    case _: Boolean => Bool
    case x: _root_.ir.ast.Value => x.t
    case _ => throw new IllegalArgumentException
  }
}
