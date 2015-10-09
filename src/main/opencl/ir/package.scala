package opencl

import _root_.ir.{Type, TupleType}
import _root_.ir.ast.{UserFun, Value}

import scala.language.implicitConversions

package object ir {
  // commonly used user functions

  val id = UserFun("id", "x", "{ return x; }", Float, Float).
    setScalaFun(x => x.head)

  val idI = UserFun("id", "x", "{ return x; }", Int, Int)

  val iddouble = UserFun("iddouble", "x", "{ return x; }", Double, Double)

  val idFI = UserFun("id", "x", "{ return x; }", TupleType(Float, Int), TupleType(Float, Int))

  val idFF = UserFun("id", "x", "{ return x; }", TupleType(Float, Float), TupleType(Float, Float))

  val absAndSumUp = UserFun("absAndSumUp", Array("acc", "x"), "{ return acc + fabs(x); }",
                            Seq(Float, Float), Float)

  val add = UserFun("add", Array("x", "y"), "{ return x+y; }", Seq(Float, Float), Float).
    setScalaFun( xs => xs(0).asInstanceOf[Float] + xs(1).asInstanceOf[Float] )

  val adddouble = UserFun("adddouble", Array("x", "y"), "{ return x+y; }", Seq(Double, Double), Double)

  val plusOne = UserFun("plusOne", "x", "{ return x+1; }", Float, Float).
    setScalaFun( xs => xs(0).asInstanceOf[Float] + 1.0f )

  val doubleItAndSumUp = UserFun("doubleItAndSumUp", Array("x", "y"), "{ return x + (y * y); }",
                                 Seq(Float, Float), Float)

  val sqrtIt = UserFun("sqrtIt", "x", "{ return sqrt(x); }", Float, Float)

  val abs = UserFun("abs", "x", "{ return x >= 0 ? x : -x; }", Float, Float)

  val neg = UserFun("neg", "x", "{ return -x; }", Float, Float).
    setScalaFun( x => - x.head.asInstanceOf[Float] )

  val mult = UserFun("mult", Array("l", "r"), "{ return l * r; }", Seq(Float, Float), Float)

  val multdouble = UserFun("multdouble", Array("l", "r"), "{ return l * r; }", Seq(Double, Double), Double)

  val multAndSumUp = UserFun("multAndSumUp", Array("acc", "l", "r"),
                             "{ return acc + (l * r); }",
                             Seq(Float, Float, Float), Float)

  val addPair = UserFun(
                         "pair",
                         Array("x", "y"),
                         "{ x._0 = x._0 + y._0;" +
                         "x._1 = x._1 + y._1;" +
                         "return x; }",
                         Seq(TupleType(Float, Float), TupleType(Float, Float)),
                         TupleType(Float, Float))

  implicit def IntToValue(i: Int): Value = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float): Value = Value(f.toString + "f", opencl.ir.Float)

  implicit def DoubleToValue(d: Double): Value = Value(d.toString, opencl.ir.Double)

  implicit def Tuple2ToValue[T1, T2](t: (T1, T2)): Value = {
    val tupleType = TupleType(getType(t._1), getType(t._2))
    Value(t.toString().replace('(', '{').replace(')', '}'), tupleType)
  }

  implicit def Tuple3ToValue[T1, T2, T3](t: (T1, T2, T3)): Value = {
    val tupleType = TupleType(getType(t._1), getType(t._2), getType(t._3))
    Value(t.toString().replace('(', '{').replace(')', '}'), tupleType)
  }

  private def getType(a: Any): Type = a match {
    case _: Float => Float
    case _: Int => Int
    case _ => throw new IllegalArgumentException
  }
}
