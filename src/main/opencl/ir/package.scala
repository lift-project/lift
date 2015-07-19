package opencl

import _root_.ir.{Type, TupleType}
import _root_.ir.ast.Value

package object ir {
  implicit def IntToValue(i: Int): Value = Value(i.toString, opencl.ir.Int)

  implicit def FloatToValue(f: Float): Value = Value(f.toString + "f", opencl.ir.Float)

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
