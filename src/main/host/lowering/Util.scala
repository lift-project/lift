package host.lowering

import core.generator.GenericAST.{CArrayType, CHostArrayType, COclArrayType, CTypeT, FloatType, PointerType, VoidType}
import ir.Type


object Util {


 def IRType2CastType(t:Type, array_hint:String = "unknown"):CTypeT = {

  t match {
   case ir.ArrayType(elemT) => {
    array_hint match {
     case "unknown" => CArrayType("array", IRType2CastType(elemT))
     case "host" => CHostArrayType("array", IRType2CastType(elemT))
     case "device" => COclArrayType("array", IRType2CastType(elemT) )
     case _ => {
      assert(false)
      CArrayType("error", IRType2CastType(elemT))
     }
    }
   }
   case opencl.ir.Float => FloatType()
   case _ => {
    assert(false)
    VoidType()
   }
  }

 }

 def GetElementTypeFromPointer(t: CTypeT) : CTypeT = {
  t match {
   case PointerType(_,elem_t) => GetElementTypeFromPointer(elem_t)
   case _ => t
  }
 }

 def GetElementTypeFromArray(t: CTypeT) : CTypeT = {
  t match {
   case array_t: CArrayType => GetElementTypeFromArray(array_t.elem_t)
   case _ => t
  }
 }


 def Array2Pointer(t:CTypeT, flatType : Boolean = false):CTypeT = {

  t match {
   case array_t:CArrayType =>
    flatType match {
     case false => PointerType(Array2Pointer(array_t.elem_t))
     case true => PointerType(GetElementTypeFromArray(array_t.elem_t))
    }
   case _ => t
  }

 }

}
