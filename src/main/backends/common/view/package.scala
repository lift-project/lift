package backends.common

import ir.ast.{FunCall, Param}
import ir.{AddressSpace, ArrayType, Type}
import lift.arithmetic.ArithExpr

package object view {
  /**
   * A shorthand used to talk about AccessInfo.
   * It represents the access information for one dimension whereas AccessInfo
   * gives the access information for an array of arbitrary dimension in
   * different address spaces. Hence, AccessInfo can be seen as a 3-tuple of
   * `List[SingleAccess]`
   */
  type SingleAccess = (Type => ArrayType, ArithExpr)
  type MemoryAccessInfo = (scala.collection.mutable.ListMap[AddressSpace, List[SingleAccess]])

  abstract class AccessInfo() {
    def accessInf: MemoryAccessInfo
    def collection: Seq[AccessInfo]

    def apply[T <: AccessInfo](thisLevel: SingleAccess, useMemories: Set[AddressSpace])
                              (implicit accessInfoSingleton: AccessInfoSingleton[T]): AccessInfo = {
      if (collection.isEmpty) {
        accessInfoSingleton(accessInf.map {
          case (as: AddressSpace, accessInfo: List[SingleAccess]) =>
            if (useMemories.contains(as) || accessInfoSingleton.alwaysUsedMemories.contains(as))
              as -> (thisLevel :: accessInfo)
            else
              as -> accessInfo
        })
      } else {
        accessInfoSingleton(collection.map(aInf => accessInfoSingleton(aInf.accessInf.map {
          case (as: AddressSpace, accessInfo: List[SingleAccess]) =>
            if (useMemories.contains(as) || accessInfoSingleton.alwaysUsedMemories.contains(as))
              as -> (thisLevel :: accessInfo)
            else
              as -> accessInfo
        })))
      }
    }
  }

  trait AccessInfoSingleton[T <: AccessInfo] {
    val alwaysUsedMemories: Set[AddressSpace]

    def apply(): T
    def apply(accessInf: MemoryAccessInfo): T
    def apply(collection: Seq[T]): T
  }

  trait BuildDepthInfo[T <: AccessInfo] {
    var memoryAccessInfo: MemoryAccessInfo
    implicit val accessInfoSingleton: AccessInfoSingleton[T]

    protected def updateAccessInf(accessedMemories: Set[AddressSpace],
                                  tuple: SingleAccess): Unit = {
      (accessedMemories ++ accessInfoSingleton.alwaysUsedMemories).foreach(memoryAccessInfo(_) ::= tuple)
    }

    protected def restoreAccessInf(accessedMemories: Set[AddressSpace]): Unit = {
      (accessedMemories ++ accessInfoSingleton.alwaysUsedMemories).foreach(addressSpace =>
        memoryAccessInfo(addressSpace) = memoryAccessInfo(addressSpace).tail)
    }

    protected def setDepths(call: FunCall, readMemories: Set[AddressSpace], writeMemories: Set[AddressSpace]): Unit = {
      call.inputDepth = getAccessInfo(writeMemories)
      call.outputDepth = getAccessInfo(readMemories)
    }

    protected def setAccessInfo(list: T, param: Param): Unit
    protected def getAccessInfo(accessedMemories: Set[AddressSpace]): List[SingleAccess]

    /**
     * Utility function for building a bit of access information about an
     * ArrayType instance
     *
     * @param ty the array type passed as a generic type. We check that it is
     *           indeed an array type below.
     * @param v the variable used to perform the access
     */
    protected def getArrayAccessInf(ty: Type, v: ArithExpr): SingleAccess = {
      ty match {
        case at: ArrayType => (at.replacedElemT, v)
        case _ => throw new IllegalArgumentException(
          s"Cannot compute access information for $ty. ArrayType required."
        )
      }
    }
  }
}
