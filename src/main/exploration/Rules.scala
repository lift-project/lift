package exploration


import apart.arithmetic.{Cst, RangeMul, RangeUnknown, Var}
import ir._
import ir.ast._

import scala.collection.Seq
import opencl.ir.pattern._

object Rules {
   

  private def composedDerivations(l: Lambda, c: Constraints, level: Int): Seq[Lambda] = {
    l.body match {
      case call: FunCall => call.f match {
        case cf: CompFun =>
          val optionsList = cf.funs.map(f => derivsWithOneRule(f,c, level))
          // TODO: unsure here ...
          Utils.listPossiblities(cf.funs, optionsList).map(funs => fun( _ => (new CompFun(funs: _*))(call.args:_*)))
      }
    }
  }
   
   private def innerDerivations(l: Lambda, c: Constraints, level: Int): Seq[Lambda] = {
     l.body match {
       case call: FunCall => call.f match {
         case fpat: FPattern =>
           val newCalleeList = derivsWithOneRule(fpat.f, c, level).map((f) => call.f match {
             case ar: AbstractPartRed => ar.getClass.getConstructor(classOf[Lambda],classOf[Value]).newInstance(f, call.args.head)
             case fpat: FPattern => fpat.getClass.getConstructor(classOf[Lambda]).newInstance(f)
           } )
           newCalleeList.map(c => new Lambda(fpat.f.params, c(call.args: _*)))
       }
     }
   }
    
    
  /*
   * Return a list of all possible derivations using only one rule at a given level
   */
  def derivsWithOneRule(l: Lambda, c: Constraints, level: Int): Seq[Lambda] =  {
      l.body match {
        case call: FunCall => call.f match {
            case cf: CompFun => composedDerivations(cf, c, level)
            case fp: FPattern if level==0 => derivePatFunCall(call,c).map(c => new Lambda(l.params, c))
            case fp: FPattern if level>0 => innerDerivations(fp,c,level-1)
            case p: Pattern if level==0  => derivePatFunCall(call, c).map(c => new Lambda(l.params, c))
            case _ => List()
          }
        case _ => List()
      }
  } 


  private def validOSplitRange(t: Type) = {
    t match {
      case ArrayType(_, len) => RangeMul(Cst(1), len, Cst(2))
      case _ => RangeUnknown // Error
    }
  }
 

  def derivePatFunCall(call: FunCall, c: Constraints): Seq[FunCall] = {

    val newCalleeList = call.f match {

      case Map(inF) =>
        var result = List[FunDecl]()

        // sequential
        if (!call.context.inSeq && (call.context.inMapGlb || call.context.inMapLcl))
          result = result :+ MapSeq(inF)

        // global, workgroup
        if (call.context.mapDepth == 0 && !call.context.inMapGlb && !call.context.inMapWrg) {
          result = result :+ MapGlb(inF)
          result = result :+ MapWrg(inF)
        }

        // local
        if (call.context.mapDepth == 1 && call.context.inMapWrg && !call.context.inMapGlb && !call.context.inMapLcl) {
          result = result :+ MapLcl(inF)
        }

        // warp
        if (call.context.mapDepth == 1 && call.context.inMapLcl && !call.context.inMapWarp  && !call.context.inMapLane) {
          result = result :+ MapWarp(inF)
        }

        // lane
        if (call.context.mapDepth == 1 && (call.context.inMapLcl || call.context.inMapWarp) && !call.context.inMapLane) {
          result = result :+ MapLane(inF)
        }

        // split-join
        if (call.context.mapDepth+1 < c.maxMapDepth && !c.converge)
          result = result :+ (Join() o Map(Map(inF)) o Split(Var(validOSplitRange(call.argsType))))

        result

      case Reduce(inF) =>
        var result = List[FunDecl]()
        if (!c.converge)
          result = result :+ (Reduce(inF) o PartRed(inF))

        if (!call.context.inSeq && (call.context.inMapGlb || call.context.inMapLcl))
          result = result :+ ReduceSeq(inF)

        result

      case PartRed(inF) =>
        var result = List[FunDecl]()
        result = result :+ Reduce(inF)
        if (call.context.mapDepth < c.maxMapDepth && !c.converge)
          result = result :+ (Join() o Map(PartRed(inF)) o Split(Var(validOSplitRange(call.argsType))))
        result

      case _ => List[FunDecl]() // all the terminals end up here

    }

    newCalleeList.map(fd => new FunCall(fd,call.args:_*))
  }
  
  
  
  /*
   * Simplification and fusion rules.
   */
  /*def pairDeriv(f1: Fun, f2: Fun): List[Fun] = {
    f1 match {

      case Map(f,ctx1) => {
        f2 match {
          case Map(g,ctx2) => List(Map(new CompFun(f, g),ctx1)) // TODO: merge context ctx1 and ctx2
          case _ => List()
        }
      }

      case oSplit(_) => {
        f2 match {
          case oJoin(_) => List(new NullFun())
          case _ => List()
        }
      }

      case iSplit(_) => {
        f2 match {
          case iJoin(_) => List(new NullFun())
          case _ => List()
        }
      }

      case _ => List()      
    }
  }*/

}