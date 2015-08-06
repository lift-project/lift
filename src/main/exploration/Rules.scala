package exploration


import apart.arithmetic.{Cst, RangeMul, RangeUnknown, Var}
import ir._
import ir.ast._
import opencl.ir._
import opencl.ir.pattern._

import scala.collection.Seq

case class Rule(desc: String,
                rewrite: PartialFunction[Expr, Expr],
                isValid: Context => Boolean = _ => true)

object Rules {

  val rules: Seq[Rule] =
    Seq(
      Rule("Iterate(0, _) => Epsilon", {
        case FunCall(Iterate(n, _), arg) if n.eval == 0 => arg
      }),

      Rule("Iterate(1, x) => x", {
        case FunCall(Iterate(n, f), arg) if n.eval == 1 => f(arg)
      }),

      Rule("Epsilon() o x => x", {
        case FunCall(Epsilon(), arg) => arg
      }),

      Rule("x o Epsilon() => x", {
        case FunCall(f, FunCall(Epsilon(), arg)) => f(arg)
      }),

      Rule("Map(Epsilon()) => Epsilon()", {
        case FunCall(Map(Lambda(_, FunCall(Epsilon(), _))), arg) => arg
      }),

      Rule("joinVec o splitVec => id", {
        case FunCall(asScalar(), FunCall(asVector(_), arg)) => arg
      }),

      Rule("splitVec(_) o joinVec() => id", {
        case FunCall(asVector(n), FunCall(asScalar(), arg))
          if (arg.t match {
            case ArrayType(VectorType(_, m), _) => n == m
            case _ => false
          }) => arg
      }),

      Rule("Transpose() o Transpose() => id", {
        case FunCall(Transpose(), FunCall(Transpose(), arg)) => arg
      }),

      Rule("Join() o Split(_) => id", {
        case FunCall(Join(), FunCall(Split(_), arg)) => arg
      }),

      Rule("Split(_) o Join() => id", {
        case FunCall(Split(n), FunCall(Join(), arg))
          if (arg.t match {
            case ArrayType(ArrayType(_, m), _) => n == m
            case _ => false
          }) => arg
      }),

      Rule("Map(f) => MapSeq(f)", {
        case FunCall(Map(f), arg) => MapSeq(f)(arg)
      }),

      Rule("Map(f) => MapGlb(f)", {
        case FunCall(Map(f), arg)
          // check that none of these are nested inside
          if !f.body.contains({
            case FunCall(_:MapGlb, _) =>
            case FunCall(_:MapWrg, _) =>
            case FunCall(_:MapLcl, _) =>
            case FunCall(_:MapWarp, _) =>
            case FunCall(_:MapLane, _) =>
          })
        => MapGlb(f)(arg)

      }, c => !(c.inMapGlb || c.inMapWrg || c.inMapWarp)),

      Rule("Map(f) => MapWrg(f)", {
        case FunCall(Map(f), arg)
          // check that there is a nested map inside ...
          if f.body.contains({
            case FunCall(_:Map, _) =>
          }) &&
            // and that none of these are nested inside
            !f.body.contains({
              case FunCall(_:MapGlb, _) =>
              case FunCall(_:MapWrg, _) =>
            })
        => MapWrg(f)(arg)

      }, c => !(c.inMapGlb || c.inMapWrg || c.inMapWarp)),

      Rule("Map(f) => MapLcl(f)", {
        case FunCall(Map(f), arg)
          // check that none of these are nested inside
          if !f.body.contains({
            case FunCall(_:MapLcl, _) =>
            case FunCall(_:MapWarp, _) =>
            case FunCall(_:MapLane, _) =>
          })
        => MapLcl(f)(arg)
      }, c => c.inMapWrg && !c.inMapLcl),

      Rule("Map(f) => MapWarp(f)", {
        case FunCall(Map(f), arg)
          // check if there is a nested map inside
          if f.body.contains({
            case FunCall(_:Map, _) =>
          }) &&
            // and that none of these are nested inside
            !f.body.contains({
              case FunCall(_:MapGlb, _) =>
              case FunCall(_:MapWrg, _) =>
              case FunCall(_:MapLcl, _) =>
              case FunCall(_:MapWarp, _) =>
              case FunCall(_:MapLane, _) =>
            })
        => MapWarp(f)(arg)
      }, c => !(c.inMapGlb || c.inMapWrg || c.inMapWarp)),

      Rule("Map(f) => MapLane(f)", {
        case FunCall(Map(f), arg)
          // check that none of these are nested inside
          if !f.body.contains({
            case FunCall(_:MapLane) =>
          })
        => MapLane(f)(arg)
      }, c => c.inMapWarp && !c.inMapLane),

      Rule("Map(f) => Join() o Map(Map(f)) o Split(I)", {
        case FunCall(Map(f), arg) =>
          Join() o Map(Map(f)) o Split(4) $ arg
      }),

      Rule("Map(Reduce(f)) => Reduce(Map(f)) o Transpose()", {
        case FunCall(Map(Lambda(lambdaParams, FunCall(Reduce(f), init: Value, arg))), mapArg)
          if lambdaParams.head eq arg
        =>
          val newInit = Value(init.value, ArrayType(init.t, Type.getLength(mapArg.t)))

          Reduce(fun((acc, c) => Map(fun(x => f(Get(x, 0), Get(x, 1)))) $ Zip(acc, c)),
            newInit) o Transpose() $ mapArg
      }),

      Rule("Map(fun(a => Map() $ Zip(..., a, ...)) $ A => " +
        "Transpose() o Map(Map(fun(a => ))) $ Zip(..., Transpose() $ A, ...) ", {
        case FunCall(Map(
        Lambda(outerLambdaParam, FunCall(Map(
        Lambda(innerLambdaParam, expr
        )), FunCall(Zip(_), zipArgs @ _*))
        )), arg)
          if zipArgs.contains(outerLambdaParam.head)
        =>
          // Find all Get patterns that refer to the an element from the zipped array
          // and have to be replaced in expr
          val gets = findGets(expr, innerLambdaParam.head)

          // Find which Get pattern corresponds to the component containing an element of 'a'
          val zipToReplace = zipArgs.zipWithIndex.find(e => e._1 eq outerLambdaParam.head).get

          // Create the new Get patterns with a new parameter
          val newParam = Param()
          val getPairs =  gets zip gets.map(get => Get(newParam, get.f.asInstanceOf[Get].n))

          // Separate the Get pattern containing an element of 'a', as it will now refer
          // to the component containing an element of 'A'
          val (one, two) = getPairs.partition(_._1.f.asInstanceOf[Get].n == zipToReplace._2)

          // Replace most of the old Get patterns with new ones
          val newExpr = two.foldRight(expr)((get, e) => Expr.replace(e, get._1, get._2))

          // Create a new parameter for an element of 'a' and replace for the Get referring
          // an element of 'a'
          val secondNewParam = Param()
          val finalNewExpr = Expr.replace(newExpr, one.head._1, secondNewParam)

          // Create the arguments for the zip, replacing 'a' with 'Transpose() $ A'
          val newZipArgs = zipArgs.updated(zipToReplace._2, Transpose() $ arg)

          // Construct the final expression
          val lambda = Lambda(Array(secondNewParam), finalNewExpr)
          Transpose() o Map(Lambda(Array(newParam), Map(lambda) $ one.head._2)) $ Zip(newZipArgs:_*)
      }),

      Rule("Map(f o g) => Map(f) o Map(g)", {
        case FunCall(Map(Lambda(p1, FunCall(fun1, FunCall(fun2, p2)))), arg)
          if p2.contains({ case a => a eq p1.head})
        =>
          Map(fun1) o Map(Lambda(p1, fun2(p2))) $ arg

        case FunCall(Map(Lambda(p1, FunCall(Reduce(fun1), init, FunCall(fun2, p2)))), arg)
          if p2.contains({ case a => a eq p1.head})
        =>
          Map(Reduce(fun1, init)) o Map(Lambda(p1, fun2(p2))) $ arg
      }),

      Rule("Reduce(f) => ReduceSeq(f)", {
        case FunCall(Reduce(f), init, arg) =>

          // Construct id functions using the type of init
          var idFunction: FunDecl = id
          var idFunction2: FunDecl = id
          Type.visit(init.t, t => if(t.isInstanceOf[ArrayType]) {
            idFunction = Map(idFunction)
            idFunction2 = Map(idFunction2)
          }, t => Unit)

          val newInit = if (init.isInstanceOf[Value]) idFunction $ init else init

          toGlobal(MapSeq(idFunction2)) o ReduceSeq(f, newInit) $ arg
      }),

      Rule("Reduce(f) => Reduce(f) o PartRed(f)", {
        case FunCall(Reduce(f), init, arg) =>
          Reduce(f, init) o PartRed(f, init) $ arg
      }),

      Rule("PartRed(f) => Join() o Map(PartRed(f)) o Split()", {
        case FunCall(PartRed(f), init, arg) =>
          Join() o Map(PartRed(f, init)) o Split(4) $ arg
      }),

      Rule("Map(uf) => asScalar() o MapGlb(Vectorize(4)(uf)) o asVector(4)", {
        case FunCall(Map(Lambda(_, FunCall(uf: UserFun, _))), arg) =>
          asScalar() o Map(Vectorize(4)(uf)) o asVector(4) $ arg
      }),

      Rule("ReduceSeq o MapSeq => ReduceSeq(fused)", {
        case FunCall(ReduceSeq(f), init, FunCall(MapSeq(g), arg))
          if g.isGenerable
        =>
          ReduceSeq(fun( (acc, x) => f(acc, g(x))), init) $ arg
      }),

      Rule("Map(f) o Map(g) => Map(f o g)", {
        case FunCall(Map(f), FunCall(Map(g), arg)) =>
          Map(f o g) $ arg
      }),

      Rule("Map(fun(a => Map(fun( b => ... ) $ B) $ A => " +
        "Transpose() o Map(fun(b => Map(fun( a => ... ) $ A) $ B", {
        case FunCall(Map(Lambda(a, FunCall(Map(Lambda(b, expr)), bArg))), aArg)
          if !(a.head eq bArg)
        =>
          TransposeW() o Map(Lambda(b, FunCall(Map(Lambda(a, expr)), aArg))) $ bArg
      }),

      Rule("Map(f) => Reorder(g^{-1}) o Map(f) o Reorder(g)", {
        case FunCall(map @ Map(_), arg) =>
          Scatter(reorderStride(4)) o map o Gather(reorderStride(4)) $ arg
      }),

      Rule("Map(fun(a => ... $ a)) $ A => " +
        "Transpose() o Map(fun(a =>... $ a)) o Transpose() $ A  ", {
        case outerCall @ FunCall(Map(Lambda(lambdaArg, innerCall @ FunCall(f, funCallArg))), arg)
          if (lambdaArg.head eq funCallArg) // TODO: Too strict, the final arg at this level should be the same
            && (outerCall.t match {
            case ArrayType(ArrayType(_, _), _) => true
            case _ => false
          })
            && (arg.t match {
            case ArrayType(ArrayType(_, _), _) => true
            case _ => false
          })
            && !innerCall.contains({
            case FunCall(Split(_), _) =>
            case FunCall(Join(), _) => // TODO: ok, if to get rid of length 1 array from reduce?
            // TODO: Map( Join() o Map(Reduce()))
          })
        =>
          TransposeW() o Map(f) o Transpose() $ arg
      }),

      Rule("Map(fun(x => Map(fun(a => )) $ Get(n, ) $ Zip(..., A, ...)  => " +
        "Transpose() o Map(fun(a => Map() $ Zip(..., a, ...)) o Transpose() $ A", {
        case c @ FunCall(Map(Lambda(outerLambdaParam ,
        FunCall(Map(Lambda(innerLambdaParam,
        expr
        )), getCall @ FunCall(Get(n), getParam))
        )), FunCall(Zip(_), zipArgs@_*))

          if getParam eq outerLambdaParam.head
        =>

          // Find all Get patterns that refer to the an element from the zipped array
          // and have to be replaced in expr
          val gets = findGets(expr, outerLambdaParam.head)

          // Find 'A'
          val newArg = zipArgs(n)

          // Construct new Get patterns and replace them for the old ones
          val newParam = Param()
          val newGets = gets zip gets.map(get => Get(newParam, get.f.asInstanceOf[Get].n))
          val newExpr =  newGets.foldRight(expr)((get, e) => Expr.replace(e, get._1, get._2))

          // Create a get pattern for an element 'a'
          val finalNewExpr = Expr.replace(newExpr, innerLambdaParam.head, Get(n)(newParam))

          // Replace 'a' with a new parameter to create the new arguments for zip
          val secondNewParam = Param()
          val newZipArgs = zipArgs.updated(n, secondNewParam)

          Transpose() o Map(Lambda(Array(secondNewParam),
            Map(Lambda(Array(newParam), finalNewExpr)) $ Zip(newZipArgs:_*)
          )) o Transpose() $ newArg
      })
    )

  private def findGets(expr: Expr, tupleParam: Expr): List[FunCall] = {
    Expr.visitWithState(List[FunCall]())(expr, (e, s) => {
      e match {
        case get@FunCall(Get(_), getParam) if getParam eq tupleParam => get :: s
        case _ => s
      }
    })
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
//            case cf: CompFun => composedDerivations(cf, c, level)
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
//        if (!call.context.inMapSeq && (call.context.inMapGlb || call.context.inMapLcl))
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

//        if (!call.context.inSeq && (call.context.inMapGlb || call.context.inMapLcl))
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
}
