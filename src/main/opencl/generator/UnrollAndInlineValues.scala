package opencl.generator

import core.generator.GenericAST
import core.generator.GenericAST._
import core.generator.GenericAST.{AstNode, CVar}
import ir._
import lift.arithmetic.{Cst, Var}
import opencl.generator.OpenCLAST.{OclStore, OclVarDecl, VectorLiteral}
import opencl.ir.PrivateMemory

import scala.collection.mutable._

/**
  * Class for functionality to unroll arrays or inline structs in private memory in the AST
  */

object UnrollValues {

  // static variable set when a pass makes a change
  var hasChanged : Boolean = false

  // anonymous identity function
  val idPostFun = (n: AstNode) => n

  // map to keep track of the unrolled variables from private arrays and structs
  var oclVarDeclMap = ListMap[CVar, Array[OclVarDecl]]()
  // map to keep track of the tuple types of the unrolled tuples - if there is a better way feel free to implement it
  var oclTupleTypeMap = ListMap[CVar, TupleType]()
  // map to keep track of "already temporarily unrolled tuples" - we do this otherwise the pass will unroll forever
  var tempTuples = Array[String]()

  // until there is a better way: get first index from suffix and return the resulting suffix string
  def getIndexSuffix(str: String): (Int, String) = {

    val idx = str.split("_|\\.").filter(_.nonEmpty).lift(0).getOrElse("-1")
    if (idx == "-1")
    {
      (idx.toInt, "")
    }
    else {
      var suffix = str.split("_", 2).filter(_.nonEmpty)(0).split(idx, 2).mkString
      if (suffix == ".") {
        suffix = "";
      }
      (idx.toInt, suffix)
    }
  }

  // sometimes need to recreate the tuple we have unrolled ( ie. putting it into global memory )
  def recreateStruct(arr: Array[OclVarDecl], ai: Option[ArithExpression], vi: Option[Int], tt: TupleType): StructConstructor = {
    var varList = Vector[AstNode]()
    // loop over array of oclVarDecls
    for (ocl <- arr)
    {
      val vR = VarRef(v = ocl.v, arrayIndex = ai, vectorIndex = vi)
      varList = varList :+ vR
    }
    StructConstructor(tt, varList)
  }

  // Create a new VarRef referring to the correct unrolled value
  def getCorrectVarRef(v: CVar,
                       s: Option[String] ,
                       ai: Option[ArithExpression],
                       vi: Option[Int],
                       oclVarDeclMap: ListMap[CVar, Array[OclVarDecl]]
                      ): VarRef =
  {
    var vr = VarRef(v, s, ai, vectorIndex = vi)
    if (oclVarDeclMap.contains(v))
    {
      val idxSuffix = getIndexSuffix(s.getOrElse(throw new Exception("Unable to find index for " + v.v.toString)))
      val ocl = oclVarDeclMap(v)(idxSuffix._1)
//      val strSuffix = if (!idxSuffix._2.equals("")) Some(idxSuffix._2) else None
      val strSuffix = Some(idxSuffix._2)
      vr = VarRef(ocl.v, strSuffix, ai, vectorIndex = vi)
    }
    vr
  }

  // recreate list of VarRefs
  def getVarRefList(args: List[GenericAST.AstNode],oclVarDeclMap: ListMap[CVar, Array[OclVarDecl]]) : List[GenericAST.AstNode] =
  {
    var lst = List[GenericAST.AstNode]()
    for (arg <- args)
    {
      arg match
      {
        case VarRef(v_b, s_b, ai_b, vi_b) =>
          var vrH = getCorrectVarRef(v_b,s_b,ai_b,vi_b,oclVarDeclMap)
          lst = lst :+ vrH
        case StructConstructor(t, args) =>
          var newargs = Vector[GenericAST.AstNode]()
          for (arg <- args)
          {
            arg match
            {
              case VarRef(v_b, s_b, ai_b, vi_b) =>
                var vrL = getCorrectVarRef(v_b,s_b,ai_b,vi_b,oclVarDeclMap)
                newargs = newargs :+ vrL
              case _ =>
                newargs = newargs :+ arg
            }
          }
          lst = lst :+ StructConstructor(t, newargs)

        case an: AstNode => lst = lst :+ an
        case _ =>
          lst = lst :+ arg
      }
    }
    lst
  }

  /**
    * pass to unroll private arrays
    */

  def unrollPrivateMemoryArrayValues(node: AstNode): AstNode =
  {

    hasChanged = false

    val preFunctionForUnrollingArrays = (n: AstNode) => n match {
      case mb: MutableBlock =>
        // create new vector for our mb
        var nodeVector = Vector[AstNode with BlockMember]()
        mb.content.map(node =>
          node match {
            // if it's a variable declaration:
            case ovd: OclVarDecl =>
              ovd.addressSpace match
              {
                case PrivateMemory =>
                  ovd.t match
                  {
		   case ArrayTypeWSWC(t,s,c) =>
                        // loop over size of array and create new OclVarDecls for each "unrolled value"
                        oclVarDeclMap += (ovd.v -> Array[OclVarDecl]())
                          for (i <- 1 to ovd.length.toInt)
                          {
                            var oclVDtmp = OclVarDecl(CVar(Var(ovd.v.v.toString + "_" + i)), Type.getValueType(t), ovd.init, 0, PrivateMemory)
                            // push them back in new vector
                            nodeVector = nodeVector :+ oclVDtmp
                           // and add them to our "map" to reference later
                            oclVarDeclMap += (ovd.v -> (oclVarDeclMap(ovd.v) :+ oclVDtmp))
                          }
                          hasChanged = true
                    case ArrayTypeWS(t,s) =>
                      throw new Exception("Unable to handle ArrayTypeWS in Private Array Unrolling!")
                    case ArrayTypeWC(t,c) =>
                      throw new Exception("Unable to handle ArrayTypeWC in Private Array Unrolling!")
                    case _ => nodeVector = nodeVector :+ ovd

                  }
                case _ => nodeVector = nodeVector :+ ovd
              }
            // otherwise just push back in new Vector
            case an: AstNode => nodeVector = nodeVector :+ an
          }
        )
        // return block with new vector values
        MutableBlock(nodeVector, mb.global)

      case ExpressionStatement(e, _) => e match
      {
        case AssignmentExpression(lhs, rhs) => (lhs, rhs) match
        {
          case (VarRef(v1, s1, ai1, vi1), VarRef(v2, s2, ai2, vi2)) =>
            if (oclVarDeclMap.contains(v1) && !oclVarDeclMap.contains(v2))
            {
              val idxSuffix = getIndexSuffix(s1.getOrElse(throw new Exception("Unable to find index for " + v1.v.toString)))
              // need to update the variable for v1, v2 stays the same
              val lhsOcl = oclVarDeclMap(v1)(idxSuffix._1)
              val lhs = VarRef(lhsOcl.v, Some(idxSuffix._2), ai1, vectorIndex = vi1)
              val rhs = VarRef(v2, s2, ai2, vectorIndex = vi2)
              ExpressionStatement(AssignmentExpression(lhs, rhs))
            }
            else if (oclVarDeclMap.contains(v2) && !oclVarDeclMap.contains(v1))
            {
              // need to update the variable for v2, v1 stays the same
              val idxSuffix = getIndexSuffix(s2.getOrElse(throw new Exception("Unable to find index for " + v1.v.toString)))
              val lhs = VarRef(v1, s1, ai1, vectorIndex = vi1)
              val rhsOcl = oclVarDeclMap(v2)(idxSuffix._1)
              val rhs = VarRef(rhsOcl.v, s2, ai2, vectorIndex = vi2)
              ExpressionStatement(AssignmentExpression(lhs, rhs))
            }
            else if (oclVarDeclMap.contains(v1) && oclVarDeclMap.contains(v2))
            {
              val idxSuffix1 = getIndexSuffix(s1.getOrElse(throw new Exception("Unable to find index for " + v1.v.toString)))
              val lhsOcl = oclVarDeclMap(v1)(idxSuffix1._1)
              val lhs = VarRef(lhsOcl.v, Some(idxSuffix1._2), ai1, vectorIndex = vi1)
              val idxSuffix2 = getIndexSuffix(s2.getOrElse(
                throw new Exception("Unable to find index for " + v1.v.toString)))
              val rhsOcl = oclVarDeclMap(v2)(idxSuffix2._1)
              val rhs = VarRef(rhsOcl.v, Some(idxSuffix2._2), ai2, vectorIndex = vi2)
              ExpressionStatement(AssignmentExpression(lhs, rhs))
            }
            else // nothing to be unrolled - yay!
            {
              ExpressionStatement(AssignmentExpression(VarRef(v1, s1, ai1, vectorIndex = vi1), VarRef(v2, s2, ai2, vectorIndex = vi2)))
            }
          case (VarRef(v, s, ai, vi), FunctionCall(f, args, _)) =>
            val vr = getCorrectVarRef(v,s,ai,vi,oclVarDeclMap)
            // update args list with new values of VarRefs
            val lst = getVarRefList(args,oclVarDeclMap)
            ExpressionStatement(AssignmentExpression(vr, FunctionCall(f, lst)))
          case (VarRef(v, s, ai, vi), TernaryExpression(cond, trueExpr, falseExpr)) =>
            val vr = getCorrectVarRef(v,s,ai,vi,oclVarDeclMap)
            ExpressionStatement(AssignmentExpression(vr, TernaryExpression(cond, trueExpr, falseExpr)))

          case (VarRef(v, s, ai, vi), StructConstructor(t, args)) =>
            val vr = getCorrectVarRef(v,s,ai,vi,oclVarDeclMap)
            ExpressionStatement(AssignmentExpression(vr, StructConstructor(t, args)))
          case _ => ExpressionStatement(e)
        }
        case _ => ExpressionStatement(e)
      }

      case StructConstructor(t, args) =>
        var newargs = Vector[GenericAST.AstNode]()
        for (arg <- args) {
          arg match {
            case VarRef(v_b, s_b, ai_b, vi_b) =>
              var vr = getCorrectVarRef(v_b,s_b,ai_b,vi_b,oclVarDeclMap)
              newargs = newargs :+ vr
            case _ =>
              newargs = newargs :+ arg
          }
        }
        StructConstructor(t, newargs)

      case v: VectorLiteral =>
        var newargs = List[GenericAST.VarRef]()
        for (arg <- v.vs)
        {
          arg match {
            case VarRef(v_b, s_b, ai_b, vi_b) =>
              var vr = getCorrectVarRef(v_b,s_b,ai_b,vi_b,oclVarDeclMap)
              newargs = newargs :+ vr
            case _ =>
              newargs = newargs :+ arg
          }
        }
        VectorLiteral(v.t, newargs: _*)

      case OclStore(vr, t, v, offset, addressSpace) =>
        var varRef = vr
        var vNew = v
        if (oclVarDeclMap.contains(vr.v))
        {
          val idxSuffix = getIndexSuffix(vr.suffix.getOrElse(throw new Exception("Unable to find index for " + vr.v.v.toString)))
          val ocl = oclVarDeclMap(vr.v)(idxSuffix._1)
          varRef = VarRef(ocl.v, Some(idxSuffix._2), vr.arrayIndex, vr.vectorIndex)
        }
        v match
        {
          case FunctionCall(f, args, _) =>
            val lst = getVarRefList(args,oclVarDeclMap)
            vNew = FunctionCall(f, lst)
          case _ => v
        }
        OclStore(varRef, t, vNew, offset, addressSpace)

      case IfThenElse(cond,tb,fb) =>
        var newCond = cond
        cond match
        {
          case VarRef(v, s, ai, vi) =>
            if (oclVarDeclMap.contains(v))
            {
              val idxSuffix = getIndexSuffix(s.getOrElse(throw new Exception("Unable to find index for " + v.v.toString)))
              val ocl = oclVarDeclMap(v)(idxSuffix._1)
              newCond = VarRef(ocl.v, Some(idxSuffix._2), ai, vectorIndex = vi)
            }
          case _ =>
        }
        IfThenElse(newCond,tb,fb)

      case OclVarDecl(v,t,i,l,as) =>
        var init = i
        i.getOrElse("") match {
          case VarRef(v, s, ai, vi) =>
            if (oclVarDeclMap.contains(v))
            {
              val idxSuffix = getIndexSuffix(s.getOrElse(throw new Exception("Unable to find index for " + v.v.toString)))
              val ocl = oclVarDeclMap(v)(idxSuffix._1)
              init = Option(VarRef(ocl.v, Some(idxSuffix._2), ai, vectorIndex = vi))
            }
          case _ =>
        }
        OclVarDecl(v,t,init,l,as)

      case VarRef(v, s, ai, vi) =>
        if (oclVarDeclMap.contains(v)) {
          throw new Exception("Unrolling private memory unavailable for variable " + v.v.toString + "!")
        }
        else VarRef(v, s, ai, vi)
      case _ => n
    }

    node.visitAndRebuild(preFunctionForUnrollingArrays, idPostFun)
  }

  /**
    * pass to inline structs
    */
  def inlinePrivateMemoryStructValues(node: AstNode): AstNode =
  {

    hasChanged = false

    val preFunctionForUnrollingStructs = (n: AstNode) => n match {
      case mb: MutableBlock =>
        // create new vector
        var nodeVector = Vector[AstNode with BlockMember]()
        mb.content.map(node =>
          node match {
            // if it's a variable declaration:
            case ovd: OclVarDecl =>
              ovd.t match
              {
                case tt : TupleType =>
                  oclTupleTypeMap += (ovd.v -> tt)
                  ovd.addressSpace match
                  {
                    case PrivateMemory =>
                      if( !oclVarDeclMap.contains(ovd.v) && !tempTuples.contains(ovd.v.toString()) )
                      {
                        hasChanged = true
                        // loop over number of elements and create a new variable for each
                        oclVarDeclMap += (ovd.v -> Array[OclVarDecl]())
                        for (i <- 0 until tt.elemsT.length)
                        {
                          val currElem = tt.elemsT(i)
                          var oclVDtmp = OclVarDecl(CVar(Var(ovd.v.v.toString+"_" + i)), tt.proj(i), ovd.init, 0, PrivateMemory)
                          if(ovd.init != None)
                          {
                            throw new NotImplementedError("Trying to unroll initialised tuples - there is no method that can currently handle this!")
                          }
                          // and push them back in new vector
                          nodeVector = nodeVector :+ oclVDtmp
                          oclVarDeclMap += (ovd.v -> (oclVarDeclMap(ovd.v) :+ oclVDtmp))
                        }
                      }
                      else
                      {
                        nodeVector = nodeVector :+ ovd
                      }
                    case _ => nodeVector = nodeVector :+ ovd
                  }
                case _ => nodeVector = nodeVector :+ ovd
              }
            // otherwise just push back in new Vector
            case an: AstNode => nodeVector = nodeVector :+ an
          }
        )
        // return block with new vector
        MutableBlock(nodeVector, mb.global)

      case ExpressionStatement(e, _) => e match
      {
        case AssignmentExpression(lhs, rhs) => (lhs, rhs) match {
          case (VarRef(v1, s1, ai1, vi1), VarRef(v2, s2, ai2, vi2)) =>
            if (oclVarDeclMap.contains(v1) && !oclVarDeclMap.contains(v2))
            {
              val idxSuffix = getIndexSuffix(s1.getOrElse(""))
              if( idxSuffix._1 < 0 )
              {
                // In this situation, we are not supplied with a suffix, which we must suppose means that we are setting an unrolled tuple equal to another tuple
                // pull out the separated tuple variable (LHS) and set them equal to the appropriate values in the RHS tuple

                var nodeVector = Vector[AstNode with BlockMember]()

                // get a list of the VarDecls that v1 points to
                val v1OclList = oclVarDeclMap(v1)

                val numTupleValues = oclVarDeclMap(v1).length

                // loop over and set each one to the variable + suffix added to s2 suffix
                for(i <- 0 until numTupleValues)
                {
                  val ocl = oclVarDeclMap(v1)(i)
                  var suffix = None: Option[String]
                  suffix = Some(s2.getOrElse("")+"._"+i)
                  nodeVector = nodeVector :+ ExpressionStatement(AssignmentExpression(VarRef(ocl.v,Some(""),None,vi1),VarRef(v2.v,suffix,None,vi2)))
                }

                MutableBlock(nodeVector)
              }
              else
              {
                // need to update the variable for v1, v2 stays the same
                val lhsOcl = oclVarDeclMap(v1)(idxSuffix._1)
                val lhs = VarRef(lhsOcl.v, Some(idxSuffix._2), ai1, vi1)
                val rhs = VarRef(v2, s2, ai2, vi2)
                ExpressionStatement(AssignmentExpression(lhs, rhs))
              }
            }
            else if (oclVarDeclMap.contains(v2) && !oclVarDeclMap.contains(v1)) {
              // need to update the variable for v2, v1 stays the same
              val idxSuffix = getIndexSuffix(s2.getOrElse(""))
              if( idxSuffix._1 < 0 )
              {

                var nodeVector = Vector[AstNode with BlockMember]()

                // get a list of the VarDecls that v1 points to
                val v1OclList = oclVarDeclMap(v2)

                val numTupleValues = oclVarDeclMap(v2).length

                // loop over and set each one to the variable + suffix added to s2 suffix
                for(i <- 0 until numTupleValues)
                {
                  val ocl = oclVarDeclMap(v2)(i)
                  var suffix = None: Option[String]
                  suffix = Some(s1.getOrElse("")+"._"+i)
                  nodeVector = nodeVector :+ ExpressionStatement(AssignmentExpression(VarRef(v1.v,suffix,None,vi1),VarRef(ocl.v,Some(""),None,vi2)))
                }

                MutableBlock(nodeVector)

              }
              else
              {
                val lhs = VarRef(v1, s1, ai1, vi1)
                val rhsOcl = oclVarDeclMap(v2)(idxSuffix._1)
                val rhs = VarRef(rhsOcl.v, s2, ai2, vi2)
                ExpressionStatement(AssignmentExpression(lhs, rhs))
              }
            }
            else if (oclVarDeclMap.contains(v1) && oclVarDeclMap.contains(v2)) {

              val tupleListL = oclVarDeclMap(v1)
              val tupleListR = oclVarDeclMap(v2)
              var nodeVector = Vector[AstNode with BlockMember]()
              // unroll them both
              for(i <- 0 until tupleListL.length )
              {
                val oclL = tupleListL(i).v
                val oclR = tupleListR(i).v
                nodeVector = nodeVector :+ ExpressionStatement(AssignmentExpression(VarRef(oclL.v,s1,ai1,vi1),VarRef(oclR.v,s2,ai2,vi2)))
              }
              MutableBlock(nodeVector,true)
            }
            else // nothing to be unrolled - yay!
            {
              ExpressionStatement(AssignmentExpression(VarRef(v1, s1, ai1, vi1), VarRef(v2, s2, ai2, vi2)))
            }

          case (VarRef(v, s, ai, vi), rhs) =>
            var vr = VarRef(v, s, ai, vi)
            if (oclVarDeclMap.contains(v))
            {
              val idxSuffix = getIndexSuffix(s.getOrElse(""))
              if (idxSuffix._1 < 0) {

                // determine how many "unrolled" values are in struct
                val numTupleValues = oclVarDeclMap(v).length
                // create new block
                var nodeVector = Vector[AstNode with BlockMember]()
                // set result of function to "tmp" variable
                // first "reconstruct the tuple type from the OclDeclMap
                var tupleTypes = List[Type]()
                for(ocl <- oclVarDeclMap(v))
                {
                  tupleTypes = tupleTypes :+ ocl.t
                }
                var tmp_cvar = CVar(Var(v.v.toString + "_tmp"))
                val tup = TupleType(tupleTypes: _*)
                var tmp = OclVarDecl(tmp_cvar, tup, None, 0, PrivateMemory)
                nodeVector = nodeVector :+ tmp
                nodeVector = nodeVector :+ ExpressionStatement(AssignmentExpression(VarRef(tmp_cvar,Some(""),None,vi),rhs))

                // loop over number of "unrolled values" and set the tmp values to these values
                for(i <- 0 until numTupleValues)
                {
                  val ocl = oclVarDeclMap(v)(i)
                  var suffix = None: Option[String]
                  suffix = Some("._"+i)
                  var idx = None: Option[ArithExpression]
                  idx = Some(ArithExpression(Cst(i)))
                  nodeVector = nodeVector :+ ExpressionStatement(AssignmentExpression(VarRef(ocl.v,Some(""),None,vi),VarRef(tmp.v,suffix,None,vi)))
                }

                //ensure we don't try to unroll this tuple creation
                tempTuples = tempTuples :+ tmp_cvar.toString()
                MutableBlock(nodeVector)

              }
              else
              {
                val ocl = oclVarDeclMap(v)(idxSuffix._1)
                vr = VarRef(ocl.v, Some(idxSuffix._2), ai,vi)
                ExpressionStatement(AssignmentExpression(vr, rhs))
              }
            }
            else
            {
              ExpressionStatement(AssignmentExpression(vr, rhs))
            }

          case _ => ExpressionStatement(AssignmentExpression(lhs,rhs))
        }
        case _ => ExpressionStatement(e)
      }

      case FunctionCall(f, args, _) =>
        // update args list with new values of VarRefs
        var lst = List[GenericAST.AstNode]()
        for (arg <- args)
        {
          arg match
          {
            case VarRef(v_b, s_b, ai_b, vi_b) if ai_b.isEmpty =>
              var vr = VarRef(v_b, s_b, ai_b, vi_b)
              if (oclVarDeclMap.contains(v_b))
              {
                val idxSuffix = getIndexSuffix(s_b.getOrElse(""))
                if (idxSuffix._1 < 0) // This means there is no suffix attached - must use whole unrolled Tuple!
                {
                  var newStruct: AstNode = recreateStruct(oclVarDeclMap(v_b), ai_b, vi_b, oclTupleTypeMap(v_b))
                  lst = lst :+ newStruct
                }
                else
                {
                  val ocl = oclVarDeclMap(v_b)(idxSuffix._1)
                  vr = VarRef(ocl.v, Some(idxSuffix._2), ai_b, vi_b)
                  lst = lst :+ vr
                }


              }
              else {
                lst = lst :+ vr
              }
            case an: AstNode =>
              lst = lst :+ an
          }
        }
        FunctionCall(f, lst)

      case StructConstructor(t, args) =>
        var newargs = Vector[GenericAST.AstNode]()
        for (arg <- args)
        {
          arg match
          {
            case VarRef(v_b, s_b, ai_b, vi_b) if ai_b.isEmpty =>

              var vr = VarRef(v_b, s_b, ai_b, vi_b)
              if (oclVarDeclMap.contains(v_b))
              {
                val idxSuffix = getIndexSuffix(s_b.getOrElse(""))
                if (idxSuffix._1 < 0) // This means there is no suffix attached - must use whole unrolled Tuple!
                {
                  var newStruct: AstNode = recreateStruct(oclVarDeclMap(v_b), ai_b, vi_b, oclTupleTypeMap(v_b))
                  newargs = newargs :+ newStruct
                }
                else
                {
                  val ocl = oclVarDeclMap(v_b)(idxSuffix._1)
                  vr = VarRef(ocl.v, Some(idxSuffix._2), ai_b, vi_b)
                  newargs = newargs :+ vr
                }
              }
              else {
                newargs = newargs :+ vr
              }

            case _ =>
              newargs = newargs :+ arg
          }
        }
        StructConstructor(t, newargs)

      case VarRef(v, s, ai, vi) =>
        if (oclVarDeclMap.contains(v))
        {
          // get actual reference
          val idxSuffix = getIndexSuffix(s.getOrElse(""))
          if(idxSuffix._1 < 0 ) throw new Exception("Unknown reference to unrolled tuple!")
          val ocl = oclVarDeclMap(v)(idxSuffix._1)
          VarRef(ocl.v, Some(idxSuffix._2), ai, vi)
        }
        else VarRef(v, s, ai, vi)
      case _ => n
    }

    node.visitAndRebuild(preFunctionForUnrollingStructs, idPostFun)

  }

}


