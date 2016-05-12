package ir.ast

import apart.arithmetic.{ArithExpr, Var}
import ir._
import opencl.ir.{OpenCLAddressSpace, UndefAddressSpace}
import opencl.ir.ast.OpenCLBuiltInFun
import opencl.ir.pattern.{LSearch, _}


trait IRNode {
}

object IRNode {

  /*
   * pre and post should not forget to set the type
   */
  def visit(n: IRNode,
            pre: IRNode => IRNode,
            post: IRNode => IRNode) : IRNode = {
    val nPre = pre(n)
    val newNode = nPre match {

      case fc: FunCall => new FunCall(visit(fc.f,pre,post).asInstanceOf[FunDecl],fc.args.map(visit(_,pre,post).asInstanceOf[Expr]):_*)

      case vec: VectorizeUserFun => new VectorizeUserFun(vec.n, visit(vec.userFun,pre,post).asInstanceOf[UserFun])
      case u : UserFun => u

      case l: Lambda => Lambda(l.params.map(visit(_,pre,post).asInstanceOf[Param]),visit(l.body,pre,post).asInstanceOf[Expr])

      case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
           Split(_) | Join() | Group(_) | Zip(_) | Tuple(_) | Filter() |
           Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_,_) | Value(_) => nPre // nothing to visit here

      case p: Param => nPre // nothing to visit here

      case fp : FPattern => fp.copy(visit(fp.f,pre,post).asInstanceOf[Lambda])
    }
    // set the type
    (nPre,newNode) match{
      case (e1:Expr,e2:Expr) => e2.t = e1.t
      case _=>
    }
    post(newNode)
  }

  def visit(n: IRNode,
            pre: IRNode => Unit,
            post: IRNode => Unit = _ => {}) : Unit = {
    pre(n)
    n match {

      case fc: FunCall =>
        visit(fc.f,pre,post)
        fc.args.foreach(visit(_,pre,post))

      case vec: VectorizeUserFun => visit(vec.userFun,pre,post)
      case u : UserFun =>

      case l: Lambda =>
        l.params.foreach(visit(_,pre,post))
        visit(l.body,pre,post)

      case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
           Split(_) | Join() | Group(_) | Zip(_) | Tuple(_) | Filter() |
           Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Pad(_,_) | Value(_) =>  // nothing to visit here

      case p: Param =>  // nothing to visit here

      case fp : FPattern => visit(fp.f,pre,post)
    }
    post(n)
  }


  def visitArithExpr(n: IRNode, f: ArithExpr => ArithExpr): IRNode = {

    visit(n, {
      // for expression we mainly just have to rebuild the type
      case e: Expr =>
        val newType = Type.visitAndRebuild(e.t, f)
        val newExpr = e match {
          case vp: VectorParam => new VectorParam(vp.p, f(vp.n))
          case p: Param => p
          case v: Value => v
          case fc: FunCall => fc
        }
        newExpr.t = newType
        newExpr

      case m: MapAtomWrg => new MapAtomWrg(m.dim, m.f, f(m.workVar).asInstanceOf[Var])
      case m: MapAtomLcl => new MapAtomLcl(m.dim, m.f, f(m.workVar).asInstanceOf[Var])

      case i: Iterate => new Iterate(f(i.n), i.f)

      case vec: VectorizeUserFun => new VectorizeUserFun(f(vec.n), vec.userFun)

      case u: OpenCLBuiltInFun => new OpenCLBuiltInFun(u.name, u.inTs.map(Type.visitAndRebuild(_, f)), Type.visitAndRebuild(u.outT, f))
      case u: UserFun => new UserFun(u.name, u.paramNames, u.body, u.inTs.map(Type.visitAndRebuild(_, f)), Type.visitAndRebuild(u.outT, f))

      case a: asVector => new asVector(f(a.len))
      case a: asScalar => new asScalar()

      case s: Split => new Split(f(s.chunkSize))

      case s: Scatter => new Scatter(new IndexFunction((ae: ArithExpr, t: Type) => f(s.idx.f.apply(ae, t))))
      case g: Gather => new Gather(new IndexFunction((ae: ArithExpr, t: Type) => f(g.idx.f.apply(ae, t))))

      case p: Pad => new Pad(p.offset, (ae1: ArithExpr, ae2: ArithExpr) => f(p.boundary.apply(ae1, ae2)))

      case x@(MapGlb(_, _) | MapLcl(_, _) | MapWarp(_) | MapLane(_) | MapSeq(_) | MapWrg(_,_) | Map(_) |
              ReduceSeq(_) | BSearch(_) | LSearch(_) | FunCall(_, _) | Lambda(_, _) |
              Unzip() | Transpose() | TransposeW() | Join() | Group(_) | Zip(_) | Tuple(_) | Filter() |
              Head() | Tail() | Get(_) | toGlobal(_) | toLocal(_) | toPrivate(_)) => x

    }
      , x => x)
  }



}
