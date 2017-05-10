package ir.ast

import lift.arithmetic.{ArithExpr, Var}
import ir._
import ir.ast.Pad.BoundaryFun
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

      case fc: FunCall => FunCall(visit(fc.f,pre,post).asInstanceOf[FunDecl],fc.args.map(visit(_,pre,post).asInstanceOf[Expr]):_*)

      case vec: VectorizeUserFun => VectorizeUserFun(vec.n, visit(vec.userFun,pre,post).asInstanceOf[UserFun])
      case u : UserFun => u

      case l: Lambda => Lambda(l.params.map(visit(_,pre,post).asInstanceOf[Param]),visit(l.body,pre,post).asInstanceOf[Expr])

      case Unzip() | Transpose() | TransposeW() | asVector(_) | asScalar() |
           Split(_) | Join() | Zip(_) | Tuple(_) | Filter() |
           Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Slide(_, _) | Pad(_,_,_) | Value(_) => nPre // nothing to visit here

      case _: Param | _:ArrayAccess => nPre // nothing to visit here

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
           Split(_) | Join() | Zip(_) | Tuple(_) | Filter() |
           Head() | Tail() | Scatter(_) | Gather(_) | Get(_) | Slide(_, _) |
           Pad(_,_,_) | Value(_) | UnsafeArrayAccess(_) | CheckedArrayAccess(_,_) | Id() =>  // nothing to visit here

      case _: Param | _:ArrayAccess | _: ArrayConstructors =>  // nothing to visit here

      case fp : FPattern => visit(fp.f,pre,post)
    }
    post(n)
  }


  def visitArithExpr(n: IRNode, f: ArithExpr => Unit): Unit = {

    visit(n, {
      // for expression we mainly just have to rebuild the type
      case e: Expr =>
        e match {
          case vp: VectorParam => f(vp.n)
          case _: Param | _:FunCall =>
          case _: ArrayConstructors =>
        }

      case m: MapAtomWrg => f(m.workVar)
      case m: MapAtomLcl => f(m.workVar)

      case i: Iterate => f(i.n)

      case vec: VectorizeUserFun => f(vec.n)

      case u: OpenCLBuiltInFun =>
        u.inTs.foreach(Type.visit(_, f))
        Type.visit(u.outT, f)

      case u: UserFun =>
        u.inTs.foreach(Type.visit(_, f))
        Type.visit(u.outT, f)

      case a: asVector => f(a.n)
      case a: asScalar =>

      case s: Split => f(s.chunkSize)

      case s: Scatter => // TODO: figure out how to find ArithExpr in the index function
      case g: Gather => // TODO: figure out how to find ArithExpr in the index function

      case p: Pad => // TODO: figure out how to find ArithExpr in the boundary function function

      case ArrayAccess(idx) => f(idx)

      case x@(MapGlb(_, _) | MapLcl(_, _) | MapWarp(_) | MapLane(_) | MapSeq(_) | MapWrg(_,_) | Map(_) |
              Reduce(_) | PartRed(_) | ReduceSeq(_) | ReduceWhileSeq(_,_) | BSearch(_) | LSearch(_) | FunCall(_, _) | Lambda(_, _) |
              Unzip() | Transpose() | TransposeW() | Join() | Slide(_, _) | Zip(_) | Tuple(_) | Filter() |
              Head() | Tail() | Get(_) | toGlobal(_) | toLocal(_) | toPrivate(_) | Id() | UnsafeArrayAccess(_) | CheckedArrayAccess(_,_) ) =>

    })
  }



  def visitArithExpr(n: IRNode, f: ArithExpr => ArithExpr): IRNode = {

    visit(n, {
      // for expression we mainly just have to rebuild the type
      case e: Expr =>
        val newType = Type.visitAndRebuild(e.t, f)
        val newExpr = e match {
          case vp: VectorParam => new VectorParam(vp.p, f(vp.n))
          case p: Param => p
          case fc: FunCall => fc
        }
        newExpr.t = newType
        newExpr

      case m: MapAtomWrg => new MapAtomWrg(m.dim, m.f, f(m.workVar).asInstanceOf[Var])
      case m: MapAtomLcl => new MapAtomLcl(m.dim, m.f, f(m.workVar).asInstanceOf[Var])

      case i: Iterate => new Iterate(f(i.n), i.f)

      case vec: VectorizeUserFun => VectorizeUserFun(f(vec.n), vec.userFun)

      case u: OpenCLBuiltInFun => new OpenCLBuiltInFun(u.name, u.inTs.map(Type.visitAndRebuild(_, f)), Type.visitAndRebuild(u.outT, f))
      case u: UserFun => new UserFun(u.name, u.paramNames, u.body, u.inTs.map(Type.visitAndRebuild(_, f)), Type.visitAndRebuild(u.outT, f))

      case a: asVector => asVector(f(a.n))
      case a: asScalar => asScalar()

      case s: Split => Split(f(s.chunkSize))

      case s: Scatter => Scatter(new IndexFunction((ae: ArithExpr, t: Type) => f(s.idx.f.apply(ae, t))))
      case g: Gather => Gather(new IndexFunction((ae: ArithExpr, t: Type) => f(g.idx.f.apply(ae, t))))

      case p: Pad => new Pad(p.left, p.right,  new BoundaryFun {
        def apply(ae1: ArithExpr, ae2: ArithExpr) = f(p.boundary.apply(ae1, ae2))
      })

      case ArrayAccess(idx) => ArrayAccess(f(idx))


      case x@(MapGlb(_, _) | MapLcl(_, _) | MapWarp(_) | MapLane(_) | MapSeq(_) | MapWrg(_,_) | Map(_) |
              ReduceSeq(_) | PartRed(_) | BSearch(_) | LSearch(_) | FunCall(_, _) | Lambda(_, _) |
              Unzip() | Transpose() | TransposeW() | Join() | Slide(_, _) | Zip(_) | Tuple(_) | Filter() |
              Head() | Tail() | Get(_) | toGlobal(_) | toLocal(_) | toPrivate(_)) => x

    }
      , x => x)
  }



}
