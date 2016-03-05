package ir.printer

import java.io.Writer

import ir.ast._
import opencl.ir.pattern._


/**
  * @author cdubach
  */
class DotPrinter(w: Writer) {

  // keeps track of the visited node
  lazy val visited : collection.mutable.Set[Any] = collection.mutable.HashSet()
  lazy val counters : collection.mutable.Map[Param, Int]  = collection.mutable.HashMap()

  def writeln(s: String) = {
    w.write(s+"\n")
  }

  def getNodeId(n: Any) = {
    "n"+Math.abs(n.hashCode())
  }

  def print(node: IRNode) = {
    writeln("digraph{")
    node match {
      case l: Lambda => countParams(l.body)
      case fp: FPattern => countParams(fp.f.body)
      case e: Expr => countParams(e)
    }
    printNodes(node)
    printEdges(node, "", "")
    writeln("}")

    w.flush()
    w.close()
  }


  def printEdges(node : IRNode, parent: String, label: String, attr: String = "") : Unit = {

    val nodeId = getNodeId(node)


    node match {
      case fc: FunCall =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
        fc.args.zipWithIndex.foreach(p=> printEdges(p._1, nodeId, "arg_"+p._2, ",color=Red"))
        printEdges(fc.f, nodeId,"f")
      case p : Param =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
      case l: Lambda =>
        l.body match {
          case fc: FunCall =>
            if (fc.args.length == l.params.length)
              if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.getOrElse(p._2,0) <= 2).fold(true)(_ && _)) {
                printEdges(fc.f, parent, "o", ",color=Blue, style=dashed")
                return
              }
        }
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
        l.params.zipWithIndex.foreach(p => printEdges(p._1, nodeId, "param_"+p._2))
        printEdges(l.body, nodeId, "body")
      case z: Zip =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
      case p: Pattern =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case j : Join =>
          case s : Split =>
        }
      case uf: UserFun =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
    }
  }

  def printNodes(node: IRNode): Unit = {

    if (visited.contains(node))
      return
    visited.add(node)

    val nodeId = getNodeId(node)


    node match {
      case fc: FunCall =>
        writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>>]")
        fc.args.foreach(printNodes)
        printNodes(fc.f)
      case v: Value =>
        writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>("+v.value+")>]")
      case p: Param =>
        writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>>]")

      case l: Lambda =>

        l.body match {
          case fc: FunCall =>
            if (fc.args.length == l.params.length)
              if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.getOrElse(p._2,0) <= 2).fold(true)(_ && _)) {
                printNodes(fc.f)
                return
              }
        }

        writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
        l.params.foreach(p => printNodes(p))
        printNodes(l.body)
      case z: Zip =>
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
      case p: Pattern =>
        p match {
          case fp: FPattern =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
            printNodes(fp.f)
          case j : Join =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
          case s : Split =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>("+s.chunkSize+")>]")
        }
      case uf: UserFun =>
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>UserFun</b>("+uf.name+")>]")

    }
  }


/*  def print(decl: Lambda) = {
    writeln("digraph{")
    countParams(decl.body)
    printNodes(decl)
    printEdges(decl, "", "")
    writeln("}")

    w.flush()
    w.close()
  }

  def print(expr: Expr) = {
    writeln("digraph{")
    countParams(expr)
    printNodes(expr)
    printEdges(expr, "", "")
    writeln("}")

    w.flush()
    w.close()
  }*/

  def countParams(expr: Expr) = {
    Expr.visit(expr,
      {
        case p: Param => counters.put(p, counters.getOrElse(p, 0) + 1)
        case _ =>
      }
      , post => {})
  }
/*

  def printEdges(expr : Expr, parent: String, label: String, attr: String = "") : Unit = {

    val nodeId = getNodeId(expr)

    if (!parent.equals(""))
      writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")

    expr match {
      case fc: FunCall =>
        fc.args.zipWithIndex.foreach(p=> printEdges(p._1, nodeId, "arg_"+p._2))
        printEdges(fc.f, nodeId,"f")
      case p : Param =>
    }
  }

  def printEdges(decl : Decl, parent: String, label: String, attr: String = "") : Unit = {

    val nodeId = getNodeId(decl)

    decl match {
      case l: Lambda =>
        l.body match {
          case fc: FunCall =>
            if (fc.args.length == l.params.length)
              if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.getOrElse(p._2,0) <= 2).fold(true)(_ && _)) {
                printEdges(fc.f, parent, "o", ",color=Blue, style=dashed")
                return
              }
        }
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
        l.params.zipWithIndex.foreach(p => printEdges(p._1, nodeId, "param_"+p._2))
        printEdges(l.body, nodeId, "body")
      case z: Zip =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
      case p: Pattern =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case j : Join =>
          case s : Split =>
        }
      case uf: UserFun =>
        if (!parent.equals(""))
          writeln (nodeId+" -> "+parent+" [label=\""+label+"\""+attr+"];")
    }
  }

  def printNodes(expr: Expr): Unit = {

    if (visited.contains(expr))
      return
    visited.add(expr)

    val nodeId = getNodeId(expr)


    expr match {
      case fc: FunCall =>
        writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + expr.getClass.getSimpleName + "</b>>]")
        fc.args.foreach(printNodes)
        printNodes(fc.f)
      case v: Value =>
        writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + expr.getClass.getSimpleName + "</b>("+v.value+")>]")
      case p: Param =>
        writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + expr.getClass.getSimpleName + "</b>>]")
    }
  }

  def printNodes(decl : Decl) : Unit = {

    if (visited.contains(decl))
      return
    visited.add(decl)

    val nodeId = getNodeId(decl)

    //writeln(nodeId+" [style=rounded,shape=box,label=\""+decl.getClass.getSimpleName+"\"]")

    decl match {
      case l: Lambda =>

        l.body match {
          case fc: FunCall =>
            if (fc.args.length == l.params.length)
              if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.getOrElse(p._2,0) <= 2).fold(true)(_ && _)) {
                printNodes(fc.f)
                return
              }
        }

        writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+decl.getClass.getSimpleName+"</b>>]")
        l.params.map(p => printNodes(p))
        printNodes(l.body)
      case z: Zip =>
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+decl.getClass.getSimpleName+"</b>>]")
      case p: Pattern =>
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+decl.getClass.getSimpleName+"</b>>]")
        p match {
          case fp: FPattern =>
            printNodes(fp.f)
          case j : Join =>
          case s : Split =>
        }
      case uf: UserFun =>
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>UserFun</b>("+uf.name+")>]")

  }
  }
*/

}