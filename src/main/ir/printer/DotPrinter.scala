package ir.printer

import java.io.Writer

import ir.ast._


/**
  * @author cdubach
  */
class DotPrinter(w: Writer, compressLambda : Boolean = true) {

  // keeps track of the visited node
  lazy val visited : collection.mutable.Map[Any, Int] = collection.mutable.HashMap()
  lazy val counters : collection.mutable.Map[Param, Int]  = collection.mutable.HashMap()

  val nodesId : collection.mutable.Map[IRNode, String] = collection.mutable.HashMap()

  def writeln(s: String) = {
    w.write(s+"\n")
  }

  def getNodeId(n: Any) = {
    "n"+Math.abs(n.hashCode()) + visited.get(n).get
  }

  def print(node: IRNode) = {
    writeln("digraph{")
    writeln("ratio=\"compress\"")
    writeln("size=8")
    writeln("margin=\"0.0,0.0\"")
    node match {
      case l: Lambda => countParams(l.body)
      case fp: FPattern => countParams(fp.f.body)
      case e: Expr => countParams(e)
    }
    printNodes(node)
    visited.clear() // start counting again to associate the right nodes with the right edges
    printEdges(node, "", "")
    writeln("}")

    w.flush()
    w.close()
  }


  def printEdges(node : IRNode, parent: String, label: String, attr: String = "") : Unit = {
    if (!(visited.contains(node) && !(node.isInstanceOf[FunDecl] || node.isInstanceOf[Value]))) {
      visited.put(node, visited.getOrElse(node, 0) + 1)
    }

    val nodeId = getNodeId(node)


    node match {
      case fc: FunCall =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
        fc.args.zipWithIndex.foreach(p=> printEdges(p._1, nodeId, "arg_"+p._2))//, ",color=Red"))
        printEdges(fc.f, nodeId,"f")//, ",constraint=false")
        //writeln(parent+"->"+getNodeId(fc.f)+"[color=red]")//style=\"invis\"]")

//        printEdges(fc.f, nodeId,"f", ",rank=same")



      /*        writeln("edge [samehead=h]")
              writeln (getNodeId(fc.args(0))+" -> "+getNodeId(fc.f)+ " [color=Red, constraint=false]")
              writeln (nodeId + "->" + getNodeId(fc.f)+ " [dir=back,color=Red, constraint=false]")
              writeln("edge [samehead=\"\"]")*/

        // constraint = false makes sure the data flow edge do not influence the layout
        /*writeln("edge [samehead=h]")
        writeln (getNodeId(fc.args(0))+" -> "+getNodeId(fc)+ " [color=Red, constraint=false]")
        writeln (getNodeId(fc.f) + "->" + getNodeId(fc)+ " [dir=back,color=Red, constraint=false]")
        writeln("edge [samehead=\"\"]")

        writeln (getNodeId(fc.f) + "->" + getNodeId(fc)+ " [color=Red, constraint=false]")
*/
      case p : Param =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
      case l: Lambda =>
        if (compressLambda)
          l.body match {
            case fc: FunCall =>
              if (fc.args.length == l.params.length)
                if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.get(p._2).get <= 2).forall(identity)) {
                  printEdges(fc.f, parent, "f")//, ",style=dashed")
                  return
                }
          }
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
        l.params.zipWithIndex.foreach(p => printEdges(p._1, nodeId, "param_"+p._2))

       /* subgraph step1 {
                style=filled;
                node [label="Compiler"] step1_Compiler;
                node [label="Maschine"] step1_Maschine;
                color=lightgrey;
        }*/



        printEdges(l.body, nodeId, "body")//, ",constraint=false")
        //if (!parent.equals(""))
        //  writeln(parent+"->"+getNodeId(l.body)+"[color=red]")//+"[style=\"invis\"]")

      case z: Zip =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
      case p: Pattern =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case j : Join =>
          case s : Split =>
        }
      case uf: UserFun =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
    }
  }

  def printNodes(node: IRNode): Unit = {

    if (visited.contains(node) && !(node.isInstanceOf[FunDecl] || node.isInstanceOf[Value]))
      return
    visited.put(node, visited.getOrElse(node, 0)+1)

    val nodeId = getNodeId(node)


    node match {
      case fc: FunCall =>

        fc.f match {
          case fp: FPattern =>
            /*
            visited.put(fp, visited.getOrElse(fp, 0)+1)
            writeln("subgraph {")
            writeln("rank=\"same\"")
            writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>>]")
            writeln(getNodeId(fp) + " [style=rounded,shape=box,label=<<b>" + fp.getClass.getSimpleName + "</b>>]")
            writeln("}")
            fc.args.foreach(printNodes)

            printNodes(fp.f)

            return*/
          case p : Pattern =>

            writeln("subgraph {")
            writeln("rank=\"same\"")
            writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>>]")
            printNodes(p)
            writeln("}")

            fc.args.foreach(printNodes)

            return
          case _ =>
        }

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
            if (compressLambda)
              if (fc.args.length == l.params.length)
                if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.getOrElse(p._2, 0) <= 2).forall(identity)) {
                  printNodes(fc.f)
                  return
                }

            l.params.foreach(p => printNodes(p))

            visited.put(fc, visited.getOrElse(fc, 0)+1)

            // put the lambda and its body in the same subgraph so that they are side by side
            writeln("subgraph {")
            writeln("rank=\"same\"")
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
            writeln(getNodeId(fc)+" [style=rounded,shape=box,label=<<b>"+fc.getClass.getSimpleName+"</b>>]")
            writeln("}")

            fc.args.foreach(printNodes)
            printNodes(fc.f)

            return
        }

       /* writeln("subgraph {")
        writeln("rank=\"same\"")*/
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
        l.params.foreach(p => printNodes(p))
        /*writeln("}")*/

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
      writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")

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
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
        l.params.zipWithIndex.foreach(p => printEdges(p._1, nodeId, "param_"+p._2))
        printEdges(l.body, nodeId, "body")
      case z: Zip =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
      case p: Pattern =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case j : Join =>
          case s : Split =>
        }
      case uf: UserFun =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
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