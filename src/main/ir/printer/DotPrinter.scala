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

  def print(decl: Lambda) = {
    writeln("digraph{");
    countParams(decl.body)
    printNodes(decl)
    printEdges(decl, "", "")
    writeln("}")

    w.flush()
    w.close()
  }

  def print(expr: Expr) = {
    writeln("digraph{");
    countParams(expr)
    printNodes(expr)
    printEdges(expr, "", "")
    writeln("}")

    w.flush()
    w.close()
  }

  def countParams(expr: Expr) = {
    Expr.visit(expr,
      pre => pre match {
        case p : Param => counters.put(p,counters.getOrElse(p,0)+1)
        case _ =>
      }
      , post => {})
  }

  def printEdges(expr : Expr, parent: String, label: String) : Unit = {

    val nodeId = getNodeId(expr)

    if (!parent.equals(""))
      writeln (nodeId+" -> "+parent+" [label=\""+label+"\"];")

    expr match {
      case fc: FunCall =>
        fc.args.zipWithIndex.foreach(p=> printEdges(p._1, nodeId, "arg_"+p._2))
        printEdges(fc.f, nodeId,"f")
      case p : Param =>
    }
  }

  def printEdges(decl : Decl, parent: String, label: String) : Unit = {

    val nodeId = getNodeId(decl)



    decl match {
      case l: Lambda =>

        l.body match {
          case fc: FunCall =>
            if (fc.args.length == l.params.length)
              if (fc.args.zip(l.params).map(p => p._1 == p._2 && counters.getOrElse(p._2,0) <= 2).fold(true)(_ && _)) {
                printEdges(fc.f, parent, "f")
                return
              }
        }
        if (!parent.equals("")) writeln (nodeId+" -> "+parent+" [label=\""+label+"\"];")
        l.params.zipWithIndex.foreach(p => printEdges(p._1, nodeId, "param_"+p._2))
        printEdges(l.body, nodeId, "body")
      case z: Zip =>
        if (!parent.equals("")) writeln (nodeId+" -> "+parent+" [label=\""+label+"\"];")
      case p: Pattern =>
        if (!parent.equals("")) writeln (nodeId+" -> "+parent+" [label=\""+label+"\"];")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case j : Join =>
          case s : Split =>
        }
      case uf: UserFun =>
        if (!parent.equals("")) writeln (nodeId+" -> "+parent+" [label=\""+label+"\"];")
    }
  }

    def printNodes(expr : Expr) : Unit = {

      if (visited.contains(expr))
        return
      visited.add(expr)

    val nodeId = getNodeId(expr)


      writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+expr.getClass.getSimpleName+"</b>>]")

    expr match {
      case fc: FunCall =>
        fc.args.foreach(printNodes)
        printNodes(fc.f)
      case p : Param =>
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

}
/*


\node               (applyRS) {\textbf{apply}};
  \node               (join)  [below of=applyRS]         {\textbf{join}};
  \node               (reduceSeq)  [right of=applyRS, node distance=2cm]   {\textbf{reduceSeq}};
  \node[draw=none] (dummy1) [below of=reduceSeq] {};
  \node               (LRS) [below of=reduceSeq] {$\lambda$};
  \node               (inputLRS) [below of=LRS] {input};
  \node               (applyLRS) [right of=inputLRS] {apply};
  \node[draw=none] (dummy2) [below of=applyLRS] {};
  \node               (add)  [right of=applyLRS]         {$+$};

  \node               (applyMW) [below of=join] {\textbf{apply}};
  \node               (split128)  [below of=applyMW]         {$\textbf{split}^{128}$};
  \node               (mapWrg)  [right of=split128]   {\textbf{mapWrg}};
  \node               (zip) [below of=split128]             {\textbf{zip}};
  \node               (input1)  [below left of=zip]                      {input};
  \node               (input2)  [below right of=zip]         {input};
  \node[fill=blue!10]               (dotProdInWrg)  [right of=zip, node distance=2cm]   {dotProdInWrg};

  \begin{scope}[every node/.style={scale=.75}]
  \path[->]
  (input1)     edge                  node[left]           {in1}         (zip)
  (input2)     edge                  node[right]           {in2}         (zip)
  (zip)        edge                  node           {in}          (split128)
  (split128)   edge                  node           {in}          (applyMW)
  (applyMW)    edge                  node           {in}          (join)
  (mapWrg)     edge                  node[right]           {f}          (applyMW)
  (join)       edge                  node           {in}          (applyRS)
  (reduceSeq)    edge                  node            {f}          (applyRS)
  (add)        edge                  node           {f}           (applyLRS)
  (dotProdInWrg) edge                node[right]           {f}           (mapWrg)
  (LRS) edge node {l} (reduceSeq)
  (inputLRS) edge node {p} (LRS)
  (inputLRS) edge [bend right=90] node {in} (applyLRS)
  %(inputLRS) edge [style={-}] node {p} (LRS)
  (applyLRS) edge node {in} (LRS);
  \end{scope}*/
