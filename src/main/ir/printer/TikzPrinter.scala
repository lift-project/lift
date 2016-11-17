package ir.printer

import java.io.Writer

import ir.ast._
import opencl.ir.pattern._


/**
  * @author cdubach
  */
class TikzPrinter(w: Writer) {

  // keeps track of the visited node
  lazy val visited : collection.mutable.Set[Any] = collection.mutable.HashSet()

  def writeln(s: String): Unit = {
    w.write(s+"\n")
  }

  def getNodeId(n: Any) = {
    "n"+Math.abs(n.hashCode())
  }

  def print(decl: Decl): Unit = {
    writeln("\\begin{tikzpicture}[baseline,>=stealth',shorten >=1pt,auto,node distance=1.3cm,minimum size=0pt,every node/.style={rectangle,draw=black!80,rounded corners=3pt,thick}]")
    printNodes(decl, "")

    writeln("\\begin{scope}[every node/.style={scale=.75}]")
    writeln("\\path[->]")
    printEdges(decl, "","")
    writeln(";")
    writeln("\\end{scope}")

    writeln("\\end{tikzpicture}")

    w.flush()
    w.close()
  }

  def printEdges(expr : Expr, parent: String, label: String) : Unit = {

    val nodeId = getNodeId(expr)

    if (!parent.equals(""))
      writeln ("("+nodeId+") edge node {"+label+"}("+parent+")")

    expr match {
      case fc: FunCall =>
        printEdges(fc.args.head, nodeId,"arg")
        printEdges(fc.f, nodeId,"f")
      case p : Param =>
    }
  }

  def printEdges(decl : Decl, parent: String, label: String) : Unit = {

    val nodeId = getNodeId(decl)

    if (!parent.equals(""))
      writeln ("("+nodeId+") edge node {"+label+"}("+parent+")")

    decl match {
      case l: Lambda =>
        printEdges(l.params(0), nodeId, "param")
        printEdges(l.body, nodeId, "body")
      case z: Zip =>

      case p: Pattern =>
        p match {
          case t : toLocal =>
            printEdges(t.f, nodeId, "f");
          case t : toGlobal=>
            printEdges(t.f, nodeId, "f");
          case i : Iterate =>
            printEdges(i.f, nodeId, "f");
          case j : Join =>
          case s : Split =>
          case m : MapSeq =>
            printEdges(m.f, nodeId, "f");
          case m : MapWrg =>
            printEdges(m.f, nodeId, "f");
          case m : MapLcl =>
            printEdges(m.f, nodeId, "f");
          case r : ReduceSeq =>
            printEdges(r.f, nodeId, "f");

        }
      case uf: UserFun =>
    }
  }

    def printNodes(expr : Expr, pos: String) : Unit = {

      if (visited.contains(expr))
        return
      visited.add(expr)

    val nodeId = getNodeId(expr)

    expr match {
      case fc: FunCall =>
        writeln("\\node ("+nodeId+") ["+pos+"] {FunCall};")
        printNodes(fc.args.head, "below of="+nodeId)
        printNodes(fc.f, "right of="+nodeId)
      case p : Param =>
        writeln("\\node ("+nodeId+") ["+pos+"] {Param};")
    }
  }

  def printNodes(decl : Decl, pos: String) : Unit = {

    if (visited.contains(decl))
      return
    visited.add(decl)

    val nodeId = getNodeId(decl)

    // first print all the nodes
  decl match {
      case l: Lambda =>
        writeln("\\node ("+nodeId+") ["+pos+"] {$\\lambda$};")
        printNodes(l.params(0), "below of="+nodeId)
        printNodes(l.body, "right of="+getNodeId(l.params(0)))
      case z: Zip =>
        writeln("\\node ("+nodeId+") ["+pos+"] {zip};")
        writeln("\\node ("+nodeId+"0) [below left of="+nodeId+"] {input};")
        writeln("\\node ("+nodeId+"1) [below right of="+nodeId+"] {input};")
      case p: Pattern =>
        p match {
          case t : toLocal =>
            writeln("\\node ("+nodeId+") ["+pos+"] {ToLocal};")
            printNodes(t.f, "below of="+nodeId);
          case t : toGlobal=>
            writeln("\\node ("+nodeId+") ["+pos+"] {ToGlobal};")
            printNodes(t.f, "below of="+nodeId);
          case i : Iterate =>
            writeln("\\node ("+nodeId+") ["+pos+"] {Iterate};")
            printNodes(i.f, "below of="+nodeId);
          case j : Join =>
            writeln("\\node ("+nodeId+") ["+pos+"] {Join};")
            writeln("\\node ("+nodeId+"1) [below of="+nodeId+"] {input};")
          case s : Split =>
            writeln("\\node ("+nodeId+") ["+pos+"] {Split};")
            writeln("\\node ("+nodeId+"1) [below of="+nodeId+"] {input};")

          case m : MapSeq =>
            writeln("\\node ("+nodeId+") ["+pos+"] {MapSeq};")
            printNodes(m.f, "below of="+nodeId);

          case m : MapWrg =>
            writeln("\\node ("+nodeId+") ["+pos+"] {MapWrg};")
            printNodes(m.f, "below of="+nodeId);

          case m : MapLcl =>
            writeln("\\node ("+nodeId+") ["+pos+"] {MapLcl};")
            printNodes(m.f, "below of="+nodeId);

          case r : ReduceSeq =>
            writeln("\\node ("+nodeId+") ["+pos+"] {ReduceSeq};")
            printNodes(r.f,"below of="+nodeId);

        }
      case uf: UserFun =>
        writeln("\\node ("+nodeId+") ["+pos+"] {"+uf.name+"};")

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
