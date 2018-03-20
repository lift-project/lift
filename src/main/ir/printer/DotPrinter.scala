package ir.printer

import java.io.{File, PrintWriter, Writer}

import scala.sys.process._
import ir.ast._
import rewriting.utils.NumberExpression


/**
  * @author cdubach
  */
object DotPrinter {
  def apply(name: String, f: Lambda): Unit = {
    val home = System.getProperty("user.home")
    this.apply(home, name, f)
  }

  def apply(path: String, name: String, f: Lambda): Unit = {
    new DotPrinter(new PrintWriter(new File(s"$path/$name.dot"))).print(f)
    s"dot -Tpdf $path/$name.dot -o $path/$name.pdf".!
  }

  def withNumbering(path: String, name: String, f: Lambda, compress : Boolean = true): Unit = {
    val numbering = NumberExpression.breadthFirst(f)
    new DotPrinter(new PrintWriter(new File(s"$path/$name.dot")), compress, false, false, numbering).print(f)
    s"dot -Tpdf $path/$name.dot -o $path/$name.pdf".!
  }
}

class DotPrinter(w: Writer,
                 compressLambda : Boolean = true,
                 printAddressSpace : Boolean = false,
                 printRef : Boolean = false,
                 numbering : scala.collection.Map[Expr, Int] = scala.collection.Map()
                ) {

  // keeps track of the visited node
  lazy val visited : collection.mutable.Map[Any, Int] = collection.mutable.HashMap()
  lazy val counters : collection.mutable.Map[Param, Int]  = collection.mutable.HashMap()

  val nodesId : collection.mutable.Map[IRNode, String] = collection.mutable.HashMap()



  def writeln(s: String): Unit = {
    w.write(s+"\n")
  }

  def getNodeId(n: Any) = {
    "n"+Math.abs(n.hashCode()) + visited.get(n).get
  }

  def print(node: IRNode): Unit = {
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
      case Unzip() =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
      case p: Pattern =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
        p match {
          case fp: FPattern =>
            printEdges(fp.f, nodeId, "f")
          case _ =>
        }
      case _ =>
        if (!parent.equals(""))
          writeln (parent+" -> "+nodeId+" [label=\""+label+"\""+attr+"];")
    }
  }

  def writeNodeDef(e: Expr): Unit = {
    val addrSpce = if (printAddressSpace)
      ":addrSpce("+e.addressSpace+")"
      else
        ""

    val number = if (numbering.contains(e))
      "<BR/><i>" + numbering(e).toString() + "</i>"
    else
      ""

    val ref = if (printRef)
      "@"+e.##
    else
      ""
    writeln(getNodeId(e) + " [style=rounded,shape=box,label=<<b>" + e.getClass.getSimpleName + "</b>"+ ref+addrSpce + number + ">]")
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
          case p : Pattern =>

            writeln("subgraph {")
            writeln("rank=\"same\"")
            writeNodeDef(fc)
            //writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>"+ {if (printAddressSpace) {"("+fc.addressSpaces+")"} else ""} +">]")
            printNodes(p)
            writeln("}")

            fc.args.foreach(printNodes)

            return
          case _ =>
        }
        writeNodeDef(fc)
        //writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>"+ {if (printAddressSpace) {"("+fc.addressSpaces+")"} else ""} +">]")
        fc.args.foreach(printNodes)
        printNodes(fc.f)

      case v: Value =>
        val number = if (numbering.contains(v))
          "<BR/><i>" + numbering(v).toString() + "</i>"
        else
          ""
        writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>("+v.value+")" + number + ">]")
      case p: Param =>
        writeNodeDef(p)
        //writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>>]")

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
            writeNodeDef(fc)
            //writeln(getNodeId(fc)+" [style=rounded,shape=box,label=<<b>"+fc.getClass.getSimpleName+"</b>"+ {if (printAddressSpace) {"("+fc.addressSpaces+")"} else ""} +">]")
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

      case p: Pattern =>
        p match {
          case fp: FPattern =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
            printNodes(fp.f)
          case Split(chunkSize) =>
            writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>(" + chunkSize + ")>]")
          case Slide(size, step) =>
            writeln(nodeId + " [style=rounded,shape=box,label=<<b>" + node.getClass.getSimpleName + "</b>(" + size + ", " + step + ")>]")
          case Get(i) =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>("+i+")>]")
          case t: Tuple =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>Tuple</b>"+t.n+">]")
          case z: Zip =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
          case u: Unzip =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>Unzip</b>>]")
          case  _ =>
            writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
        }
      case uf: UserFun =>
        val number = numbering.find{
          case (expr, _) => println(expr.getClass.getName); expr match {
            case FunCall(u : UserFun, _*) if uf == u => true
            case _ => false
          }
        }.map(x => "<BR/><i>" + x._2.toString + "*</i>").getOrElse("")
        val print = if (compressLambda) number else ""
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>UserFun</b>("+uf.name+")" + print + ">]")
      case  _ =>
        writeln(nodeId+" [style=rounded,shape=box,label=<<b>"+node.getClass.getSimpleName+"</b>>]")
    }
  }



  def countParams(expr: Expr): Unit = {
    Expr.visit(expr,
      {
        case p: Param => counters.put(p, counters.getOrElse(p, 0) + 1)
        case _ =>
      }
      , post => {})
  }
}
