package rewriting.MapTree
import ir.ast._


class MapTree{
  val NodesMap = collection.mutable.Map[MapTreeNode,Expr]()
  val RootNode = new MapTreeNode
  def apply(l:Lambda)={
    parse(l)
  }
  def parse(l:Lambda):Unit ={
    build(RootNode,l.body,1)
  }
  /*def build(parentNode: MapTreeNode,funDecl:FunDecl,depth:Int):Unit = {

    funDecl match{
      case lambda:Lambda => build(parentNode,lambda.body,depth)
      case map:ir.ast.Map =>
        val currNode = new MapTreeNode
        currNode.Depth = depth
        build(currNode,map.f,depth+1)
        parentNode.Child += currNode

      case fp:FPattern => build(parentNode,fp.f,depth)
      case uf:UserFun => parentNode.shouldConsequence = true

    }
  }*/
  def build(parentNode:MapTreeNode,expr:Expr,depth:Int):Unit =  {
    expr match{
      case call:FunCall =>
        call.f match{
          case lambda:Lambda => build(parentNode,lambda.body,depth)
          case map:ir.ast.Map =>
            val currNode = new MapTreeNode
            currNode.Depth = depth
            build(currNode,map.f.body,depth+1)
            parentNode.Child += currNode
            NodesMap += (currNode -> expr)
          case fp:FPattern => build(parentNode,fp.f.body,depth)
          case uf:UserFun => parentNode.shouldConsequence = true
          case _=>
        }
        call.args.foreach(build(parentNode,_,depth))
      case _=>
    }
  }

}
class MapTreeNode{
  var Depth:Int = 0
  val Child = collection.mutable.ArrayBuffer[MapTreeNode]()
  var shouldConsequence = false

}
