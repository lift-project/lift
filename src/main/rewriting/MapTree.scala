package rewriting.MapTree
import ir.ast._
import rewriting.utils.NumberExpression


class MapTree{
  val NodesMap = collection.mutable.Map[MapTreeNode,Int]()
  val RootNode = new MapTreeNode
  var MaxAvaDepth:Int = 1000000
  var MaxDepth:Int = 0
  def apply(l:Lambda)={
    parse(l)
  }
  def parse(l:Lambda):Unit ={
    val numbering = NumberExpression.breadthFirst(l)
    build(RootNode,l.body,1)(numbering)
  }

  def build(parentNode:MapTreeNode,expr:Expr,depth:Int)(implicit numbering:collection.Map[Expr,Int]):Unit =  {
    expr match{
      case call:FunCall =>

        call.f match{

          case lambda:Lambda =>
            build(parentNode,lambda.body,depth)

          case map:ir.ast.Map =>
            val currNode = new MapTreeNode
            currNode.Depth = depth
            if(MaxDepth < depth)
              MaxDepth = depth

            build(currNode,map.f.body,depth+1)

            parentNode.Child += currNode
            NodesMap += (currNode -> numbering(expr))

          case fp:FPattern => build(parentNode,fp.f.body,depth)

          case uf:UserFun =>
            parentNode.shouldConsequence = true
            if(MaxAvaDepth >= depth)
              MaxAvaDepth = depth -1

          case _=>
        }
        call.args.foreach(build(parentNode,_,depth))
      case _=>
    }
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
}
class MapTreeNode{
  var Depth:Int = 0
  val Child = collection.mutable.ArrayBuffer[MapTreeNode]()
  var shouldConsequence = false

}
