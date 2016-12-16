package rewriting

import ir.ast._
import rewriting.utils.NumberExpression

import scala.collection.mutable.ArrayBuffer


case class ControlMapNum(
                          level1:Boolean,
                          level2:Boolean,
                          level3:Boolean,
                          level4:Boolean
                        )

class MapTree{
  val NodesMap = collection.mutable.Map[MapTreeNode,Int]()
  val RootNode = new MapTreeNode
  var MaxAvaDepth:Int = 1000000
  var MaxDepth:Int = 0

  def apply(l:Lambda) = {
    parse(l)
  }

  def parse(l:Lambda): Unit ={
    val numbering = NumberExpression.breadthFirst(l)
    build(RootNode,l.body,1)(numbering)
  }

  private def build(parentNode:MapTreeNode,expr:Expr,depth:Int)(implicit numbering:collection.Map[Expr,Int]):Unit =  {
    expr match{
      case call:FunCall =>

        call.f match{

          case lambda:Lambda =>
            build(parentNode,lambda.body,depth)

          case map:Map if map.f.body.isConcrete =>
            val currNode = new MapTreeNode
            currNode.Depth = depth
            if(MaxDepth < depth)
              MaxDepth = depth

            build(currNode,map.f.body,depth+1)

            parentNode.Child += currNode
            NodesMap += (currNode -> numbering(expr))

          case fp:FPattern => build(parentNode,fp.f.body,depth)

          case _: UserFun =>
            parentNode.shouldSequential = true
            if(MaxAvaDepth >= depth)
              MaxAvaDepth = depth -1

          case _=>
        }
        call.args.foreach(build(parentNode,_,depth))
      case _=>
    }
  }

  def getMapsIdOnLevel(level:Int):Array[Int] = {
    NodesMap.filterKeys(_.Depth == level).values.toArray[Int]
  }
  def getMapsNodeOnLevel(level:Int):Array[MapTreeNode] = {
    NodesMap.filterKeys(_.Depth == level).keys.toArray[MapTreeNode]
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
  var shouldSequential = false

}



object FindAllMapsLowering{
  def findAllLoweringBySwitch(mapTree: MapTree,totalLevels:Int):Unit = {
    val answerSet = scala.collection.mutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()
    var remainSet = scala.collection.immutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()

    var initSet = scala.collection.immutable.Map[MapTreeNode,Int]()
    //get init input
    mapTree.NodesMap.keys.foreach(
        currNode => {
          if (currNode.Depth <= totalLevels && currNode.Depth != 0) {
            initSet += (currNode -> currNode.Depth)
          }
          if (currNode.Depth > totalLevels) {
            initSet += (currNode -> -1)
          }
        }
    )
    remainSet += initSet

    while(remainSet.nonEmpty){
      var nextToDeal = scala.collection.immutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()
      remainSet.foreach(
        currState => {
          nextToDeal ++= trySwitch(mapTree,currState)
        }
      )
      answerSet ++= remainSet
      remainSet = nextToDeal
    }
  }

  private def trySwitch(mapTree: MapTree,currState:scala.collection.immutable.Map[MapTreeNode,Int]):scala.collection.mutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]  ={
    val nextToDeal = scala.collection.mutable.Set[scala.collection.immutable.Map[MapTreeNode,Int]]()
    currState.foreach(
      currNode =>{
        val currTreeNode:MapTreeNode = currNode._1
        val currMaping:Int = currNode._2
        if(currMaping != -1 && (!currTreeNode.shouldSequential) && currTreeNode.Child.forall(currState(_) == -1)){
          //nextToDeal += (currState - currTreeNode -- currTreeNode.Child + (currTreeNode -> -1) + (currTreeNode.Child.map()))
          nextToDeal += currState.map(x => {
            if (x._1 == currTreeNode) {
              currTreeNode -> -1
            }
            else{
              if(currTreeNode.Child.contains(x._1)){
                x._1 -> currMaping
              }
              else{
                x
              }
            }
          })
        }
      }
    )
    nextToDeal

  }

  def findAllLoweringByEnum(mapTree: MapTree,totalLevel:Int,controlMapNum: ControlMapNum):ArrayBuffer[scala.collection.immutable.Map[Int,Int]] ={

    val beforeFilter = lowerNext(mapTree.RootNode,0,totalLevel)(mapTree.NodesMap)
    val afterFilter = beforeFilter.filter( (oneResult) =>{
      (!controlMapNum.level1 || oneResult.count((p) => {p._2 == 1}) <= 1) &&
        (!controlMapNum.level2 || oneResult.count((p) => {p._2 == 2}) <= 1) &&
        (!controlMapNum.level3 || oneResult.count((p) => {p._2 == 3}) <= 1) &&
        (!controlMapNum.level4 || oneResult.count((p) => {p._2 == 4}) <= 1)
    })
    afterFilter

  }

  private def lowerNext(currNode:MapTreeNode,currLevel:Int,totalLevel:Int)(implicit nodesMap:collection.mutable.Map[MapTreeNode,Int]):ArrayBuffer[scala.collection.immutable.Map[Int,Int]]={
    val res = ArrayBuffer[scala.collection.immutable.Map[Int,Int]]()

    //currNode should be sequential, but we can't reach the lowest level, so back trace
    if(currNode.shouldSequential && currLevel < totalLevel)
      return res

    //currNode doesn't have child, but we can't reach the lowest level, so back trace
    if(currNode.Child.isEmpty && currLevel < totalLevel){
      return res
    }

    //currNode does't have child, so we finally reach an end
    if(currNode.Child.isEmpty){

      //should not lower this level
      if(currLevel == totalLevel + 1){
        return ArrayBuffer[scala.collection.immutable.Map[Int,Int]](scala.collection.immutable.Map[Int,Int](nodesMap(currNode) -> 0))
      }

      //should lower this level
      if(currLevel == totalLevel ){
        return ArrayBuffer[scala.collection.immutable.Map[Int,Int]](scala.collection.immutable.Map[Int,Int](nodesMap(currNode) -> totalLevel))
      }
      assert(false)
    }

    if(currNode.Depth!= 0) {


      //lower this level
      if (currLevel <= totalLevel) {

        var collectedRes = Array[scala.collection.immutable.Map[Int,Int]](scala.collection.immutable.Map[Int,Int](nodesMap(currNode) -> currLevel))
        //actually we are doing cartesian product
        currNode.Child.foreach((childNode) => {
          val resOfOneChild = lowerNext(childNode, currLevel + 1, totalLevel)
          /*
          childResult.foreach((oneResult) => {
            res += (oneResult + (nodesMap(currNode) -> currLevel))
          })*/
          collectedRes = for {x <- collectedRes; y <- resOfOneChild} yield x ++ y
        })

        res ++= collectedRes
      }



      //don't lower this level
      if((!currNode.shouldSequential) || currLevel > totalLevel) {

        var collectedRes = Array[scala.collection.immutable.Map[Int, Int]](scala.collection.immutable.Map[Int, Int](nodesMap(currNode) -> 0))

        currNode.Child.foreach((childNode) => {
          val resOfOneChild = lowerNext(childNode, currLevel, totalLevel)
          /*childResult.foreach((oneResult) => {
          res += (oneResult + (nodesMap(currNode) -> 0))
        })*/
          collectedRes = for {x <- collectedRes; y <- resOfOneChild} yield x ++ y
        })

        res ++= collectedRes
      }

     res
    }
    else{
      //the temprory root node

      //actually we are doing cartesian product
      var collectedRes = Array[scala.collection.immutable.Map[Int,Int]](scala.collection.immutable.Map[Int,Int](-1 -> -1))
      currNode.Child.foreach((childNode) =>{
        val resOfOneChild = lowerNext(childNode,currLevel + 1,totalLevel)
        collectedRes = for {x <- collectedRes; y <- resOfOneChild} yield x ++ y
        })
      res ++= collectedRes
      res
    }





  }
}

