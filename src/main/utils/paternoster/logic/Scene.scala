package utils.paternoster.logic

import javafx.scene.text.Font

import ir._
import ir.ast.{Lambda, Tuple}
import lift.arithmetic.ArithExpr
import utils.paternoster.gui.{MainPane, TypeVisualizer}
import utils.paternoster.logic.Graphics.BoxWithText;

/**
  * Created by federico on 16/08/17.
  */

object Scene {


  /*A scene node is any conceptual element that can be transformed into a series of graphical primitives.
   * The idea is that each element in a Lift expression can be represented as one or more of these elements.
   * Ultimately, the entire expression corresponds to a single root-node. The Scene tree is then `drawn` by transforming
   * it into a series of abstract graphics primitives
  */
  sealed trait Node

  sealed trait TypeNode extends Node

  case class FloatNode() extends TypeNode
  case class TupleNode(elements:Seq[TypeNode]) extends TypeNode
  sealed trait ArrayTypeNode extends TypeNode
  case class LinearArrayNode(element:TypeNode, size:Int) extends ArrayTypeNode
  case class GridArrayNode(elementType: TypeNode, width:Int, height:Int) extends ArrayTypeNode
  case class BoxArrayNode(elementType: TypeNode, size:Int) extends ArrayTypeNode

  //sealed trait OperationNode extends Node
  //case class MapNode(inputElement:TypeNode, outputElement:TypeNode, size:ArraySize) extends OperationNode

  val MAP_NODE_CHILDREN_DISTANCE = 5
  val drawingPane :MainPane = TypeVisualizer.getMainPane()
  val ARRAY_NODE_MARGIN_TO_CHILDREN_X= 2
  val ARRAY_NODE_MARGIN_TO_CHILDREN_Y = 2
  val NODE_DISTANCE = 5






  private def nodeWidth(node: Node):Double = node match {
    case FloatNode() => 5
    case TupleNode(elements) => elements.map(nodeWidth).sum
    case LinearArrayNode(elem, size) => nodeWidth(elem) * size
    case GridArrayNode(elem, width, _) => (2*ARRAY_NODE_MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * width
    case BoxArrayNode(elem, size) => (2*ARRAY_NODE_MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * size
    //case MapNode(input, output, size) => Math.max(nodeWidth(input) + size, nodeWidth(output) + size)
  }

  private def nodeHeight(node: Node):Double = node match {
    case FloatNode() => 5
    case TupleNode(elements) => elements.map(nodeHeight).max
    case LinearArrayNode(elem, size) => nodeHeight(elem)
    case GridArrayNode(elem, _, height) => (2*ARRAY_NODE_MARGIN_TO_CHILDREN_Y) + nodeHeight(elem) * height
    case BoxArrayNode(elem, size) => (2*ARRAY_NODE_MARGIN_TO_CHILDREN_Y) +nodeHeight(elem)
    //case MapNode(input, output, size) => nodeHeight(input) +  MAP_NODE_CHILDREN_DISTANCE + nodeHeight(output)
  }

  //Node construction (from lift source items)

  def typeNode(t:Type,dimensions : List[List[Int]]=List()):TypeNode = t match {
      //Only float scalars for now
    case ScalarType("float",_) => FloatNode()
    case tt: TupleType => TupleNode(tt.elemsT.map(typeNode(_)))
    case array:ArrayType with Size =>
      //Get the nested array sizes as an ordered list
      //val sizes = flattenArraySizes(array)
      //Group the ordered list of sizes according to the default dimension rules
     // val groupedSizes = groupSizesByDimensions(defaultDimensionSplits(sizes.length), sizes)
      //The ultimate non-array element contained in the nested array
      val bottomElement = arrayBottomElementType(array)
      arrayTypeNode(bottomElement, dimensions)
    case _: Lambda => throw new NotImplementedError("No support for drawing function types yet")
  }

  private def arrayTypeNode(bottomT:Type, sizes:List[List[Int]]):ArrayTypeNode = {
    if(sizes.isEmpty) {
      throw new Exception("Array type renderer with empty sizes - impossible!!")
    }
    val(currentSizes::nextSizes) = sizes
    //Build the contained element first...
    val inner = nextSizes match {
      //If we are out of sizes, then this is the end of the array. we contain the bottom element
      case Nil => typeNode(bottomT)
      //Otherwise recurse
      case _ => arrayTypeNode(bottomT, nextSizes)
    }
    //Now build the current level of array
    currentSizes.length match {
      //1 dimension - linear array. Dimension is length
      //case 1 => LinearArrayNode(inner, currentSizes.head)

      case 1 => BoxArrayNode(inner, currentSizes.head)

      case 2 => {
        var innerArrayType : Scene.TypeNode = null
        var innerArraySize =0
        inner match {
          /*case b: BoxArrayNode => {
            innerArrayType=b.elementType
            innerArraySize = b.size
            GridArrayNode(innerArrayType,currentSizes.head,innerArraySize)
          }*/
          case other => GridArrayNode(inner,currentSizes.head,currentSizes.tail.head)

        }
      }
      //2 dimensions - grid. Dimensions are width and height
      //case 2 => GridArrayNode(inner, currentSizes.head, currentSizes.tail.head)
      //any other - not supported yet!
      case n => throw new Exception(s"Unsupported rendering of $n-dimensional array level. Try another dimension grouping")
    }
  }

  def arrayBottomElementType(arr:ArrayType):Type = {
    arr.elemT match {
      case nested: ArrayType => arrayBottomElementType(nested)
      case other => other
    }
  }

/**
  def operationNode(operation:Operation):OperationNode = {
    operation match {
      case Map(fType, size) => MapNode(typeNode(fType.inputType), typeNode(fType.outputType), size)
    }
  }
*/

  private def flattenArraySizes(array:ArrayType with Size):List[Int] = {
    var arrayVars = array.size.varList
    arrayVars.map((arrVar)=> System.out.println(arrVar.toString))
    array.size.eval :: (array.elemT match {
      case nested: ArrayType with Size with Capacity => flattenArraySizes(nested)
      case _ => List()
    })
  }
  private def defaultDimensionSplits(n:Int):List[Int] = n match {
    case 0 => Nil
    case 1 => List(1)
    case 2 => List(2)
    case _ => 2::defaultDimensionSplits(n - 2)
  }


  private def groupSizesByDimensions(dimensions:List[Int], sizes:List[Int]):List[List[Int]] = {
    dimensions match {
      case Nil => List()
      case dim::other_dims => sizes.take(dim) ::groupSizesByDimensions(other_dims, sizes.drop(dim))
    }
  }

    import Graphics._

    //The methods here take care of transforming nodes into sets of graphical primitives.
    def drawType(typeNode: TypeNode):Iterable[GraphicalPrimitive] = {
      typeNode match {
        case FloatNode() => Seq(Rectangle(0, 0,5,5))
        case TupleNode(elements) =>
          //Draw elements
          val elementPrimitives = elements.flatMap(drawType)
          //TODO:Replicate and place elements
          //TODO:Alignment
          //TODO:Separator lines
          throw new NotImplementedError("Tuple implementation not finished")
        case LinearArrayNode(element, size) =>
          val elemWidth = nodeWidth(element)
          //compute inner element primitives
          val elementPrims = drawType(element)
          //Repeat each set of primitives up to size times, translating the set by the
          //position
          val sets = (0 until size).map(pos => translateAll(elementPrims, dx = pos*elemWidth, dy = 0))
          //As a final results, flatten the sets and add the container box
          sets.flatten ++ Seq(Box(0, 0, size*elemWidth, 1))
        case GridArrayNode(elementType, width, height) =>
          val elemWidth =nodeWidth(elementType)
          val elemHeight = nodeHeight(elementType)
          //compute inner element primitives
          val elementPrims = drawType(elementType)
          //Compute the positions where the children will go
          val positions = for(x <- 0 until width;
                              y <- 0 until height) yield (x*elemWidth, y*elemHeight)
          //For each position, replicate the elementPrimitives and translate them to that place
          val sets = positions.map{case (x,y) => translateAll(elementPrims, x+ARRAY_NODE_MARGIN_TO_CHILDREN_X-0.5, y+ARRAY_NODE_MARGIN_TO_CHILDREN_Y-0.5)}
          //Flatten the sets and wrap in container box
          sets.flatten ++ Seq(BoxWithText(width.toString+"x"+height.toString ,0, 0, (width*elemWidth)+(ARRAY_NODE_MARGIN_TO_CHILDREN_X*2), (height*elemHeight)+(ARRAY_NODE_MARGIN_TO_CHILDREN_Y*2)))
        case BoxArrayNode(elementType, size) =>
          val elemWidth =nodeWidth(elementType)
          val elemHeight = nodeHeight(elementType)

          var sets : IndexedSeq[Iterable[Graphics.GraphicalPrimitive]] = null
          //compute inner element primitives
          val elementPrims = drawType(elementType)

          elementType match {
            case _:FloatNode =>sets = (0 until size).map(pos => translateAll(elementPrims, dx = ((pos*elemWidth)+ARRAY_NODE_MARGIN_TO_CHILDREN_X)-0.5 , dy = ARRAY_NODE_MARGIN_TO_CHILDREN_Y-0.5))
            case _:Any =>sets = (0 until size).map(pos => translateAll(elementPrims, dx = (pos*elemWidth)+ARRAY_NODE_MARGIN_TO_CHILDREN_X, dy = ARRAY_NODE_MARGIN_TO_CHILDREN_Y))
          }





        //As a final results, flatten the sets and add the container box
          sets.flatten ++ Seq(BoxWithText(size.toString,0, 0, (size*elemWidth)+(ARRAY_NODE_MARGIN_TO_CHILDREN_X*2), elemHeight+(ARRAY_NODE_MARGIN_TO_CHILDREN_Y*2)))
      }
    }

/*
     /***
       * Computes horizontal translations needed for each node to be aligned on a common
       * center axis.
       * @param nodes The nodes to align
       * @return A mapping from each node to the amount of horizontal translation needed to align
       */
     private def horizontalAlignment(nodes:Iterable[Node]):scala.collection.Map[Node,Int] = {
       val nodeWidthMap = nodes.map(node => (node, nodeWidth(node))).toMap
       val maxWidth = nodeWidthMap.values.max
       //For each node, we need to translate by (maxWidth - nodeWidth)/2
       nodeWidthMap.mapValues(x => (maxWidth - x)/2)
     }
*/
}