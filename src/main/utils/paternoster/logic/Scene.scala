package utils.paternoster.logic

import javafx.scene.text.Font

import ir._
import ir.ast.{Lambda, Tuple}
import lift.arithmetic.ArithExpr
import utils.paternoster.gui.{MainPane, TypeVisualizer}
import utils.paternoster.logic.Graphics.{BoxWithText, Seperator}

import scala.collection.mutable.ListBuffer;

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
  case class VectorNode(element:TypeNode, size: Int) extends TypeNode
  sealed trait ArrayTypeNode extends TypeNode
  case class LinearArrayNode(element:TypeNode, size:Int) extends ArrayTypeNode
  case class GridArrayNode(elementType: TypeNode, width:Int, height:Int) extends ArrayTypeNode
  case class BoxArrayNode(elementType: TypeNode, size:Int) extends ArrayTypeNode

  //sealed trait OperationNode extends Node
  //case class MapNode(inputElement:TypeNode, outputElement:TypeNode, size:ArraySize) extends OperationNode

  val MAP_NODE_CHILDREN_DISTANCE = 5
  val ARRAY_NODE_MARGIN_TO_CHILDREN_X= 2
  val ARRAY_NODE_MARGIN_TO_CHILDREN_Y = 2
  val VECTOR_NODE_MARGIN_TO_CHILDREN_X= 2
  val VECTOR_NODE_MARGIN_TO_CHILDREN_Y = 2
  val CONTAINER_NODE_SPACING = 1
  val NODE_DISTANCE = 5
  val SEPERATOR_WIDTH=1






  private def nodeWidth(node: Node):Double = node match {
    case FloatNode() => 5
    case TupleNode(elements) =>  2*CONTAINER_NODE_SPACING + elements.map(nodeWidth).sum + (2*ARRAY_NODE_MARGIN_TO_CHILDREN_X)+(elements.size-1 )*(SEPERATOR_WIDTH*4)//Add seperator width
    case LinearArrayNode(elem, size) => 2*CONTAINER_NODE_SPACING +nodeWidth(elem) * size
    case GridArrayNode(elem, width, _) => 2*CONTAINER_NODE_SPACING +(2*ARRAY_NODE_MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * width
    case BoxArrayNode(elem, size) => 2*CONTAINER_NODE_SPACING +(2*ARRAY_NODE_MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * size
    case VectorNode(elem, size) => 2*CONTAINER_NODE_SPACING +(2*ARRAY_NODE_MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * size
    //case MapNode(input, output, size) => Math.max(nodeWidth(input) + size, nodeWidth(output) + size)
  }

  private def nodeHeight(node: Node):Double = node match {
    case FloatNode() => 5
    case TupleNode(elements) => elements.map(nodeHeight).max + (2*ARRAY_NODE_MARGIN_TO_CHILDREN_Y)
    case LinearArrayNode(elem, size) => nodeHeight(elem)
    case GridArrayNode(elem, _, height) => (2*ARRAY_NODE_MARGIN_TO_CHILDREN_Y) + nodeHeight(elem) * height
    case BoxArrayNode(elem, size) => (2*ARRAY_NODE_MARGIN_TO_CHILDREN_Y) +nodeHeight(elem)
    case VectorNode(elem, size) => (2*ARRAY_NODE_MARGIN_TO_CHILDREN_Y) +nodeHeight(elem)
    //case MapNode(input, output, size) => nodeHeight(input) +  MAP_NODE_CHILDREN_DISTANCE + nodeHeight(output)
  }

  //Node construction (from lift source items)

  def typeNode(t:Type,dimensions : List[List[Int]]=List()):TypeNode = t match {
      //Only float scalars for now
    case ScalarType("float",_) => FloatNode()
      //ToDo check if this is left side first
    case tt: TupleType => {
      var elementBuffer = new ListBuffer[TypeNode]()

      var modifiedDimensions = dimensions
      for(node <- tt.elemsT){
        elementBuffer+= typeNode(node,dimensions)
        var nestedArrayCount = getDimensionCount(node)
        modifiedDimensions = dimensions.drop(nestedArrayCount)
      }
      TupleNode(elementBuffer.toSeq)
    }
    case vt: VectorType => {
      val bottomElement = bottomElementType(vt)
      vectorTypeNode(bottomElement,vt.len.evalInt)
    }
    case array:ArrayType with Size =>
      //Get the nested array sizes as an ordered list
      //val sizes = flattenArraySizes(array)
      //Group the ordered list of sizes according to the default dimension rules
     // val groupedSizes = groupSizesByDimensions(defaultDimensionSplits(sizes.length), sizes)
      //The ultimate non-array element contained in the nested array
      val bottomElement = bottomElementType(array)
      arrayTypeNode(array,bottomElement, dimensions)
    case _: Lambda => throw new NotImplementedError("No support for drawing function types yet")
  }

  private def vectorTypeNode(scalarT: Type, size: Int):VectorNode = {
    //Build the contained element first...
    val inner = typeNode(scalarT);
    VectorNode(inner,size)
  }

  private def arrayTypeNode(currentType:Type,bottomType:Type, sizes:List[List[Int]]):ArrayTypeNode = {
    if(sizes.isEmpty) {
      throw new Exception("Array type renderer with empty sizes - impossible!!")
    }
    val(currentSizes::nextSizes) = sizes
    //Build the contained element first...
    val inner = nextSizes match {
      //If we are out of sizes, then this is the end of the array. we contain the bottom element
      case Nil => typeNode(bottomType)
      //Otherwise recurse
      case _ => currentType match {
        case array:ArrayType=> array.elemT match {
          //case vector: VectorType => if(currentSizes.length == 2) typeNode(bottomType) else typeNode(array.elemT,nextSizes)
          case default => typeNode(array.elemT,nextSizes)
        }
        //case vector:VectorType => if(currentSizes.length == 1) vectorTypeNode(vector.scalarT,nextSizes) else typeNode(vector.scalarT)
        case default =>throw new Exception("Should not be possible. Cases are exaustive.")
      }
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
          case other => GridArrayNode(inner,currentSizes.head,currentSizes.tail.head)
        }
      }
      //2 dimensions - grid. Dimensions are width and height
      //case 2 => GridArrayNode(inner, currentSizes.head, currentSizes.tail.head)
      //any other - not supported yet!
      case n => throw new Exception(s"Unsupported rendering of $n-dimensional array level. Try another dimension grouping")
    }
  }

  def bottomElementType(arr:ArrayType):Type = {
    arr.elemT match {
      case array: ArrayType => bottomElementType(array)
      case other => other
    }
  }
  def bottomElementType(vt:VectorType):Type = {
    vt.scalarT
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
  def getDimensionCount(argType:Type): Int= {
    argType match {
      case ar: ArrayType with Size => 1 + getDimensionCount(ar.elemT)
      case other => 0
    }
  }
    import Graphics._

    //The methods here take care of transforming nodes into sets of graphical primitives.
    def drawType(typeNode: TypeNode):Iterable[GraphicalPrimitive] = {
      typeNode match {
        case FloatNode() => Seq(Rectangle(0, 0,5,5))
        case TupleNode(elements) =>{
          //Draw elements
          var elementIterator = elements.iterator
          var accumulatedWidth = Double.box(0)
          var maxNodeHeight    = Double.box(0)
          var primitiveBuffer = new ListBuffer[GraphicalPrimitive]
          var sets = new ListBuffer[Iterable[GraphicalPrimitive]]
          var seperators = new ListBuffer[Iterable[GraphicalPrimitive]]
          while(elementIterator.hasNext){
            val current = elementIterator.next()
            maxNodeHeight = Double.box(nodeHeight(current)).doubleValue().max(maxNodeHeight)

            if(elementIterator.hasNext){
              //Print seperator
              sets += translateAll(drawType(current), dx =accumulatedWidth+ARRAY_NODE_MARGIN_TO_CHILDREN_X-0.5, dy =ARRAY_NODE_MARGIN_TO_CHILDREN_Y-0.5)
              accumulatedWidth+= nodeWidth(current)
              seperators += translateAll(Seq(Seperator(0,0)), dx =accumulatedWidth+ARRAY_NODE_MARGIN_TO_CHILDREN_X+SEPERATOR_WIDTH*2, dy =ARRAY_NODE_MARGIN_TO_CHILDREN_Y-0.5)
              accumulatedWidth+= SEPERATOR_WIDTH*4//seperator width
            }else{
              //No seperator
              sets += translateAll(drawType(current), dx =accumulatedWidth+ARRAY_NODE_MARGIN_TO_CHILDREN_X-0.5, dy =ARRAY_NODE_MARGIN_TO_CHILDREN_Y-0.5)
              accumulatedWidth+= nodeWidth(current)
            }
          }

          sets.flatten.toSeq ++ translateAll(seperators.flatten.toSeq, dx = 0, dy = maxNodeHeight) ++ Seq(CorneredClause(0,0,accumulatedWidth+(VECTOR_NODE_MARGIN_TO_CHILDREN_X*2), maxNodeHeight+(VECTOR_NODE_MARGIN_TO_CHILDREN_Y*2)))
      }
          //TODO:Replicate and place elements
          //TODO:Alignment
          //TODO:Separator lines
          //throw new NotImplementedError("Tuple implementation not finished")
        case VectorNode(elementType,size)=>{
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
            sets.flatten ++ Seq(DashedBox(0, 0, (size*elemWidth)+(ARRAY_NODE_MARGIN_TO_CHILDREN_X*2), elemHeight+(ARRAY_NODE_MARGIN_TO_CHILDREN_Y*2)))
        }
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