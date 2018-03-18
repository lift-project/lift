package utils.paternoster.visualisation

import ir._
import ir.ast.Lambda
import utils.paternoster.rendering.Graphics

import scala.collection.mutable.ListBuffer;

/**
  * Capsules all methods and classes to convert a Lift-Type to graphical representations.
  */
object Scene {


  //Definition of the node classes
  sealed trait Node
  case class ScalarNode() extends Node
  case class TupleNode(elements:Seq[Node]) extends Node
  case class VectorNode(element:Node, size: Int) extends Node
  sealed trait ArrayNode extends Node
  case class MatrixNode(element: Node, rows:Int, columns:Int) extends ArrayNode
  case class LinearArrayNode(element: Node, size:Int) extends ArrayNode


  //Positioning Constants

  //Margin that a container element has to the child elements.
  val MARGIN_TO_CHILDREN_X= 2
  val MARGIN_TO_CHILDREN_Y = 2

  //Margin between container elements
  val CONTAINER_NODE_SPACING = 1

  //Width of a Tuple seperator
  val SEPERATOR_WIDTH=1


  /**
    * Returns the width of the given node.
    * @param node The node of wich the width will be returned.
    * @return The width of the given node.
    */
  private def nodeWidth(node: Node):Double = node match {
    case ScalarNode() => 5
    case TupleNode(elements) =>  2*CONTAINER_NODE_SPACING + elements.map(nodeWidth).sum + (2*MARGIN_TO_CHILDREN_X)+(elements.size-1 )*(SEPERATOR_WIDTH*4)//Add seperator width
    case MatrixNode(elem, _, columns) => 2*CONTAINER_NODE_SPACING +(2*MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * columns
    case LinearArrayNode(elem, size) => 2*CONTAINER_NODE_SPACING +(2*MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * size
    case VectorNode(elem, size) => 2*CONTAINER_NODE_SPACING +(2*MARGIN_TO_CHILDREN_X) + nodeWidth(elem) * size
  }

  /**
    * Returns the height of the given node.
    * @param node The node of wich the height will be returned.
    * @return The height of the given node.
    */
  private def nodeHeight(node: Node):Double = node match {
    case ScalarNode() => 5
    case TupleNode(elements) => 2*CONTAINER_NODE_SPACING + elements.map(nodeHeight).max + (2*MARGIN_TO_CHILDREN_Y)
    case MatrixNode(elem, rows, _ ) => 2*CONTAINER_NODE_SPACING + (2*MARGIN_TO_CHILDREN_Y) + nodeHeight(elem) * rows
    case LinearArrayNode(elem, size) => 2*CONTAINER_NODE_SPACING + (2*MARGIN_TO_CHILDREN_Y) +nodeHeight(elem)
    case VectorNode(elem, size) => 2*CONTAINER_NODE_SPACING + (2*MARGIN_TO_CHILDREN_Y) +nodeHeight(elem)
  }

  //Node construction (from lift source items)
  /**
    * Constructs the node structure from the given type and Dimension-Grouping.
    * @param t The Lift-Type wich will be processed.
    * @param dimensions The Dimension-Grouping wich will be processed.
    * @return The node structure that represents the visualisation.
    */
  def typeNode(t:Type,dimensions : List[List[Int]]=List()):Node = t match {
      //Only float scalars for now
    case ScalarType(_,_) => ScalarNode()
    case tt: TupleType => {
      var elementBuffer = new ListBuffer[Node]()

      var modifiedDimensions = dimensions
      //render each tupel element after another
      for(node <- tt.elemsT){
        elementBuffer+= typeNode(node,modifiedDimensions)
        var nestedArrayCount = getDimensionCount(node)
        while(nestedArrayCount != 0){
          //if arrays are grouped they share a sizelist. if the current level is not grouped
          //this is 0
          nestedArrayCount -= modifiedDimensions.head.size-1
          //remove the next size
          modifiedDimensions = modifiedDimensions.drop(1)
          //count down
          nestedArrayCount-=1
        }
      }
      TupleNode(elementBuffer.toSeq)
    }
    case vt: VectorType => {
      val bottomElement = bottomElementType(vt)
      vectorTypeNode(bottomElement,vt.len.evalInt)
    }
    case array:ArrayType with Size =>
      //The ultimate non-array element contained in the nested array
      val bottomElement = bottomElementType(array)
      arrayTypeNode(array,bottomElement, dimensions)
    case _: Lambda => throw new NotImplementedError("No support for drawing function types yet")
  }

  /**
    * Generates a vector node.
    * @param scalarT The scalarType of the vector.
    * @param size The size of the vector.
    * @return The generated vector node.
    */
  private def vectorTypeNode(scalarT: Type, size: Int):VectorNode = {
    //Build the contained element first...
    val inner = typeNode(scalarT);
    VectorNode(inner,size)
  }

  /**
    * Generates a LinearArrayNode or MatrixArrayNoded depending on the grouping.
    * @param currentType The current type.
    * @param bottomType The bottom type of the nesting of arrays.
    * @param dimensionGrouping The grouping containing the sizes of arrays.
    * @return The generated ArrayNode.
    */
  private def arrayTypeNode(currentType:Type,bottomType:Type, dimensionGrouping:List[List[Int]]):ArrayNode = {
    if(dimensionGrouping.isEmpty) {
      throw new Exception("Array type renderer with empty sizes - impossible!!")
    }
    val currentSizes = dimensionGrouping.head
    val nextSizes = dimensionGrouping.tail

    //Build inner element first. If this is a matrix (currentsize = 2) take the inner element of the inner array as inner
    val inner  = if (currentSizes.size == 2) typeNode(currentType.asInstanceOf[ArrayType].elemT.asInstanceOf[ArrayType].elemT, nextSizes) else typeNode(currentType.asInstanceOf[ArrayType].elemT,nextSizes)


    //Now build the current level of array
    currentSizes.length match {
      //1 dimension
      case 1 => LinearArrayNode(inner, currentSizes.head)

      case 2 => {
        //2 dimensions
        MatrixNode(inner, currentSizes.head, currentSizes.tail.head)
      }
      //any other - not supported yet!
      case n => throw new Exception(s"Unsupported rendering of $n-dimensional array level. Try another dimension grouping")
    }
  }

  /**
    * Helper method that returns the bottom-type of an array.
    * @param arr The array that will be searched.
    * @return The bottom type.
    */
  def bottomElementType(arr:ArrayType):Type = {
    arr.elemT match {
      case array: ArrayType => bottomElementType(array)
      case other => other
    }
  }

  /**
    * Helper method that returns the scalartype of a vector.
    * @param vt The vector of wich the scalartype will be returned.
    * @return The scalartype of the vector.
    */
  def bottomElementType(vt:VectorType):Type = {
    vt.scalarT
  }


  /**
    * Helper method that counts the number of nested arrays.
    * @param argType The type in wich the arrays are searched.
    * @return The number of nested arrays.
    */
  def getDimensionCount(argType:Type): Int= {
    argType match {
      case ar: ArrayType with Size => 1 + getDimensionCount(ar.elemT)
      case other => 0
    }
  }
    import Graphics._

  /**
    * Recursively converts the node into a series of GraphicalPrimitives.
    * @param typeNode The node that will be converted.
    * @return The corresponding GraphicalPrimitives to the node.
    */
    def drawType(typeNode: Node):Iterable[GraphicalPrimitive] = {
      typeNode match {
        case ScalarNode() => Seq(Rectangle(0, 0,5,5))
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

            sets += translateAll(drawType(current), dx =accumulatedWidth+MARGIN_TO_CHILDREN_X-0.5, dy =MARGIN_TO_CHILDREN_Y-0.5)
            accumulatedWidth+= nodeWidth(current)
            if(elementIterator.hasNext){
              //Print seperator
              seperators += translateAll(Seq(Seperator(0,0)), dx =accumulatedWidth+MARGIN_TO_CHILDREN_X+SEPERATOR_WIDTH*2, dy =MARGIN_TO_CHILDREN_Y-0.5)
              accumulatedWidth+= SEPERATOR_WIDTH*4//seperator width
            }
          }

          sets.flatten.toSeq ++ translateAll(seperators.flatten.toSeq, dx = 0, dy = maxNodeHeight) ++ Seq(CorneredClause(0,0,accumulatedWidth+(MARGIN_TO_CHILDREN_X*2), maxNodeHeight+(MARGIN_TO_CHILDREN_Y*2)))
      }

        case VectorNode(elementType,size)=>{
            val elemWidth =nodeWidth(elementType)
            val elemHeight = nodeHeight(elementType)


            //compute inner element primitives
            val elementPrims = drawType(elementType)
            var sets = (0 until size).map(pos => translateAll(elementPrims, dx = ((pos*elemWidth)+MARGIN_TO_CHILDREN_X)-0.5 , dy = MARGIN_TO_CHILDREN_Y-0.5))
            //As a final results, flatten the sets and add the container box
            sets.flatten ++ Seq(DashedBox(0, 0, (size*elemWidth)+(MARGIN_TO_CHILDREN_X*2), elemHeight+(MARGIN_TO_CHILDREN_Y*2)))
        }
        case MatrixNode(elementType, rows, columns) =>
          val elemWidth =nodeWidth(elementType)
          val elemHeight = nodeHeight(elementType)
          //compute inner element primitives
          val elementPrims = drawType(elementType)
          //Compute the positions where the children will go
          val positions = for(x <- 0 until columns;
                              y <- 0 until rows) yield (x*elemWidth, y*elemHeight)
          //For each position, replicate the elementPrimitives and translate them to that place
          val sets = positions.map{case (x,y) => translateAll(elementPrims, x+MARGIN_TO_CHILDREN_X-0.5, y+MARGIN_TO_CHILDREN_Y-0.5)}
          //Flatten the sets and wrap in container box
          sets.flatten ++ Seq(BoxWithText(rows.toString+"x"+ columns.toString,0, 0, (columns*elemWidth)+(MARGIN_TO_CHILDREN_X*2), (rows*elemHeight)+(MARGIN_TO_CHILDREN_Y*2)))
        case LinearArrayNode(elementType, size) =>
          val elemWidth =nodeWidth(elementType)
          val elemHeight = nodeHeight(elementType)

          var sets : IndexedSeq[Iterable[Graphics.GraphicalPrimitive]] = null
          //compute inner element primitives
          val elementPrims = drawType(elementType)

          elementType match {
            case _:ScalarNode =>sets = (0 until size).map(pos => translateAll(elementPrims, dx = ((pos*elemWidth)+MARGIN_TO_CHILDREN_X)-0.5 , dy = MARGIN_TO_CHILDREN_Y-0.5))
            case _:Any =>sets = (0 until size).map(pos => translateAll(elementPrims, dx = (pos*elemWidth)+MARGIN_TO_CHILDREN_X, dy = MARGIN_TO_CHILDREN_Y))
          }

        //As a final results, flatten the sets and add the container box
          sets.flatten ++ Seq(BoxWithText(size.toString,0, 0, (size*elemWidth)+(MARGIN_TO_CHILDREN_X*2), elemHeight+(MARGIN_TO_CHILDREN_Y*2)))
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