package ir.ast

import ir.interpreter.Interpreter._
import lift.arithmetic.{ArithExpr, Cst, floor}
import ir._
import opencl.ir.pattern.{MapSeq, MapSeq2D_}
import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * Slide pattern.
 * Create sliding windows of input
 */
case class Slide(size: ArithExpr, step: ArithExpr) extends Pattern(arity = 1) {
  Slide.cnt += 1
  val id = Slide.cnt

  override def toString: String = "Slide(" + size + "," + step + ")"

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    argType match {
      case ArrayTypeWSWC(et,s,c) if s == c =>
        // todo check that the sliding window always ends at the last element of the input
//        if (((s - (size - step)) % step) != Cst(0))
//          throw TypeException(s"Dimensions of slide are not divisible: (($s - ($size - $step)) % $step) = " +
//            s"${(s - (size - step)) % step}")
//        if (((c - (size - step)) % step) != Cst(0))
//          throw TypeException(s"Dimensions of slide are not divisible: (($c - ($size - $step)) % $step) = " +
//            s"${(c - (size - step)) % step}")
        val innerSize = size
        val innerCapacity = size
        val outerSize = (s - (size - step)) / step // TODO: make sure that these are divisible
        val outerCapacity = (c - (size - step)) / step  // TODO: make sure that these are divisible
        ArrayTypeWSWC(ArrayTypeWSWC(et, innerSize, innerCapacity), outerSize, outerCapacity)
      case _ => throw new TypeException(argType, "ArrayType", this)
    }
  }

  /**
   * Define equality operator based on ID to be able to insert [[Slide]] instances
   * into a set properly.
   *
   * @param other Another object.
   * @return True if the other object is a [[Slide]] instance with the same ID, false otherwise.
   */
  override def equals(other: Any): Boolean = other match {
    case s: Slide => s.id == id
    case _ => false
  }

  override def eval(valueMap: ValueMap, args: Any*): Array[Any] = {
    throw new NotImplementedException()
  }

  def eval(arg: Array[Array[Float]]): Array[Array[Array[Float]]] =
    arg.iterator.sliding(size.evalInt, step.evalInt).withPartial(false).map(_.toArray).toArray

  def eval(arg: Array[Array[Array[Float]]]): Array[Array[Array[Array[Float]]]] =
    arg.iterator.sliding(size.evalInt, step.evalInt).withPartial(false).map(_.toArray).toArray




  /**
   * Define hash based on the ID to identify unique instances in associative containers.
   * @return The hashCode of the id.
   */
  override def hashCode = id.hashCode()
}

case class Slide2D_(sizeRow: ArithExpr, stepRow: ArithExpr,
            sizeCol: ArithExpr, stepCol: ArithExpr, noExpand: Boolean = true) extends Pattern(arity = 1) {

  var expanded_lambda : Lambda = null

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    expanded_lambda = Slide2D(sizeRow, stepRow, sizeCol, stepCol)
    expanded_lambda.checkType(argType, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

  override def toString: String = "Slide2D()"

}

case class Tile2D(override val f: Lambda1, sizeRow: ArithExpr, stepRow: ArithExpr,
            sizeCol: ArithExpr, stepCol: ArithExpr) extends Pattern(arity = 1) with FPattern {

  override def copy(f: Lambda): Pattern = Tile2D(f, sizeRow, stepRow, sizeCol, stepCol)

  var expanded_lambda : Lambda = null

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
      // Check f. This has to be done every time in case we call checkType on this Tile2D more than once;
      // otherwise, the types of f.params.head and f will reduce every time
      f.params.head.t = argType
      TypeChecker(f)
      val golden_type = f.body.t
      //here we know the output type must be at 2D, as we apply slide2d, so the original shape is at least 2d, which means the result shape is at least 2d as well,
      //so this pattern matching always work
      val ArrayTypeWS(ArrayTypeWS(_, golden_row_size), num_rows) = golden_type

      val ArrayTypeWS(_, image_row_size) = argType
      val output_row_size = (image_row_size - sizeRow + stepRow) / stepRow
      expanded_lambda = Split(golden_row_size) o Join() o Join() o Join() o Map(TransposeW()) o Split(output_row_size) o MapSeq(f) o Join() o Slide2D_(sizeRow, stepRow, sizeCol, stepCol)
      val outT = expanded_lambda.checkType(argType, setType)
      // The check below might not be evaluable statically. If sizeRow or stepRow or sizeCol or stepCol are variables,
      // then outT has them inside array sizes, while golden_type doesn't
      //assert(outT == golden_type)
      outT
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

  override def toString: String = "Tile2D("+ f +")"

}

/**
 * This is not Data reusage -- it is tiling. To be removed
 */
case class NotDataReusageButTiling(override val f: Lambda1, reusage_size: ArithExpr) extends Pattern(arity =1 ) with FPattern {

  override def copy(f: Lambda): Pattern = NotDataReusageButTiling(f, reusage_size)

  var expanded_lambda : Lambda = null

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    //if (expanded_lambda == null) {
      val ArrayTypeWS(ArrayTypeWS(_, row_size), num_rows) = argType
      expanded_lambda = Split(row_size) o Join() o MapSeq2D_(f) o Split(reusage_size) o Join()
      val tmp = expanded_lambda.checkType(argType, setType)
      tmp
    //} else expanded_lambda.checkType(argType, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }


}


case class ShapeRestoreForReserseSplitJoin(override val f: Lambda1)  extends Pattern(arity =1 ) with FPattern  {

  override def copy(f: Lambda): Pattern = ShapeRestoreForReserseSplitJoin(f)

  var expanded_lambda : Lambda = null

  override def checkType(argType: Type,
                         setType: Boolean): Type = {

    val ArrayTypeWS(ArrayTypeWS(_, row_size), num_of_rows) = argType

    expanded_lambda = Split(row_size) o f o Join()
    expanded_lambda.checkType(argType, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

}

case class ShapeRestoreFor2DReusage()  extends Pattern(arity =1 )  {

  var expanded_lambda : Lambda = null

  override def checkType(argType: Type,
                         setType: Boolean): Type = {
    expanded_lambda = Map(Join()) o Join() o Map(TransposeW())
    expanded_lambda.checkType(argType, setType)
  }

  override def eval(valueMap: ValueMap, args: Any*): Any = {
    assert(args.length == arity)
    throw new NotImplementedException()
  }

}

object Slide {
  var cnt: Int = -1
}

object Slide2D {
  /** Symmetrical sliding */
  def apply(size: ArithExpr, step: ArithExpr): Lambda = {
    SlideND(2)(size,step)
  }

  /** Asymmetrical sliding */
  def apply(sizeRow: ArithExpr, stepRow: ArithExpr,
            sizeCol: ArithExpr, stepCol: ArithExpr): Lambda = {
    Map(Transpose()) o Slide(sizeRow, stepRow) o Map(Slide(sizeCol, stepCol))
  }

  def apply(sizeRow: ArithExpr, stepRow: ArithExpr,
            sizeCol: ArithExpr, stepCol: ArithExpr, noExpand: Boolean): Lambda = {
    Slide2D_(sizeRow, stepRow, sizeCol, stepCol, noExpand)
    //Map(Transpose()) o Slide(sizeRow, stepRow) o Map(Slide(sizeCol, stepCol))
  }
}

object Slide3D {
  /** Symmetrical sliding */
  def apply(size: ArithExpr, step: ArithExpr): Lambda = {
    SlideND(3)(size,step)
  }

  def apply(sizeX: ArithExpr, stepX: ArithExpr,
            sizeY: ArithExpr, stepY: ArithExpr,
            sizeZ: ArithExpr, stepZ: ArithExpr): Lambda = {
    Map(Map(Transpose()) o Transpose()) o
    Slide(sizeZ, stepZ) o
    Map(Map(Transpose()) o Slide(sizeY, stepY) o Map(Slide(sizeX, stepX)))
  }
}

/* Pass the dimension from high to low */
object Slide3D_R{
  def apply(sizeZ: ArithExpr, stepZ: ArithExpr,
            sizeY: ArithExpr, stepY: ArithExpr,
            sizeX: ArithExpr, stepX: ArithExpr): Lambda = {
    Slide3D(sizeX, stepX, sizeY, stepY, sizeZ, stepZ)
  }

}

object SlideND {

  def apply(dim: Int)(size: ArithExpr, step: ArithExpr): Lambda = {
    if(dim==1) Slide(size,step)
    else {
      GenerateIR.interleaveDimensions(dim, dim) o
        GenerateIR.applyInEveryDimUntilDimReverse(Slide(size, step), dim)
    }
  }
}

object TiledSlidedND {
  def undoTiling(dim: Int): Lambda = {
    if(dim == 1) Join()
    else GenerateIR.applyInEveryDimUntilDim(Join(), dim) o GenerateIR.interleaveDimensionsReverse(dim)
  }

  def apply(dim: Int)(size: ArithExpr, step: ArithExpr, tileStep: ArithExpr, enableUndoTiling: Boolean = true): Lambda = {
    val tileSize = (size - step) + tileStep
    val tiledLambda =
      GenerateIR.wrapInMaps(SlideND(dim)(size,step), dim) o
        SlideND(dim)(tileSize, tileStep)
    if (enableUndoTiling) undoTiling(dim) o tiledLambda
    else tiledLambda
  }
}

object TiledSlided2D {
  def undoTiling(dim: Int): Lambda = {
    if(dim == 1) Join()
    else GenerateIR.applyInEveryDimUntilDim(Join(), dim) o GenerateIR.interleaveDimensionsReverse(dim)
  }

  def apply(size: ArithExpr, step: ArithExpr, tileStepX: ArithExpr, tileStepY: ArithExpr,
            enableUndoTiling: Boolean = true): Lambda = {
    val tileSizeX = (size - step) + tileStepX
    val tileSizeY = (size - step) + tileStepY
    val tiledLambda =
      GenerateIR.wrapInMaps(SlideND(2)(size, step), 2) o
        Slide2D(
          sizeRow = tileSizeY, stepRow = tileStepY,
          sizeCol = tileSizeX, stepCol = tileStepX)

    if (enableUndoTiling) undoTiling(2) o tiledLambda
    else tiledLambda
  }

}

