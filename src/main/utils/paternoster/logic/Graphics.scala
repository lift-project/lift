package utils.paternoster.logic

/**
  * Created by Federico on 18-Aug-17.
  */
object Graphics {
  sealed trait GraphicalPrimitive
  case class Rectangle(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive
  case class Box(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive
  case class Line(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive
  case class Arrow(x1:Double, y1:Double, x2:Double, y2:Double) extends GraphicalPrimitive
  case class BoxWithText(text:String,bx:Double, by:Double, bwidth:Double, bheight:Double)extends GraphicalPrimitive

  def translate(primitive:GraphicalPrimitive, dx:Double, dy:Double):GraphicalPrimitive = {
    primitive match {
      case r:Rectangle => r.copy(x = r.x + dx, y = r.y + dy)
      case b:Box => b.copy(x = b.x + dx, y = b.y + dy)
      case bwt:BoxWithText => bwt.copy(bwt.text,bwt.bx+dx,bwt.by+dy)
      case Line(x1, y1, x2, y2) => Arrow(x1 + dx, y1 + dy, x2 + dx, x2 + dy)
      case Arrow(x1, y1, x2, y2) => Arrow(x1 + dx, y1 + dy, x2 + dx, x2 + dy)
    }
  }

  def translateAll(primitives:Iterable[GraphicalPrimitive], dx:Double, dy:Double):Iterable[GraphicalPrimitive] = {
    primitives.map(translate(_, dx = dx, dy = dy))
  }
}
