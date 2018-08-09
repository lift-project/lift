package utils.paternoster.rendering

/**
  * All basic shapes of wich the visualisation is made.
  */
object Graphics {
  sealed trait GraphicalPrimitive
  case class Rectangle(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive
  case class Box(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive
  case class Line(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive
  case class Arrow(x1:Double, y1:Double, x2:Double, y2:Double) extends GraphicalPrimitive
  case class BoxWithText(text:String, x:Double, y:Double, width:Double, height:Double)extends GraphicalPrimitive
  case class CorneredClause(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive
  case class Seperator(x:Double,y:Double) extends GraphicalPrimitive
  case class ExpressionSource(text:String, beginHighlight : Int , endHighLight: Int ,x:Double,y:Double) extends GraphicalPrimitive
  case class DashedBox(x:Double, y:Double, width:Double, height:Double) extends GraphicalPrimitive

  /**
    * Moves the give primitive by the given amount.
    * @param primitive The primitive that will be moved.
    * @param dx The amount that the primitive will be moved in x direction.
    * @param dy The amount that the primitive will be moved in y direction.
    * @return The moved primitive.
    */
  def translate(primitive:GraphicalPrimitive, dx:Double, dy:Double):GraphicalPrimitive = {
    primitive match {
      case et: ExpressionSource => et.copy(text = et.text, beginHighlight = et.beginHighlight, endHighLight = et.endHighLight, x = et.x + dx, y = et.y + dy)
      case c: CorneredClause=> c.copy(x = c.x + dx, y = c.y + dy)
      case s:Seperator => s.copy(x= s.x+dx, y= s.y+dy)
      case r:Rectangle => r.copy(x = r.x + dx, y = r.y + dy)
      case b:Box => b.copy(x = b.x + dx, y = b.y + dy)
      case db:DashedBox => db.copy(x = db.x + dx, y = db.y + dy)
      case bwt:BoxWithText => bwt.copy(bwt.text,bwt.x+dx,bwt.y+dy)
      case Line(x1, y1, x2, y2) => Arrow(x1 + dx, y1 + dy, x2 + dx, x2 + dy)
      case Arrow(x1, y1, x2, y2) => Arrow(x1 + dx, y1 + dy, x2 + dx, x2 + dy)
    }
  }

  /**
    * Moves all given primitives by the given amount.
    * @param primitives The primitives that will be moved.
    * @param dx The amount that the primitives will be moved in x direction.
    * @param dy The amount that the primitives will be moved in y direction.
    * @return The moved primitives.
    */
  def translateAll(primitives:Iterable[GraphicalPrimitive], dx:Double, dy:Double):Iterable[GraphicalPrimitive] = {
    primitives.map(translate(_, dx = dx, dy = dy))
  }
}
