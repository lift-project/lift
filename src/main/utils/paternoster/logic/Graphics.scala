package utils.paternoster.logic

/**
  * Created by Federico on 18-Aug-17.
  */
object Graphics {
  sealed trait GraphicalPrimitive
  case class Rectangle(x:Int, y:Int, width:Int, height:Int) extends GraphicalPrimitive
  case class Box(x:Int, y:Int, width:Int, height:Int) extends GraphicalPrimitive
  case class Line(x:Int, y:Int, width:Int, height:Int) extends GraphicalPrimitive
  case class Arrow(x1:Int, y1:Int, x2:Int, y2:Int) extends GraphicalPrimitive

  def translate(primitive:GraphicalPrimitive, dx:Int, dy:Int):GraphicalPrimitive = {
    primitive match {
      case r:Rectangle => r.copy(x = r.x + dx, y = r.y + dy)
      case b:Box => b.copy(x = b.x + dx, y = b.y + dy)
      case Line(x1, y1, x2, y2) => Arrow(x1 + dx, y1 + dy, x2 + dx, x2 + dy)
      case Arrow(x1, y1, x2, y2) => Arrow(x1 + dx, y1 + dy, x2 + dx, x2 + dy)
    }
  }

  def translateAll(primitives:Iterable[GraphicalPrimitive], dx:Int, dy:Int):Iterable[GraphicalPrimitive] = {
    primitives.map(translate(_, dx = dx, dy = dy))
  }
}
