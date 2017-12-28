package utils.paternoster.gui

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color

import utils.paternoster.logic.Graphics
import utils.paternoster.logic.Graphics._

/**
  * Created by Federico on 18-Aug-17.
  */
object JavaFXRenderer {
  case class Context(gc:GraphicsContext, unitX:Int, unitY:Int, smallX:Int, smallY:Int)

  def drawPrimitives(primitives:Iterable[GraphicalPrimitive], ctx:Context) =
    primitives.foreach(drawPrimitive(_, ctx))


  def drawPrimitive(primitive:GraphicalPrimitive, ctx: Context) = {
    primitive match {
      case Rectangle(x, y, w, h) =>
        ctx.gc.setFill(Color.GREEN)
        ctx.gc.fillRect(
          x*ctx.unitX + ctx.smallX*2,
          y*ctx.unitY + ctx.smallY*2,
          w*ctx.unitX - 4*ctx.smallX,
          h*ctx.unitY - 4*ctx.smallY)
      case Box(x, y, w, h) =>
        ctx.gc.setStroke(Color.RED)
        ctx.gc.strokeRect(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          w*ctx.unitX - 2*ctx.smallX,
          h*ctx.unitY - 2*ctx.smallY)
      case Line(startX, startY, endX, endY) =>
        ctx.gc.setStroke(Color.BLACK)
        ctx.gc.strokeLine(
          startX*ctx.unitX,
          startY*ctx.unitY,
          endX*ctx.unitX,
          endY*ctx.unitY
        )
      case Arrow(startX, startY, endX, endY) =>
        ctx.gc.setStroke(Color.BLACK)
        drawArrow(
          ctx.gc,
          startX*ctx.unitX,
          startY*ctx.unitY,
          endX*ctx.unitX,
          endY*ctx.unitY
        )
    }
  }

  private def drawArrow(gc:GraphicsContext, node1X:Double, node1Y:Double, node2X:Double, node2Y:Double) {
    val arrowAngle = Math.toRadians(45.0)
    val arrowLength = 10.0
    val dx = node1X - node2X
    val dy = node1Y - node2Y
    val angle = Math.atan2(dy, dx)
    val x1 = Math.cos(angle + arrowAngle) * arrowLength + node2X
    val y1 = Math.sin(angle + arrowAngle) * arrowLength + node2Y

    val x2 = Math.cos(angle - arrowAngle) * arrowLength + node2X
    val y2 = Math.sin(angle - arrowAngle) * arrowLength + node2Y
    gc.strokeLine(node1X, node1Y, node2X, node2Y)
    gc.strokeLine(node2X, node2Y, x1, y1)
    gc.strokeLine(node2X, node2Y, x2, y2)
  }
}
