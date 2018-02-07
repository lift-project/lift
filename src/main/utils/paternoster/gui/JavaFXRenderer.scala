package utils.paternoster.gui

import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color
import javafx.scene.text.{Font, Text}

import utils.paternoster.logic.Graphics._
import utils.paternoster.logic.Scene.GridArrayNode

/**
  * Created by Federico on 18-Aug-17.
  */
object JavaFXRenderer {
  case class Context(gc:GraphicsContext, unitX:Double, unitY:Double, smallX:Double, smallY:Double, width:Double,height:Double)

  def drawPrimitives(primitives:Iterable[GraphicalPrimitive], ctx:Context) ={
    var newContext = adjustScaling(primitives,ctx)
    primitives.foreach(drawPrimitive(_, newContext))
  }

  def adjustScaling(primitives:Iterable[GraphicalPrimitive], ctx:Context):Context={
    val defaultYScaling = 60;
    var maxScaledWidth=0d
    var maxwidth= 0d
    var minheight= 1000d
    var maxheight = 0d
    var minScaledHeight = 1000d
    var maxScaledHeight = 0d

    primitives.foreach(primitive =>primitive match {
      case BoxWithText(text, tx, ty, bx, by, bwidth, bheight) => {
        val currentScaledWidth = bwidth * ctx.unitX - 2 * ctx.smallX
        if ((currentScaledWidth) > maxScaledWidth) {
          maxScaledWidth = currentScaledWidth
          maxwidth = bwidth
        }
        val currentScaledHeight = bheight * ctx.unitY - 2 * ctx.smallY
        if ((currentScaledHeight) < minScaledHeight) {
          minScaledHeight = currentScaledHeight
          minheight = bheight
        }
        if ((currentScaledHeight) > maxScaledHeight) {
          maxScaledHeight = currentScaledHeight
          maxheight = bheight
        }
      }
      case Box(x, y, w, h) => {
        val currentScaledWidth = w * ctx.unitX - 2 * ctx.smallX
        if ((currentScaledWidth) > maxScaledWidth) {
          maxScaledWidth = currentScaledWidth
          maxwidth = w
        }
        val currentScaledHeight = h * ctx.unitY - 2 * ctx.smallY
        if ((currentScaledHeight) < minScaledHeight) {
          minScaledHeight = currentScaledHeight
          minheight = h
        }
        if ((currentScaledHeight) > maxScaledHeight) {
          maxScaledHeight = currentScaledHeight
          maxheight = h
        }
      }
      case _:Any =>
    })

      val newXScaling = (ctx.width-2*ctx.smallX)/maxwidth
      var newYScaling = newXScaling*0.5
      if(minScaledHeight < (ctx.height*0.5)){
        newYScaling = (ctx.height*0.08- 2*ctx.smallY)/(minheight)
      }
      val yPercentSmaller = newYScaling/defaultYScaling
    val newFontSize = Math.max(ctx.gc.getFont.getSize*yPercentSmaller,10)
      ctx.gc.setFont(new Font(newFontSize))
      Context(ctx.gc, newXScaling, newYScaling, ctx.smallX, ctx.smallY,ctx.width,ctx.height)
  }

  def drawPrimitive(primitive:GraphicalPrimitive, ctx: Context) = {
    primitive match {
      case BoxWithText(text,tx,ty,bx,by,bwidth,bheight)=>
        ctx.gc.setFill(Color.BLACK)
        ctx.gc.strokeRect(
          bx*ctx.unitX + ctx.smallX,
          by*ctx.unitY + ctx.smallY,
          bwidth*ctx.unitX - 2*ctx.smallX,
          bheight*ctx.unitY - 2*ctx.smallY
        )


        val textX = ((bx*ctx.unitX + ctx.smallX)+(bwidth*ctx.unitX - 2*ctx.smallX))-Math.min((ctx.gc.getFont.getSize*text.size),(bwidth*ctx.unitX - 2*ctx.smallX)/2)
        val textY = ((by*ctx.unitY + ctx.smallY)+(bheight*ctx.unitY - 2*ctx.smallY))-((bheight*ctx.unitY - 2*ctx.smallY)*0.025)
        //ctx.gc.setFont(new Font(ctx.gc.getFont.getName,10))
        ctx.gc.strokeText(text,textX,textY)
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
