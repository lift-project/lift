package utils.paternoster.gui

import java.awt.{BasicStroke, RenderingHints}
import javafx.scene.canvas.GraphicsContext
import java.awt.Color


import utils.paternoster.gui.JavaFXRenderer.{Context, adjustCanvas, adjustScaling, drawPrimitive}
import utils.paternoster.logic.Graphics._
import org.jfree
import org.jfree.graphics2d.svg.SVGGraphics2D


object SVGRenderer {
  case class Context(gc:SVGGraphics2D, unitX:Double, unitY:Double, smallX:Double, smallY:Double, numberFont :java.awt.Font , expressionFont: java.awt.Font , width:Double,height:Double)

  def drawPrimitives(primitives:Iterable[GraphicalPrimitive], ctx:Context):Unit ={
    primitives.foreach(drawPrimitive(_, ctx))
  }


  def drawPrimitive(primitive:GraphicalPrimitive, ctx: Context) = {

    primitive match {
      case DashedBox(x,y,w,h)=>

        var dash1= Array( 10.0f )
        var dashed = new BasicStroke(1.0f,
          BasicStroke.CAP_BUTT, BasicStroke.JOIN_MITER, 10.0f, dash1, 0.0f)
        ctx.gc.setRenderingHint(RenderingHints.KEY_ANTIALIASING,
          RenderingHints.VALUE_ANTIALIAS_ON);
        ctx.gc.setPaint(Color.GRAY)

        var old = ctx.gc.getStroke
        ctx.gc.setStroke(dashed)
        ctx.gc.drawRect(
          (x*ctx.unitX + ctx.smallX).toInt,
          (y*ctx.unitY + ctx.smallY).toInt,
          (w*ctx.unitX - 2*ctx.smallX).toInt,
          (h*ctx.unitY - 2*ctx.smallY).toInt
        )
        ctx.gc.setStroke(old)

      case ExpressionSource(text,begin,end, x,y)=>

        ctx.gc.setFont(ctx.expressionFont)
        var beforeHightLightText = text.substring(0,begin)
        var highLightText = text.substring(begin, end+1)
        var afterHighLightText = text.substring(end+1,text.length)

        var beforeLength = ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(beforeHightLightText)
        var highLightLength = ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(highLightText)
        var afterLength =ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(afterHighLightText)

        //Paint text before highlighting
        ctx.gc.setPaint(Color.BLACK)
        ctx.gc.drawString(beforeHightLightText,x.toInt, (y*ctx.unitY + ctx.smallY).toInt)

        //Paint highlighted text
        ctx.gc.setPaint(Color.RED)
        ctx.gc.drawString(highLightText,
          (x+beforeLength).toInt,
          (y*ctx.unitY + ctx.smallY).toInt)

        //Paint text after highlighting
        ctx.gc.setPaint(Color.BLACK)
        ctx.gc.drawString(afterHighLightText,
          (x+beforeLength+highLightLength).toInt,
          (y*ctx.unitY + ctx.smallY).toInt)


      case BoxWithText(text,bx,by,bwidth,bheight)=>
        ctx.gc.setPaint(Color.BLACK)
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.drawRect(
          (bx*ctx.unitX + ctx.smallX).toInt,
          (by*ctx.unitY + ctx.smallY).toInt,
          (bwidth*ctx.unitX - 2*ctx.smallX).toInt,
          (bheight*ctx.unitY - 2*ctx.smallY).toInt
        )


        val textX = ((bx*ctx.unitX + ctx.smallX)+(bwidth*ctx.unitX - 2*ctx.smallX))-(ctx.gc.getFont.getSize*text.size)/*Math.min((ctx.gc.getFont.getSize*text.size),(bwidth*ctx.unitX - 2*ctx.smallX)/2)*/
        val textY =  ((by*ctx.unitY + ctx.smallY)+(bheight*ctx.unitY - 2*ctx.smallY)) /*((by*ctx.unitY + ctx.smallY)+(bheight*ctx.unitY - 2*ctx.smallY))-((bheight*ctx.unitY - 2*ctx.smallY)*0.025)*/
        //ctx.gc.setFont(new Font(ctx.gc.getFont.getName,10))
        ctx.gc.drawString(text,
          textX.toInt,
          textY.toInt)

      case Rectangle(x, y, w, h) =>

        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setPaint(Color.green)
        ctx.gc.fillRect(
          (x*ctx.unitX + ctx.smallX*4).toInt,
          (y*ctx.unitY + ctx.smallY*4).toInt,
          (w*ctx.unitX - 2*ctx.smallX).toInt,
          (h*ctx.unitY - 2*ctx.smallY).toInt
        )
      case CorneredClause(x, y, w, h) =>

        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setPaint(Color.BLACK)
        //Left line
        ctx.gc.drawLine(
          (x*ctx.unitX + ctx.smallX).toInt,
          (y*ctx.unitY + ctx.smallY).toInt,
          (x*ctx.unitX + ctx.smallX).toInt,
          ((y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)).toInt
        )
        //Left upper corner
        ctx.gc.drawLine(
          (x*ctx.unitX + ctx.smallX).toInt,
          ( y*ctx.unitY + ctx.smallY).toInt,
          (x*ctx.unitX + ctx.smallX+(2)).toInt,
          (y*ctx.unitY + ctx.smallY).toInt
        )
        //Left lower corner
        ctx.gc.drawLine(
          (x*ctx.unitX + ctx.smallX).toInt,
          ((y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)).toInt,
          (x*ctx.unitX + ctx.smallX+(2)).toInt,
          ((y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)).toInt
        )

        //Right line
        ctx.gc.drawLine(
          ((x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)).toInt,
          (y*ctx.unitY + ctx.smallY).toInt,
          ((x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)).toInt,
          ((y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)).toInt
        )
        //Right upper corner
        ctx.gc.drawLine(
          ((x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)).toInt,
          (y*ctx.unitY + ctx.smallY).toInt,
          ((x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)-(2)).toInt,
          (y*ctx.unitY + ctx.smallY).toInt
        )
        //Right lower corner
        ctx.gc.drawLine(
          ((x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)).toInt,
          ((y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)).toInt,
          ((x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)-(2)).toInt,
          ((y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)).toInt
        )
      case Seperator(x,y)=>{
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setPaint(Color.BLACK)
        ctx.gc.drawLine(
        (x*ctx.unitX + ctx.smallX).toInt,
          (y*ctx.unitY + ctx.smallY).toInt,
          (x*ctx.unitX + ctx.smallX).toInt,
          (y*ctx.unitY + ctx.smallY-(2)).toInt
        )
      }
      case Box(x, y, w, h) =>
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setPaint(Color.RED)
        ctx.gc.drawRect(
          (x*ctx.unitX + ctx.smallX).toInt,
          (y*ctx.unitY + ctx.smallY).toInt,
          (w*ctx.unitX - 2*ctx.smallX).toInt,
          (h*ctx.unitY - 2*ctx.smallY).toInt)
    }
  }

  /*private def drawArrow(gc:GraphicsContext, node1X:Double, node1Y:Double, node2X:Double, node2Y:Double) {
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
  */
}
