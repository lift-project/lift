package utils.paternoster.rendering

/*
import java.awt.{BasicStroke, Color, Paint, RenderingHints}
import javafx.scene.text.Text

import org.jfree.graphics2d.svg.{SVGGraphics2D, SVGHints}
import utils.paternoster.rendering.Graphics._
import utils.paternoster.rendering.JavaFXRenderer.Context
import utils.paternoster.visualisation.TypeVisualiser

/**
  * Handles drawing of the primitives to the svg file.
  */
object SVGRenderer {
  /**
    * The class capsules information on scaling, spacing and used fonts als well as the context of the canvas.
    * @param gc The svg context that will be drawn to.
    * @param unitX The x-scaling.
    * @param unitY The y-scaling.
    * @param smallX The x space between shapes.
    * @param smallY The y space between shapes.
    * @param numberFont The font that is used for the arraysize numbers.
    * @param expressionFont The font that is used for the source code display.
    * @param width The widht of the canvas.
    * @param height The height of the canvas.
    */
  case class Context(gc:SVGGraphics2D, unitX:Double, unitY:Double, smallX:Double, smallY:Double, numberFont :java.awt.Font , expressionFont: java.awt.Font , width:Double,height:Double)

  /**
    * Draws all primitives to the the picture.
    * @param primitives The primitives that will be drawn.
    * @param ctx The grahics context.
    */
  def drawPrimitives(primitives:Iterable[GraphicalPrimitive], ctx:Context):Unit ={
    primitives.foreach(drawPrimitive(_, ctx))
  }

  /**
    * The method gets the visialisation dimensions
    * @param primitives The primitives of the visualisation.
    * @param ctx The graphics context.
    * @return A tuple of width and height of the visualisation.
    *
    */
  def getDimensions(primitives:Iterable[GraphicalPrimitive],ctx:Context): (Double,Double) ={
    var maxWidth=0.0
    var maxHeight=0.0
    var accumulatedHeight = 0.0
    primitives.foreach(primitive=> primitive match {
      case BoxWithText(_,x,y,w,h) => {
        val width = x+w
        val height = y+h
        accumulatedHeight+= height
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case Box(x,y,w,h)=>  {
        val width = x+w
        val height = y+h
        accumulatedHeight+= height
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case CorneredClause(x,y,w,h)=>  {
        val width = x+w
        val height = y+h
        accumulatedHeight+= height
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case Rectangle(x,y,w,h)=> {
        val width = x+w
        val height = y+h
        accumulatedHeight+= height
        if(width > maxWidth) maxWidth=width
        if(height > maxHeight) maxHeight = height
      }
      case ExpressionSource(text,beginHighlight,endHighLight,x,y)=>{
        var textWidth = ctx.gc.getFontMetrics(ctx.expressionFont).getStringBounds(text,ctx.gc).getWidth  / ctx.unitX
        var textHeight = ctx.gc.getFontMetrics(ctx.expressionFont).getStringBounds(text,ctx.gc).getHeight / ctx.unitY
        accumulatedHeight+= textHeight
        if(textWidth > maxWidth) maxWidth=textWidth
        if(textHeight > maxHeight) maxHeight = textHeight
      }
      case default =>
    })

  (maxWidth*ctx.unitX,accumulatedHeight*ctx.unitY)
  }

  /**
    * Draws a primitives to the svg context.
    * @param primitive The primitives that will be drawn.
    * @param ctx The graphics context.
    */
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

        var beforeLength = 0d
        var beforeHeight = 0d
        var beforeAndHighLightLength = 0d
        var highLightHeight = 0d

        val isMultiLine = text.contains("\n")

        if(!isMultiLine){
          beforeLength = ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(beforeHightLightText+" ")
          beforeAndHighLightLength = ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(highLightText+" ")+beforeLength
        }

        //Paint text before highlighting
        ctx.gc.setPaint(Color.BLACK)
        var beforeLines = beforeHightLightText.split("\n")
        var accLineHeight = 0d;
        for( bfl <- beforeLines){
          val spaceWidth = ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(" ")
          val numSpaces = countSpaces(bfl)
          val xOffset = numSpaces*spaceWidth
          drawTextLine(bfl.trim,x,y,xOffset,accLineHeight,ctx)
          accLineHeight+=  ctx.gc.getFontMetrics(ctx.expressionFont).getStringBounds(bfl,ctx.gc).getHeight
        }


        //Paint highlighted text
        ctx.gc.setPaint(Color.RED)
        if(!isMultiLine){
          drawTextLine(highLightText,x,y,beforeLength,0d,ctx)
        }else{
          val spaceWidth = ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(" ")
          val numSpaces = countSpaces(highLightText)
          val xOffset = numSpaces*spaceWidth
          drawTextLine(highLightText.trim,x,y,xOffset,accLineHeight,ctx)
          accLineHeight+= ctx.gc.getFontMetrics(ctx.expressionFont).getStringBounds(highLightText,ctx.gc).getHeight
        }


        //Paint text after highlighting
        ctx.gc.setPaint(Color.BLACK)
        if(!isMultiLine){
            drawTextLine(afterHighLightText,x,y,beforeAndHighLightLength,0d,ctx)
        }else{
          var afterLines = afterHighLightText.split("\n")
          for( aftl <- afterLines){
            val spaceWidth = ctx.gc.getFontMetrics(ctx.expressionFont).stringWidth(" ")
            val numSpaces = countSpaces(aftl)
            val xOffset = numSpaces*spaceWidth
            drawTextLine(aftl.trim,x,y,xOffset,accLineHeight,ctx)
            accLineHeight+=  ctx.gc.getFontMetrics(ctx.expressionFont).getStringBounds(aftl,ctx.gc).getHeight
          }
        }


      case BoxWithText(text,bx,by,bwidth,bheight)=>
        ctx.gc.setPaint(Color.BLACK)
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.drawRect(
          (bx*ctx.unitX + ctx.smallX).toInt,
          (by*ctx.unitY + ctx.smallY).toInt,
          (bwidth*ctx.unitX - 2*ctx.smallX).toInt,
          (bheight*ctx.unitY - 2*ctx.smallY).toInt
        )


        val textX = ((bx*ctx.unitX + ctx.smallX)+(bwidth*ctx.unitX - 2*ctx.smallX))-(ctx.gc.getFont.getSize*text.size)
        val textY =  ((by*ctx.unitY + ctx.smallY)+(bheight*ctx.unitY - 2*ctx.smallY))-JavaFXRenderer.NUMBER_Y_MARGIN_TO_ARRAYBOX
        //ctx.gc.setFont(new Font(ctx.gc.getFont.getName,10))
        ctx.gc.drawString(text,
          textX.toInt,
          textY.toInt)

      case Rectangle(x, y, w, h) =>

        ctx.gc.setFont(ctx.numberFont)

        var darkGreen = new Color(javafx.scene.paint.Color.DARKGREEN.getRed.toFloat,javafx.scene.paint.Color.DARKGREEN.getGreen.toFloat,javafx.scene.paint.Color.DARKGREEN.getBlue.toFloat)

        ctx.gc.setPaint(darkGreen)
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
          (y*ctx.unitY + ctx.smallY-(JavaFXRenderer.SEPERATOR_HEIGHT)).toInt
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

      case _ => throw new NotImplementedError()
    }
  }

  def countSpaces(text:String): Integer={
    var spaceCounter:Integer = 0
    text.foreach(char => if(char == ' ') spaceCounter+=1)
    spaceCounter
  }

  private def drawTextLine(text :String , x:Double ,y: Double,xOffset: Double , yOffset: Double,ctx: Context): Unit ={
    ctx.gc.drawString(text,
      (x*ctx.unitX+xOffset).toInt,
      (y*ctx.unitY + ctx.smallY+yOffset).toInt)
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
*/