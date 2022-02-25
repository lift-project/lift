package utils.paternoster.rendering

/*
import javafx.scene.canvas.GraphicsContext
import javafx.scene.paint.Color
import javafx.scene.text.{Font, Text}

import utils.paternoster.rendering.Graphics._
import utils.paternoster.visualisation.TypeVisualiser

/**
  * Handles drawing of the primitives to the user interface.
  */
object JavaFXRenderer {

  val SEPERATOR_HEIGHT = 4
  val NUMBER_Y_MARGIN_TO_ARRAYBOX = 2

  /**
    * The class capsules information on scaling, spacing and used fonts als well as the context of the canvas.
    * @param gc The context of the canvas.
    * @param unitX The x-scaling.
    * @param unitY The y-scaling.
    * @param smallX The x space between shapes.
    * @param smallY The y space between shapes.
    * @param numberFont The font that is used for the arraysize numbers.
    * @param expressionFont The font that is used for the source code display.
    * @param width The initial widht of the canvas.
    * @param height The initial height of the canvas.
    */
  case class Context(gc:GraphicsContext, unitX:Double, unitY:Double, smallX:Double, smallY:Double, numberFont :Font , expressionFont: Font , width:Double,height:Double)

  /**
    * Adjusts the canvas to fit the visualisation, clears the canvas and then draws the picture.
    * @param primitives The primitives that will be drawn.
    * @param ctx The grahics context.
    * @throws RuntimeException If the visualisation is bigger than the maximal displayable dimension in JavaFx
    */
  @throws(classOf[RuntimeException])
  def drawPrimitives(primitives:Iterable[GraphicalPrimitive], ctx:Context):Unit ={
    var newContext = ctx  //adjustScaling(primitives,ctx)
    newContext = adjustCanvas(primitives,newContext)
    ctx.gc.clearRect(0, 0, newContext.width, newContext.height)
    ctx.gc.setFill(Color.WHITE)
    ctx.gc.fillRect(0,0,newContext.width,newContext.height)
    primitives.foreach(drawPrimitive(_, newContext))
  }

  /**
    * The method checks if the visialisation fits on the canvas. If it does not fit then the canvas will be adjusted.
    * @param primitives The primitives of the visualisation.
    * @param ctx The graphics context.
    * @return The new Context.
    * @throws RuntimeException If the visualisation is bigger than the maximal displayable dimension in JavaFx
    */
  @throws(classOf[RuntimeException])
  def adjustCanvas(primitives:Iterable[GraphicalPrimitive],ctx:Context): Context ={
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
        var textObject = new Text(text)
        textObject.setFont(ctx.expressionFont)
        var textWidth = textObject.getLayoutBounds.getWidth / ctx.unitX
        var textHeight = textObject.getLayoutBounds.getHeight / ctx.unitY
        accumulatedHeight+= textHeight
        if(textWidth > maxWidth) maxWidth=textWidth
        if(textHeight > maxHeight) maxHeight = textHeight
      }
      case default =>
    })
    if(!(maxHeight*ctx.unitY > 16383 || maxWidth*ctx.unitX > 16383)){
      if(maxHeight*ctx.unitY > TypeVisualiser.getMainPane().height){
        TypeVisualiser.getMainPane().setCanvasHeight(maxHeight*ctx.unitY)
      }
      if(maxWidth*ctx.unitX > TypeVisualiser.getMainPane().width){
        TypeVisualiser.getMainPane().setCanvasWidth(maxWidth*ctx.unitX)
      }
      Context(TypeVisualiser.getMainPane().getGraphicsContext(), ctx.unitX, ctx.unitY, ctx.smallX, ctx.smallY,ctx.numberFont,ctx.expressionFont,TypeVisualiser.getMainPane().canvas.getWidth,TypeVisualiser.getMainPane().canvas.getHeight)
    }else{
      ctx.gc.clearRect(0, 0, TypeVisualiser.getMainPane().canvas.getWidth, TypeVisualiser.getMainPane().canvas.getHeight)
      ctx.gc.setFill(Color.WHITE)

      ctx.gc.setFont(new Font(ctx.expressionFont.getName,20))
      var message = "The visualisation is too big to be displayed.\nYou can set variables and groupings normally.\nBut to view the visualsisation it must be exported as scalable vector graphics (SVG)."
      var text = new Text(message)

      ctx.gc.strokeText(message,
        0,
        0+text.getLayoutBounds.getHeight)
      throw new RuntimeException("The visualisation can't be displayed.\nIt exceeds the maximal dimensions that are able to be displayed in a JavaFx Application.\n You may be able to save the graphics as vector graphic to view it.")
    }

  }


  /**
    * Draws a primitives to the user inteface.
    * @param primitive The primitives that will be drawn.
    * @param ctx The graphics context.
    */
  def drawPrimitive(primitive:GraphicalPrimitive, ctx: Context) = {

    primitive match {
      case DashedBox(x,y,w,h)=>
        ctx.gc.setLineDashes(2)
        ctx.gc.setLineDashOffset(1)
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.GREY)
        ctx.gc.strokeRect(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          w*ctx.unitX - 2*ctx.smallX,
          h*ctx.unitY - 2*ctx.smallY
        )

      case ExpressionSource(text,begin,end, x,y)=>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.expressionFont)

        var beforeHightLightText = text.substring(0,begin)
        var highLightText = text.substring(begin, end+1)
        var afterHighLightText = text.substring(end+1,text.length)

        var beforeLength = 0d
        var beforeHeight = 0d
        var highLightLength = 0d
        var highLightHeight = 0d

        if(!text.contains("\n")){
          var beforeText = new Text(beforeHightLightText)
          beforeText.setFont(ctx.expressionFont)
          beforeLength = beforeText.getLayoutBounds.getWidth


          var highLightTextO = new Text(highLightText)
          highLightTextO.setFont(ctx.expressionFont)
          highLightLength = highLightTextO.getLayoutBounds.getWidth+beforeLength
        }

        var beforeText = new Text(beforeHightLightText)
        beforeText.setFont(ctx.expressionFont)
        beforeHeight= if(beforeLength ==0) beforeText.getLayoutBounds.getHeight else 0

        var highLightTextO = new Text(highLightText)
        highLightTextO.setFont(ctx.expressionFont)
        highLightHeight = if( highLightLength ==0)  highLightTextO.getLayoutBounds.getHeight else 0


        //Paint text before highlighting
        ctx.gc.strokeText(beforeHightLightText,
          x*ctx.unitX,
          y*ctx.unitY + ctx.smallY)

        //Paint highlighted text
        ctx.gc.setStroke(Color.RED)
        ctx.gc.strokeText(highLightText,
          (x)*ctx.unitX+beforeLength,
          (y)*ctx.unitY + ctx.smallY+beforeHeight)

        //Paint text after highlighting
        ctx.gc.setStroke(Color.BLACK)
        ctx.gc.strokeText(afterHighLightText,
          (x)*ctx.unitX+highLightLength,
          (y)*ctx.unitY + ctx.smallY+highLightHeight+beforeHeight)

      case BoxWithText(text,bx,by,bwidth,bheight)=>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.BLACK)
        ctx.gc.strokeRect(
          bx*ctx.unitX + ctx.smallX,
          by*ctx.unitY + ctx.smallY,
          bwidth*ctx.unitX - 2*ctx.smallX,
          bheight*ctx.unitY - 2*ctx.smallY
        )



        val textX = ((bx*ctx.unitX + ctx.smallX)+(bwidth*ctx.unitX - 2*ctx.smallX))-(ctx.gc.getFont.getSize*text.size)
        val textY =  ((by*ctx.unitY + ctx.smallY)+(bheight*ctx.unitY - 2*ctx.smallY))- NUMBER_Y_MARGIN_TO_ARRAYBOX
        ctx.gc.strokeText(text,textX,textY)
      case Rectangle(x, y, w, h) =>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.DARKGREEN)
        ctx.gc.fillRect(
          Math.round(x*ctx.unitX + ctx.smallX*4),
          Math.round(y*ctx.unitY + ctx.smallY*4),
          Math.round(w*ctx.unitX - 2*ctx.smallX),
          Math.round(h*ctx.unitY - 2*ctx.smallY))
      case CorneredClause(x, y, w, h) =>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.BLACK)
        //Left line
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          x*ctx.unitX + ctx.smallX,
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )
        //Left upper corner
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          x*ctx.unitX + ctx.smallX+(2),
          y*ctx.unitY + ctx.smallY
        )
        //Left lower corner
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY),
          x*ctx.unitX + ctx.smallX+(2),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )

        //Right line
        ctx.gc.strokeLine(
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          y*ctx.unitY + ctx.smallY,
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )
        //Right upper corner
        ctx.gc.strokeLine(
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          y*ctx.unitY + ctx.smallY,
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)-(2),
          y*ctx.unitY + ctx.smallY
        )
        //Right lower corner
        ctx.gc.strokeLine(
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY),
          (x*ctx.unitX + ctx.smallX)+(w*ctx.unitX - 2*ctx.smallX)-(2),
          (y*ctx.unitY + ctx.smallY)+(h*ctx.unitY - 2*ctx.smallY)
        )
      case Seperator(x,y)=>{
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setFill(Color.BLACK)
        ctx.gc.strokeLine(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY-(SEPERATOR_HEIGHT))
      }
      case Box(x, y, w, h) =>
        ctx.gc.setLineDashes()
        ctx.gc.setFont(ctx.numberFont)
        ctx.gc.setStroke(Color.RED)
        ctx.gc.strokeRect(
          x*ctx.unitX + ctx.smallX,
          y*ctx.unitY + ctx.smallY,
          w*ctx.unitX - 2*ctx.smallX,
          h*ctx.unitY - 2*ctx.smallY)

      case _ => throw new NotImplementedError()
    }
  }


}
*/