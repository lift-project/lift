package utils.paternoster.gui

/*import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.WritableImage
import javafx.scene.layout.Pane
import javafx.scene.text.{Font, Text}

import org.jfree.graphics2d.svg.SVGGraphics2D
import utils.paternoster.rendering.Graphics.GraphicalPrimitive
import utils.paternoster.rendering.{JavaFXRenderer, SVGRenderer}
*/
/*

/**
  * Custon Pane class for painting of the visualisation.
  */
class MainPane(val width:Int, val height:Int) extends Pane {
  //General scaling
  var unitX = 6d
  var unitY = 6d
  //Used to separate things
  val smallX = 1
  val smallY = 1
  val canvas = new Canvas(width,height)
  //canvas.setScaleX(0.5)
  //canvas.setScaleY(0.5)
  this.getChildren.add(canvas)

  /**
    * Draws all primitives to the canvas.
    * @param primitives The primitives that will be drawn.
    */
  def draw(primitives:Iterable[GraphicalPrimitive]) = {
    val gc = this.canvas.getGraphicsContext2D
    val context = JavaFXRenderer.Context(gc, unitX, unitY, smallX, smallY, getNumberFontFx() , getExpressionFontFx() ,width.toDouble,height.toDouble)
    JavaFXRenderer.drawPrimitives(primitives, context)
  }

  /**
    * Draws all primitives to an svg file.
    * @param primitives The primitives that will be drawn.
    * @return The svg content.
    */
  def renderToSvg(primitives:Iterable[GraphicalPrimitive],visualisationDimensions : (Double,Double)): String ={

    var g2 = new SVGGraphics2D(visualisationDimensions._1.toInt*unitX.toInt,visualisationDimensions._2.toInt*unitY.toInt);
    var context = SVGRenderer.Context(g2, unitX, unitY, smallX, smallY, getNumberFontAwt() , getExpressionFontAwt() ,width.toDouble,height.toDouble)
    SVGRenderer.drawPrimitives(primitives,context)
    var svgOuptut = g2.getSVGElement()
    svgOuptut
  }

  /**
    * Helper method that returns the height of a rendered String.
    * @param str The text.
    * @param font The font of the rendering.
    * @return The height of the text in the given font.
    */
  def getStringHeight(str:String,font: javafx.scene.text.Font): Double ={
    var text = new Text(str)
    text.setFont(font)
    var textHeight = text.getLayoutBounds().getHeight / unitY
    textHeight
  }

  /**
    * Number - Awt font getter.
    * @return The awt font for numbers.
    */
  def getNumberFontAwt():java.awt.Font={
    new java.awt.Font(canvas.getGraphicsContext2D.getFont.getName,java.awt.Font.PLAIN,10)
  }

  /**
    * Expression - Awt font getter.
    * @return The awt font for source code.
    */
  def getExpressionFontAwt() :java.awt.Font={
    new java.awt.Font(canvas.getGraphicsContext2D.getFont.getName,java.awt.Font.PLAIN,15)
  }

  /**
    * Number - JavaFx font getter.
    * @return The JavaFx font for numbers.
    */
  def getNumberFontFx():Font={
    new Font(canvas.getGraphicsContext2D.getFont.getName,10)
  }

  /**
    * Expression - JavaFx font getter.
    * @return The JavaFx font for source code.
    */
  def getExpressionFontFx() :Font={
    new Font(canvas.getGraphicsContext2D.getFont.getName,15)
  }

  /**
    * Creates a snapshot of the canvas and writes it to the given WritableImage.
    * @param wim The WritableImage that the snapshot will be drawn to.
    */
  def getSnapShot(wim: WritableImage): Unit ={
    canvas.snapshot(null,wim)
  }

  /**
    * Getter for the graphicsContext.
    * @return The context.
    */
  def getGraphicsContext(): GraphicsContext ={
    canvas.getGraphicsContext2D
  }

  /**
    * Setter for the canvas width.
    * @param width The new width.
    */
  def setCanvasWidth(width:Double):Unit={
    this.canvas.setWidth(width)
  }

  /**
    * Setter for the canvas height.
    * @param height The new height.
    */
  def setCanvasHeight(height:Double):Unit={
  this.canvas.setHeight(height)
  }
}

*/
