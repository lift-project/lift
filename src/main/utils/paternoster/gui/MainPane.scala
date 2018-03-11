package utils.paternoster.gui

import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.image.WritableImage
import javafx.scene.layout.Pane
import javafx.scene.text.{Font, Text}

import org.jfree.graphics2d.svg.SVGGraphics2D
import utils.paternoster.logic.Graphics.GraphicalPrimitive



/**
  * Created by federico on 16/08/17.
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

  def draw(primitives:Iterable[GraphicalPrimitive]) = {
    val gc = this.canvas.getGraphicsContext2D
    val context = JavaFXRenderer.Context(gc, unitX, unitY, smallX, smallY, getNumberFontFx() , getExpressionFontFx() ,width.toDouble,height.toDouble)
    JavaFXRenderer.drawPrimitives(primitives, context)
  }

  def renderToSvg(primitives:Iterable[GraphicalPrimitive]): String ={
    var g2 = new SVGGraphics2D(canvas.getWidth.toInt,canvas.getHeight.toInt);
    val context = SVGRenderer.Context(g2, unitX, unitY, smallX, smallY, getNumberFontAwt() , getExpressionFontAwt() ,width.toDouble,height.toDouble)
    SVGRenderer.drawPrimitives(primitives,context)
    var svgOuptut = g2.getSVGElement()
    svgOuptut
  }
  def getStringHeight(str:String,font: javafx.scene.text.Font): Double ={
    var text = new Text(str)
    text.setFont(font)
    var textHeight = text.getLayoutBounds().getHeight / unitY
    textHeight
  }

  def getNumberFontAwt():java.awt.Font={
    new java.awt.Font(canvas.getGraphicsContext2D.getFont.getName,java.awt.Font.PLAIN,10)
  }

  def getExpressionFontAwt() :java.awt.Font={
    new java.awt.Font(canvas.getGraphicsContext2D.getFont.getName,java.awt.Font.PLAIN,15)
  }

  def getNumberFontFx():Font={
    new Font(canvas.getGraphicsContext2D.getFont.getName,10)
  }

  def getExpressionFontFx() :Font={
    new Font(canvas.getGraphicsContext2D.getFont.getName,15)
  }

  def getSnapShot(wim: WritableImage): Unit ={
    canvas.snapshot(null,wim)
  }
  def getFontSize():Double ={
    this.canvas.getGraphicsContext2D.getFont.getSize
  }
  def getGraphicsContext(): GraphicsContext ={
    canvas.getGraphicsContext2D
  }
  def setCanvasWidth(width:Double):Unit={
    this.canvas.setWidth(width)
  }
  def setCanvasHeight(height:Double):Unit={
  this.canvas.setHeight(height)
  }
}
