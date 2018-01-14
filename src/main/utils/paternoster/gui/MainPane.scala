package utils.paternoster.gui

import javafx.scene.canvas.Canvas
import javafx.scene.layout.Pane

import utils.paternoster.logic.Graphics.GraphicalPrimitive


/**
  * Created by federico on 16/08/17.
  */

class MainPane(val width:Int, val height:Int) extends Pane {
  //General scaling
  val unitX = 20
  val unitY = 20
  //Used to separate things
  val smallX = 4
  val smallY = 4
  val canvas = new Canvas(width,height)
  this.getChildren.add(canvas)

  def draw(primitives:Iterable[GraphicalPrimitive]) = {
    val gc = this.canvas.getGraphicsContext2D
    gc.clearRect(0, 0, width, height)
    val context = JavaFXRenderer.Context(gc, unitX, unitY, smallX, smallY)
    JavaFXRenderer.drawPrimitives(primitives, context)
  }


}
