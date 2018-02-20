package utils.paternoster.gui

import javafx.scene.paint.Color

import utils.paternoster.gui.JavaFXRenderer.{Context, adjustCanvas, adjustScaling, drawPrimitive}
import utils.paternoster.logic.Graphics.GraphicalPrimitive


object SVGRenderer {
  case class Context(unitX:Double, unitY:Double, smallX:Double, smallY:Double, width:Double,height:Double)

  def drawPrimitives(primitives:Iterable[GraphicalPrimitive], svgFile:Context):Unit ={

  }
}
