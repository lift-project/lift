package exploration

import ir.Fun

class Constraints(val maxMapDepth: Int, val onlyTerminal: Boolean, val randomOnly: Boolean = false) {
  
  def setOnlyTerminal() : Constraints = new Constraints(maxMapDepth, onlyTerminal)
  var fixedFuns : Set[Fun] = Set() // set of functions that shouldn't be derived
  
  /*var horDeriv = 0 // horizontal derivations
  def incHorDeriv = {
    val c = new Constraints(maxMapDepth, onlyTerminal, randomOnly)
    c.horDeriv += 1
    c
  }*/
  
  def canDerive(f: Fun) = !fixedFuns.contains(f)
}