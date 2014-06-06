package test

class Constraints(val maxMapDepth: Int, val onlyTerminal: Boolean, val randomOnly: Boolean = false) {
  def setOnlyTerminal() : Constraints = new Constraints(maxMapDepth, onlyTerminal)
  var fixedFuns : Set[Fun] = Set() // set of functions that shouldn't be derived
  
  def canDerive(f: Fun) = !fixedFuns.contains(f)
}