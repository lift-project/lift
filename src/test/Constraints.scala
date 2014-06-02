package test

class Constraints(val maxMapDepth: Int, val onlyTerminal: Boolean) {
  def setOnlyTerminal() : Constraints = new Constraints(maxMapDepth, onlyTerminal)
}