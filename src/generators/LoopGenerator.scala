package generators

import test.Expr
import test.RangeAdd

object LoopGenerator {
    
  def generate(indexVar: Expr, range: RangeAdd, body: String) : String = {
    // TODO: Do "analysis" range and don't create loop if not necessary ...
    
    "for (int " + indexVar + " = " + range.start + "; " + indexVar + " < " + range.stop + "; " +
    indexVar + " += " + range.step + ") {\n" + body + "}\n";
  }

}