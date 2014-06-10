package generators

import test.Expr

object LoopGenerator {
  
  def generate(indexVar: Expr, init: Expr, cond: Expr,
               update: Expr, body: String) : String = {
    // TODO: Do "analysis" on cond
    
    "for (int " + indexVar + " = " + init + "; " + indexVar + " < " + cond.toString + "; " +
    indexVar + " += " + update + ") {\n" + body + "}\n";
  }

}