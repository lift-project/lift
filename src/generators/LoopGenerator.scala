package generators

import test.Expr

object LoopGenerator {
  
  // TODO: should init and update also be an Expr ...
  def generate(indexVar: String, init: String, cond: Expr,
               update: String, body: String) : String = {
    // TODO: Do "analysis" on cond
    
    "for (int " + indexVar + " = " + init + "; " + indexVar + " < " + cond.toString + "; " +
    indexVar + " += " + update + ") {\n" + body + "}\n";
  }

}