package generators

import test._

object ReduceSeqGenerator {
  
   def generate(r: ReduceSeq, accessFunctions: Array[generators.AccessFunction]) : String = {
     val (elemT, len) = r.inT.extractArrayType()
     
     val fName = Fun.getName(r.f)
     val idValue = "0" // r.id
     val typeName = "int" // r.f.inT (binary func...)
     val inputVarName = "input" // has to be passed down here ...
     val outputVarName = "output" // has to be allocated ...
     
     "{ /* reduce_seq */\n" +
      	generateInit(typeName, idValue) +
      	generateLoop(fName, len, inputVarName, accessFunctions) +
      	generateWriteBack(outputVarName, len, accessFunctions) +
     "} /* reduce_seq */"
  }
   
   def generateInit(typeName: String, idValue: String) : String = {
     typeName + " acc = " + idValue + ";\n"
   }
   
   def generateLoop(fName: String, len: Expr, inputVarName: String, accessFunctions: Array[generators.AccessFunction]) : String = {
     val indexVar = Var("i")
     val init = Cst(0)
     val cond = len
     val update = Cst(1)
     
     // apply index function one after the other following the FIFO order ...
     val inputAccess = accessFunctions.foldRight[Expr](indexVar)((accessFun, index) => { accessFun(index) })
     
     val body = "  acc = " + fName + "(acc, " + inputVarName + "[" + inputAccess + "]);\n"
     
     LoopGenerator.generate(indexVar, init, cond, update, body)
   }
   
   def generateWriteBack(outputVarName: String, len: Expr, accessFunctions: Array[generators.AccessFunction]) : String = {
     val accessFun = (index: Expr) => { index / len } // add access function for the output
     // apply index function one after the other following the LIFO order ...
     val outputAccess = (accessFunctions :+ accessFun).foldLeft[Expr](Cst(0))((index, accessFun) => { accessFun(index) })
     
     outputVarName + "[" + outputAccess + "] = acc;\n"
   }

}

