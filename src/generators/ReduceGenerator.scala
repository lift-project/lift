package generators

import test._

object ReduceSeqGenerator {
  
   def generate(r: ReduceSeq, accessFunctions: Array[generators.AccessFunction]) : String = {
     val elemT = Type.getElemT(r.inT)
     val len = Type.getLength(r.inT)
     
     val fName = OpenCLGenerator.generate(r.f, accessFunctions) // kind of expecting a name here ...
     val typeName = "int" // r.f.inT (binary func...)
     val inputVarName = "input" // has to be passed down here ...
     val outputVarName = "output" // has to be allocated ...
     
     "{ /* reduce_seq */\n" +
      	generateInit(typeName, r.id) +
      	generateLoop(fName, len, inputVarName, accessFunctions) +
      	generateWriteBack(outputVarName, len, accessFunctions) +
     "} /* reduce_seq */"
  }
   
   def generateInit(typeName: String, idValue: Expr) : String = {
     typeName + " acc = " + idValue + ";\n"
   }
   
   def generateLoop(fName: String, len: Expr, inputVarName: String, accessFunctions: Array[generators.AccessFunction]) : String = {
     val range = RangeAdd(Cst(0), len, Cst(1))
     val indexVar = Var("i") // range
     
     // apply index function one after the other following the FIFO order ...
     val inputAccess = accessFunctions.foldRight[Expr](indexVar)((accessFun, index) => { accessFun(index) })
     
     val body = "  acc = " + fName + "(acc, " + inputVarName + "[" + inputAccess + "]);\n"
     
     LoopGenerator.generate(indexVar, range, body)
   }
   
   def generateWriteBack(outputVarName: String, len: Expr, accessFunctions: Array[generators.AccessFunction]) : String = {
     val accessFun = (index: Expr) => { index / len } // add access function for the output
     // apply index function one after the other following the LIFO order ...
     val outputAccess = (accessFunctions :+ accessFun).foldLeft[Expr](Cst(0))((index, accessFun) => { accessFun(index) })
     
     outputVarName + "[" + outputAccess + "] = acc;\n"
   }

}

