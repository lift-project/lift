package generators

import test._

object ReduceSeqGenerator {
  
   def generate(r: ReduceSeq) : String = {
     val (elemT, len) = r.inT.extractArrayType()
     
     val fName = Fun.getName(r.f)
     val idValue = "0" // r.id
     val typeName = "int" // r.f.inT (binary func...)
     val inputVarName = "input" // has to be passed down here ...
     val outputVarName = "output" // has to be allocated ...
     
     "{ /* reduce_seq */\n" +
      	generateInit(typeName, idValue) +
      	generateLoop(fName, len, inputVarName) +
      	generateWriteBack(outputVarName, len) +
     "} /* reduce_seq */"
  }
   
   def generateInit(typeName: String, idValue: String) : String = {
     typeName + " acc = " + idValue + ";\n"
   }
   
   def generateLoop(fName: String, len: Expr, inputVarName: String) : String = {
     val indexVar = "i"
     val init = "0"
     val cond = len
     val update = "1"
     
     // apply index function one after the other following the stacks order ...
     val inputAccess = OpenCLGenerator.foldNewToOld(indexVar)((index, accessFun) => { accessFun(index) })
     
     val body = "  acc = " + fName + "(acc, " + inputVarName + "[" + inputAccess + "]);\n"
     
     LoopGenerator.generate(indexVar, init, cond, update, body)
   }
   
   def generateWriteBack(outputVarName: String, len: Expr) : String = {
     OpenCLGenerator.pushAccessFunction((index: String) => { "(" + index + ") / " + len })
     val outputAccess = OpenCLGenerator.foldOldToNew("0")((index, accessFun) => { accessFun(index) })
     OpenCLGenerator.popAccessFunction()
     
     outputVarName + "[" + outputAccess + "] = acc;\n"
   }

}