package generators

import test._

object MapWgrGenerator {
  
  def ifndefDefine(name: String, value: String) : String = {
    "#ifndef " + name + "\n" +
    "#define " + name + " (" + value + ")\n" +
    "#endif\n"
  }
  
  def undef(name: String) : String = {
    "#undef " + name + "\n"
  }
  
  /*
  def extractNestedType(t: Type) : (Type, Expr) = {
    t match {
      case ArrayType(elemT, len) => (elemT, len)
      case _ => { assert(false); (Type, Expr) }
    }
  }
  */
  
  def generate(m: MapWrg) : String = {
    // extract the nested type information
    val (elemT, len) = m.inT match {
      case ArrayType(elemT, len) => (elemT, len)
      case _ => { assert(false); (Type, Expr) }
    }
    
    ifndefDefine("WG_SIZE", len.toString) +
    generateLoop(m) +
    undef("WG_SIZE") +
    "return;\n"
  }
  
  def generateLoop(m: MapWrg) : String = {
    val indexVar = "g_id"
    val init = "get_group_id(0)"
    val cond = "WG_SIZE"
    val update = "GET_NUM_GROUPS"
    val body = (i : String) => {
      OpenCLGenerator.generate(m.f) + 
      i
    }
    generateLoop(indexVar, init, cond, update, body)
  }
  
  def generateLoop(indexVar: String, init: String, cond: String,
                   update: String, body: (String => String)) : String = {
    "for (int " + indexVar + " = " + init + "; " + indexVar + " < " + cond + "; " +
    indexVar + " += " + update + ") {\n" + body(indexVar) + "}\n";
  }
  
}