package test


class Context extends Cloneable {
  
  var mapDepth : Int = 0
  var inMapGlb = false;
  var inMapWrg = false
  var inMapLcl = false
       
  def incMapDepth() : Context = {
    val c = this.copy()
    c.mapDepth += 1
    c
  }
  
  def setInMapGlb() : Context = {
    val c = this.copy()
    c.inMapGlb = true
    c
  }  
  
  def setInMapWrg() : Context = {
    val c = this.copy()
    c.inMapWrg = true
    c
  }
   
   def setInMapLcl() : Context = {
    val c = this.copy()
    c.inMapLcl = true
    c
  }
  
  /*override def toString(): String = {
    "Contex[mapDepth="+mapDepth+"]"
  }*/
  
  def copy() = this.clone().asInstanceOf[Context]
}