package test


class Context() {

  def this(md: Int) {
    this()
    mapDepth = md
  }
  
  var mapDepth : Int = 0
    
  def incMapDepth() : Context = {new Context(this.mapDepth+1)}	
  
  /*override def toString(): String = {
    "Contex[mapDepth="+mapDepth+"]"
  }*/
  
  def copy() = new Context(mapDepth)
}