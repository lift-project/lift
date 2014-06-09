package generators

import test._

object OpenCLGenerator {
  
  
  
  def generate(f: Fun) : String = {
    assert(f.inT != UndefType)
    assert(f.ouT != UndefType)
    
    f match {
      case FPattern(inF,_) => generate(inF) + (f match {
        case m : Map => ""
        case _ => ""
      })
      //case m : Map => "get global id..."
      case _ => ""
    }
    
  }

}