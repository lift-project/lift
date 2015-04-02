package ir

import org.junit.Test
import org.junit.Assert._
import scala.reflect._

class Utils() {

  @Test def testListPossiblities() {

    val oriList = List(1, 2, 3, 4)
    val optionsList = List(List(), List(5, 6), List(7, 8), List(9))

    val testResults = Utils.listPossiblities(oriList, optionsList)
    val expectedResults = List(List(1, 5, 3, 4), List(1, 6, 3, 4), List(1, 2, 7, 4), List(1, 2, 8, 4), List(1, 2, 3, 9))
    
    assertEquals(testResults, expectedResults)
  }
  
}

object Utils {

	def listPossiblities[T](oriList : Seq[T], optionsList : Seq[Seq[T]]) : Seq[Seq[T]] = {
	    oriList.zip(optionsList).foldLeft((Seq[T](),Seq[Seq[T]]()))((state,p) => {
	      val ori = p._1
	      val options = p._2
	      var prefix = state._1
	      var results = state._2
	      results = results.map(x => x :+ ori) // add the current original element
	      results = results ++ options.map(x => prefix :+ x) // prepand the prefix to all the current options
	      prefix = prefix :+ ori	// prepare the prefix for next time      
	      (prefix,results)	      
	    } )._2	  
	}




  /*def randomDescent(f: FunExpr, inputType: Type, maxDepth: Int,
      constraints: Constraints = new Constraints(3, false)): FunExpr = {

    var c = constraints    
    
    // setup the context
    if (f.context == null)
      Context.updateContext(f, new Context())
    else
      Context.updateContext(f)

    // setup the types
    Type.check(f, inputType)
          
    if (maxDepth < 0)
      c = c.setOnlyTerminal

    val derivs = Rules.derivsWithOneRule(f, c, 2);
    if (derivs.isEmpty)
      return f;

    // select one derivation at random
    val rnd = Random.nextInt(derivs.length)
    //println(rnd+"/"+derivs.length)
    val randomDeriv = derivs(rnd)

    randomDescent(randomDeriv, inputType, maxDepth - 1, c)
  }*/

}
