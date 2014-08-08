package junit

import ir._
import org.junit.Assert._
import org.junit.Test
import scala.util.Random



class TestExpr {

  private val rnd = new Random(0)

  private def rndPositive() : Int = {
    val r = rnd.nextInt(5)
    if (r == 0)
      r+1
    else if (r < 0)
      -r
    else
      r
  }

  private def rndExpr(maxDepth: Int, depth: Int=0) : ArithExpr = {

    if (depth > maxDepth)
      return Cst(rndPositive())

    Random.nextInt(3) match {
      case 0 => rndExpr(maxDepth, depth+1) * rndExpr(maxDepth, depth+1)
      case 1 => rndExpr(maxDepth, depth+1) + rndExpr(maxDepth, depth+1)
      case 2 => rndExpr(maxDepth, depth+1) / rndExpr(maxDepth, depth+1)
    }
  }

  def testRandom() {
    val re = rndExpr(3)
    println(re)
    println(ExprSimplifier.simplify(re))

    val ori = re.evalDbl()
    val sim = ExprSimplifier.simplify(re).evalDbl()
    assert(math.abs((ori-sim)) <= 1.0/1000000.0, ori+" != "+sim)

  }

    @Test def testSimplifcation() {

      testRandom()

      val c0 = Cst(0)
      val c1 = Cst(1)
      val c2 = Cst(2)
      val c10 = Cst(10)
      val v = Var(new RangeMul(c0,c2,c10))
      val e = (c0+c1)*(c10+c2)+(c10/c2)

      ExprSimplifier.simplify(e).eval()

    assertEquals(e.eval(), ExprSimplifier.simplify(e).eval())
    
    //println(e.eval())
    val result = ExprSimplifier.simplify(e)
    //println(result.eval())
    
    assertEquals(Cst(17),result)
    
    val e2 = (c0/v+c1/v)*v*(c10+c2)+(c10/c2)
    val result2 = ExprSimplifier.simplify(e)
    //println(result2)



    
    }

  @Test def OneByOne() {
    assertEquals(Cst(1), ExprSimplifier.simplify(Cst(1) / Cst(1)))
  }
  
}