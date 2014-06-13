package junit

import ir._
import org.junit.Assert._
import org.junit.Test


class TestExpr {

    @Test def testSimplifcation() {

      val c0 = Cst(0)
      val c1 = Cst(1)
      val c2 = Cst(2)
      val c10 = Cst(10)
      val v = Var(new RangeMul(c0,c2,c10))
      val e = (c0+c1)*(c10+c2)+(c10/c2)
      
    assertEquals(e.eval, Expr.simplify(e).eval)
    
    println(e.eval())
    val result = Expr.simplify(e)
    println(result.eval())
    
    assertEquals(Cst(17),result)
    
    val e2 = (c0/v+c1/v)*v*(c10+c2)+(c10/c2)
    val result2 = Expr.simplify(e)
    println(result2)
    
    }
  
}