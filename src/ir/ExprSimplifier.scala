package ir

object ExprSimplifier {

  private def primeFactors(n: Int, i: Int =2) : List[Int] = {
    if (i >= n)
      return List(n)

    if (n % i == 0)
      i :: primeFactors(n/i, i)
    else
      primeFactors(n, i+1)
  }

  private def simplifyPow(pow: Pow): ArithExpr = {
    pow match {
      case Pow(Cst(0), Cst(0)) => throw new NotEvaluableException(pow.toString())
      case Pow(Cst(b), Cst(e)) =>
        val powDbl = scala.math.pow(b,e)
        if (powDbl.isValidInt)
          Cst(powDbl.toInt)
        else {
          if (e < 0) {
            // x>0, y>0, pow(x,-y) = pow(pow(x,y), -1)
            val powDbl = scala.math.pow(b,-e)
            if (powDbl.isValidInt)
              Pow(powDbl.toInt, -1)
            else
              pow
          } else
            pow
        }
      case Pow(base, Cst(0)) => Cst(1)
      case Pow(base, Cst(1)) => base
      case Pow(Cst(0), _) => Cst(0)
      case Pow(Cst(1), _) => Cst(1)

      // This rule should not be used since prodFactorsSimplify turns x^m * x^n into x^(m+n)
      // x^(m+n) => x^m * x^n
      case Pow(base, Sum(terms)) =>
        val newProd = Prod(terms.map(t => simplifyPow(Pow(base, t))))
        val simpNewProd = simplifyProd(newProd)
        simpNewProd

      // (x*y)^(n) => x^n * y^n
      case Pow(Prod(terms), exp) => simplifyProd(Prod(terms.map(t => simplifyPow(Pow(t, exp)))))

      // (x^e1)^e2 => x^(e1*e2)
      case Pow(Pow(b, e1), e2) => simplifyPow(Pow(b, simplifyProd(e1 * e2)))

      // x^(a*log(x,b)*c) => b^(a*b)
      case Pow(x,Prod(factors)) =>
        var b : ArithExpr = null
        val idx = factors.indexWhere({
          case Log(_,b1) =>
            b = b1
            true
          case _ => false
        })
        if (idx != -1)
          simplifyPow(Pow(b, simplifyProd(Prod(factors.diff(List(factors(idx)))))))
        else
          pow

      // x^log(x,b) => b
      case Pow(x1,Log(x2,b)) if x1 == x2 => b



      case _ => pow
    }
  }


  private def flattenSum(sum: Sum) : Sum = {
    var result = List[ArithExpr]()

    sum.terms.foreach {
      case s: Sum => result = result ++ s.terms
      case t => result = t :: result
    }

    Sum(result)
  }

  private def simplifySum(sum: Sum): ArithExpr = {
    var resultSum = sum

    resultSum = flattenSum(resultSum)

    resultSum = simplifySumTerms(resultSum)

    // constant folding
    resultSum = sumCstFolding(resultSum)

    if (resultSum.terms.length == 1)
      return resultSum.terms(0)

    resultSum
  }

  private def simplifySumTerms(sum: Sum): Sum = {

    val terms: List[ArithExpr] = sum.terms

    def simplifyTerm(i: Int, ae: ArithExpr): Option[Sum] = {
      val vars = Var.getVars(ae)

      for (j <- 0 until vars.length) {
        val v = vars(j)

        for (k <- i + 1 until terms.length) {
          val term = terms(k)

          if (ArithExpr.contains(term, v)) {
            val e: Sum = ae / v + term / v
            val simplified = ExprSimplifier.simplify(e)

            if (!simplified.isInstanceOf[Sum])
              return Some(simplifySumTerms(Sum(ExprSimplifier.simplify(v * simplified) :: terms.slice(0, i) ++ terms.slice(i + 1, k) ++ terms.slice(k + 1, terms.length))))

          }
        }
      }
      None
    }

    for (i <- 0 until terms.length) {
      terms(i) match {
        case p: Var =>
          simplifyTerm(i, p) match {
            case Some(toReturn) => return toReturn
            case None =>
          }

        case p: Product =>
          simplifyTerm(i, p) match {
            case Some(toReturn) => return toReturn
            case None =>
          }

        case t =>
      }
    }

    sum
  }

  // constant folding
  private def cstFolding(l : List[ArithExpr], op : ((Double,Double) => Double), neutral: Int) : List[ArithExpr] = {

    // fixed point iteration until everything has been folded
    var change = true
    var curLen = l.length
    var curResult = l
    while (change) {

      var cstVal : Int = neutral
      var newResult = List[ArithExpr]()

      curResult.foreach(e =>
        try {
          val cstValDbl : Double = op(e.evalDbl(),cstVal)
          if (cstValDbl.isValidInt)
            cstVal = cstValDbl.toInt
          else
            newResult = e :: newResult
        } catch {
          case nee : NotEvaluableException => newResult = e :: newResult
        })

      newResult = if (cstVal != neutral || (cstVal == neutral && newResult.length == 0))
        cstVal :: newResult
      else
        newResult

      change = newResult.length != curLen
      curLen = newResult.length
      curResult = newResult
    }

    curResult

  }


  private def sumCstFolding(sum: Sum) : Sum =
    Sum(cstFolding(sum.terms, (x,y) => x+y, 0))

  private def prodCstFolding(prod: Prod) : Prod =
    Prod(cstFolding(prod.factors, (x,y) => x*y, 1))



  private def prodFactorsSimplify(prod: Prod) : Prod = {

    val powMap = scala.collection.mutable.LinkedHashMap[ArithExpr, Sum]()

    prod.factors.foreach(factor => {

      val (base:ArithExpr,exp:ArithExpr) = factor match {
        case Pow(b, e) => (b,e)
        case _ =>  (factor,Cst(1)) // implicit exponent (1)
      }

      base match {
        case Cst(0) => return Prod(List(0))
        case Cst(c) =>
          // fractions simplification
          val factors = primeFactors(c)
          factors.foreach(factor => {
            val baseexp = (Cst(factor), powMap.getOrElse(factor, Sum(List(0))) + exp)
            powMap += baseexp
          })

        case _ => powMap += base -> (powMap.getOrElse(base, Sum(List(0))) + exp)
      }

    }
  )

    Prod(powMap.map({case (e, s) => simplifyPow(Pow(e,simplifySum(s)))}).toList)
  }

  private def reorderProd(prod: Prod) : Prod = {
    // partition into all the pows and the rest ...
    val (pows, rest) = prod.factors.partition( {
      case p: Pow => true
      case _ => false
    })
    // ... put the rest first and then the pows
    Prod(rest ++ pows)
  }

  private def distribute(prod: Prod) : ArithExpr = {

    var sums = List[Sum]()
    var prodRest = Prod(List())

    prod.factors.foreach {
      case s: Sum => sums = sums :+ s
      case t => prodRest = prodRest * t
    }

    sums = sums :+ Sum(List(prodRest))

    simplifySum(sums.reduce((s1, s2) => Sum(s1.terms.map(t1 => s2.terms.map(t2 => simplifyProd(t1 * t2))).flatten)))
  }

  private def flattenProd(prod: Prod) : Prod = {
    var result = List[ArithExpr]()

    prod.factors.foreach {
      case p: Prod => result = result ++ p.factors
      case t => result = t :: result
    }

    Prod(result)
  }

  private def simplifyProd(prod: Prod): ArithExpr = {
    var resultProd = prod

    resultProd = flattenProd(resultProd)

    // factors simplification
    resultProd = prodFactorsSimplify(resultProd)

    // constant folding (important that this happens after power simplification since the previous phase could create more constants)
    resultProd = prodCstFolding(resultProd)

    if (resultProd.factors.length == 1)
      return resultProd.factors(0)

    // commutativity: reorder elements, because with integer semantics e.g.: 1/M * N != N * 1/M
    resultProd = reorderProd(resultProd)

    // distributivity
    val result = distribute(resultProd)


    result
  }

  def simplify(e: ArithExpr): ArithExpr = {

    // recurse inside first
    var result = e match {
      case ArithExprFunction() => e
      case Cst(_) => e
      case Var(_,_) => e
      case Pow(base, exp) => Pow(simplify(base), simplify(exp))
      case Log(b,x) => Log(simplify(b),simplify(x))
      case Mod(dividend, divisor) => Mod(simplify(dividend),simplify(divisor))
      case And(l,r) => And(simplify(l),simplify(r))
      case Prod(factors) => Prod(factors.map(t => simplify(t)))
      case Sum(terms) => Sum(terms.map(t => simplify(t)))
    }

    result = result match {
      case p: Pow => simplifyPow(p)
      case p: Prod => simplifyProd(p)
      case s: Sum => simplifySum(s)
      case _ => result
    }

    result
  }

}
