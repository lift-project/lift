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
      case Pow(Cst(b), Cst(e)) => {
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
      }
      case Pow(base, Cst(0)) => Cst(1)
      case Pow(base, Cst(1)) => base
      case Pow(Cst(0), _) => Cst(0)
      case Pow(Cst(1), _) => Cst(1)
      case Pow(Prod(terms), exp) => simplifyProd(Prod(terms.map(t => simplifyPow(Pow(t, exp)))))
      case Pow(Pow(b,e1),e2) => simplifyPow(Pow(b,simplifyProd(e1*e2)))
      case Pow(_,_) => pow
    }
  }

  private def simplifySum(sum: Sum): ArithExpr = sum match {
    case Sum(terms) => {

      var resultSum = sum

      // constant folding
      resultSum = sumCstFolding(resultSum)

      if (resultSum.terms.length == 1)
        return resultSum.terms(0)

      resultSum
    }
  }

  // constant folding
  private def sumCstFolding(sum: Sum) : Sum = {

    var sumCsts = Sum(List())
    var sumRest = Sum(List())

    sum.terms.foreach(t => t match {
      case c: Cst => sumCsts = sumCsts + c
      case Pow(Cst(_),Cst(_)) => sumCsts = sumCsts + t
      case _ => sumRest = sumRest + t
    })

    val sumCstsEval = sumCsts.evalDbl()

    if (sumCstsEval.isValidInt)
      if (sumCstsEval.toInt == 0 && sumRest.terms.length > 0)
        sumRest // remove sum with 0 if we have something else
      else
        sumRest + sumCstsEval.toInt
    else
      sumRest + sumCsts
  }

  // constant folding
  private def prodCstFolding(prod: Prod) : Prod = {

    var prodCsts = Prod(List())
    var prodRest = Prod(List())

    prod.terms.foreach(t => t match {
      case c: Cst => prodCsts = prodCsts * c
      case Pow(Cst(_),Cst(_)) => prodCsts = prodCsts * t
      case _ => prodRest = prodRest * t
    })

    val prodCstsEval = prodCsts.evalDbl()

    if (prodCstsEval.isValidInt)
      if (prodCstsEval.toInt == 1 && prodRest.terms.length > 0)
        prodRest // remove prod with 1 if we have something else
      else
        prodRest * prodCstsEval.toInt
    else
      prodRest * prodCsts
  }

  private def prodPowSimplification(prod: Prod) : Prod = {

    val powMap = scala.collection.mutable.Map[ArithExpr, Sum]()

    prod.terms.foreach(term => {

      val (base:ArithExpr,exp:ArithExpr) = term match {
        case Pow(b, e) => (b,e)
        case _ =>  (term,Cst(1)) // implicit exponent (1)
      }

      base match {
        case Cst(c) => {
          // fractions simplification
          val factors = primeFactors(c)
          factors.foreach(factor => powMap += Cst(factor) -> (powMap.getOrElse(factor, Sum(List(0))) + exp))
        }
        case _ => powMap += base -> (powMap.getOrElse(base, Sum(List(0))) + exp)
      }

    }
  )

    Prod(powMap.map({case (e, s) => simplifyPow(Pow(e,simplifySum(s)))}).toList)
  }

  private def reorderProd(prod: Prod) : Prod = {
    // partition into all the pows and the rest ...
    val (pows, rest) = prod.terms.partition( {
      case p: Pow => true
      case _ => false
    })
    // ... put the rest first and then the pows
    Prod(rest ++ pows)
  }

  private def distribute(prod: Prod) : ArithExpr = {

    var sums = List[Sum]()
    var prodRest = Prod(List())

    prod.terms.foreach(t => t match {
      case s: Sum => sums = sums :+ s
      case _ => prodRest = prodRest * t
    })

    sums = sums :+ Sum(List(prodRest))

    simplifySum(sums.reduce((s1, s2) => Sum(s1.terms.map(t1 => s2.terms.map(t2 => simplifyProd(t1 * t2))).flatten)))
  }

  private def simplifyProd(prod: Prod): ArithExpr = {
    prod match {

      case Prod(terms) => {

        var resultProd = prod

        // power simplification
        resultProd = prodPowSimplification(resultProd)

        // constant folding (important that this happens after power simplification since the previous phase could create more constants)
        resultProd = prodCstFolding(resultProd)

        if (resultProd.terms.length == 1)
          return resultProd.terms(0)

        // commutativity: reorder elements, because with integer semantics e.g.: 1/M * N != N * 1/M
        resultProd = reorderProd(resultProd)

        // distributivity
        val result = distribute(resultProd)


        result
      }
    }
  }

  def simplify(e: ArithExpr): ArithExpr = {

    // recurse inside first
    var result = e match {
      case Pow(base, exp) => Pow(simplify(base), simplify(exp))
      case Prod(terms) => Prod(terms.map(t => simplify(t)))
      case Sum(terms) => Sum(terms.map(t => simplify(t)))
      case _ => e
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
