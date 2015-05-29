package arithmetic

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

  private def simplifyMod(m: Mod): ArithExpr = {

    m.divisor match {
      case Cst(1) =>
        return Cst(0)
      case _ =>
    }

    if (m.dividend == Cst(0) && m.divisor != Cst(0) || ArithExpr.multipleOf(m.dividend, m.divisor))
      return Cst(0)

    m.dividend match {
      case Mod(dividend, divisor) =>
        if (divisor == m.divisor)
          return Mod(dividend, divisor)
      case _ =>
    }

    if(ArithExpr.isSmaller(m.dividend, m.divisor)) {
      return m.dividend
    }

    m match {
      case Mod(Cst(_), Cst(_)) => m.eval()
      case Mod(Sum(terms), d) =>
        // (A + B) mod C = (A mod C + B mod C) mod C
        val newDividend = simplify(Sum(terms.map(Mod(_, d))))
        newDividend match {
          case Sum(newTerms) =>
            if (newTerms.length < terms.length) {
              val removedMods = newTerms.map({
                case Mod(dividend, m.divisor) => dividend
                case t => t
              })
              return Mod(Sum(removedMods), d)
            }
          case _ => return simplify(Mod(newDividend, d))
        }

        m
      case Mod(Prod(dividendFactors), Prod(divisorFactors)) =>
        val common = dividendFactors.intersect(divisorFactors)
        val diff = dividendFactors.diff(divisorFactors)
        if (common.length == divisorFactors.length && !ArithExpr.hasDivision(diff))
          return Cst(0)
        m
      case Mod(Prod(factors), d) =>
        // (A * B) mod C = (A mod C * B mod C) mod C
        val newDividend = simplify(Prod(factors.map(Mod(_, d))))
        if (!ArithExpr.hasDivision(factors))
          newDividend match {
            case Prod(newFactors) =>
              if (newFactors.length < factors.length){
                val removedMods = newFactors.map({
                  case Mod(dividend, m.divisor) => dividend
                  case t => t
                })
                return Mod(Prod(removedMods), d)
              }
            case _ => return simplify(Mod(newDividend, d))
          }
        m
      case _ => m
    }
  }

  private def simplifyFraction(f: IntDiv): ArithExpr = {
    if (f.denom == Cst(1))
      return f.numer

    if (f.numer == Cst(0))
      return Cst(0)

    if (f.numer == f.denom && f.denom != Cst(0))
      return Cst(1)

    if (ArithExpr.isSmaller(f.numer, f.denom))
      return Cst(0)

    f match {
      case IntDiv(Cst(_), Cst(_)) => return f.eval()
      case IntDiv(IntDiv(numer, denom1), denom2) => return IntDiv(numer, simplify(denom1 * denom2))
      case IntDiv(numer, Pow(base, Cst(-1))) => return simplify(numer * base)
      case IntDiv(Sum(terms), denom) =>
        var newTerms = List[ArithExpr]()
        var newFractions = List[ArithExpr]()
        for (term <- terms) {
          if (ArithExpr.multipleOf(term, denom))
            newFractions = IntDiv(term, denom) :: newFractions
          else
            newTerms = term :: newTerms
        }

        if (newFractions.nonEmpty)
          return simplify(Sum(newFractions) + IntDiv(Sum(newTerms), denom))

      case IntDiv(Prod(factors), denom) =>
        // If denom or any part of denom is part of factors, eliminate
        denom match {
          case Prod(denomFactors) =>
            val common = denomFactors.intersect(factors)
            if (common.nonEmpty){
              val newNumer = Prod(factors.diff(common))
              val newDenom = Prod(denomFactors.diff(common))
              return simplify(IntDiv(newNumer, newDenom))
            }

            simplifyFractionConstants(factors, denomFactors) match {
              case Some(toReturn) => return toReturn
              case None =>
            }

          case c: Cst =>
            simplifyFractionConstants(factors, List(c)) match {
              case Some(toReturn) => return toReturn
              case None =>
            }

          case _ =>
            if (factors.contains(denom)) {
              val index = factors.indexOf(denom)
              return simplify(Prod(factors.slice(0, index) ++ factors.slice(index+1, factors.length)))
            }
        }
      case IntDiv(_, denominator @ Var(_, _) ) =>
        val numeratorAtMax = f.numer.atMax
        val numeratorAtMin = f.numer.atMin

        if (ArithExpr.multipleOf(numeratorAtMax, denominator) && ArithExpr.multipleOf(numeratorAtMin, denominator)) {
          try {
            if (simplify((numeratorAtMax - numeratorAtMin) / denominator).eval() == 1)
              return simplify(numeratorAtMin / denominator).eval()
          } catch {
            case e: NotEvaluableException =>
          }
        }
      case _ =>
    }

    f
  }

  private def simplifyFractionConstants(factors: List[ArithExpr], denomFactors: List[ArithExpr]): Option[ArithExpr] = {
    val numerConstant = factors.filter(_.isInstanceOf[Cst])
    val denomConstant = denomFactors.filter(_.isInstanceOf[Cst])

    if (denomConstant.nonEmpty && numerConstant.nonEmpty) {
      val result = simplify(numerConstant.head /^ denomConstant.head)
      result match {
        case Pow(b, e) =>
          val numer = simplify(e * Cst(-1)) :: factors.diff(numerConstant)
          val denom = b :: denomFactors.diff(denomConstant)
          return Some(simplify(IntDiv(Prod(numer), Prod(denom))))
        case c: Cst =>
          val numer = c :: factors.diff(numerConstant)
          return Some(simplify(IntDiv(Prod(numer), Prod(denomFactors.diff(denomConstant)))))
        case _ =>
      }
    }
    None
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
      return resultSum.terms.head

    resultSum
  }

  private def simplifySumTerms(sum: Sum): Sum = {

    val terms = sum.terms

    def simplifyTerm(i: Int, ae: ArithExpr): Option[Sum] = {
      val vars = Var.getVars(ae)

      for (k <- i + 1 until terms.length) {
        val term = terms(k)

        // a = (a div d)*d + a mod d
        (ae, term) match {
          case (p: Prod, Mod(a, d)) =>
            val term1 = (a / d) * d
            if (p == term1)
              return Some(simplifySumTerms(Sum(a :: terms.slice(0, i) ++ terms.slice(i + 1, k) ++ terms.slice(k + 1, terms.length))))
          case (Mod(a, d), p: Prod) =>
            val term1 = (a / d) * d
            if (p == term1)
              return Some(simplifySumTerms(Sum(a :: terms.slice(0, i) ++ terms.slice(i + 1, k) ++ terms.slice(k + 1, terms.length))))
          // Constants
          case (p1: Prod, p2: Prod) =>
            val cst1 = p1.factors.find(_.isInstanceOf[Cst])
            val cst2 = p2.factors.find(_.isInstanceOf[Cst])

            (cst1, cst2) match {
              case (Some(Cst(c1)), Some(Cst(c2))) =>
                if (c1 % c2 == 0)
                  tryToSimplifyTermPair(terms, i, ae, k, term, c2) match {
                    case Some(toReturn) => return toReturn
                    case None =>
                  }
                else if (c2 % c1 == 0)
                  tryToSimplifyTermPair(terms, i, ae, k, term, c1) match {
                    case Some(toReturn) => return toReturn
                    case None =>
                  }

              case _ =>
            }
          case _ =>
        }

        for (j <- 0 until vars.length) {
          val v = vars(j)

          if (ArithExpr.contains(term, v)) {
            tryToSimplifyTermPair(terms, i, ae, k, term, v) match {
              case Some(toReturn) => return toReturn
              case None =>
            }

          }
        }
      }
      None
    }

    for (i <- 0 until terms.length) {
      terms(i) match {
        case term @ (Var(_,_) | Prod(_) | Mod(_, _)) =>
          simplifyTerm(i, term) match {
            case Some(toReturn) => return toReturn
            case None =>
          }
        case _ =>
      }
    }

    sum
  }

  private def tryToSimplifyTermPair(terms: List[ArithExpr], i: Int, ae: ArithExpr, k: Int, term: ArithExpr, v: ArithExpr): Option[Option[Sum]] = {
    val simplified = simplify(ae /^ v + term /^ v)

    var origHasMod = false
    var newHasMod = false

    ArithExpr.visit(simplified, origHasMod |= _.isInstanceOf[Mod])
    ArithExpr.visit(ae + term, newHasMod |= _.isInstanceOf[Mod])

    if (!simplified.isInstanceOf[Sum] | (origHasMod != newHasMod))
      return Some(Some(simplifySumTerms(Sum(ExprSimplifier.simplify(v * simplified) :: terms.slice(0, i) ++ terms.slice(i + 1, k) ++ terms.slice(k + 1, terms.length)))))
    None
  }

  /** Constant folding*/
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
      return resultProd.factors.head

    // commutativity: reorder elements, because with integer semantics e.g.: 1/M * N != N * 1/M
    resultProd = reorderProd(resultProd)

    // distributivity
    val result = distribute(resultProd)


    result
  }

  private def simplifyMin(m: Min): ArithExpr = (m.var1,m.var2) match {
    case(Cst(a),Cst(b)) =>
      Cst(if (a > b) b else a)
    case _ =>
      simplify(m.var1 - m.var2) match {
        case Cst(v) => if (v > 0) m.var2 else m.var1
        case _ => Min(m.var1,m.var2)
      }
  }

  private def simplifyMax(m: Max): ArithExpr = (m.var1,m.var2) match {
    case(Cst(a),Cst(b)) =>
      Cst(if (a < b) b else a)
    case _ =>
      simplify(m.var1 - m.var2) match {
        case Cst(v) => if (v < 0) m.var2 else m.var1
        case _ => Max(m.var1,m.var2)
      }
  }

  def simplify(e: ArithExpr): ArithExpr = {

    // recurse inside first
    var result = e match {
      case ArithExprFunction(_) => e
      case Cst(_) => e
      case Var(_,_) => e
      case Pow(base, exp) => Pow(simplify(base), simplify(exp))
      case Log(b,x) => Log(simplify(b),simplify(x))
      case Mod(dividend, divisor) => Mod(simplify(dividend),simplify(divisor))
      case And(l,r) => And(simplify(l),simplify(r))
      case Prod(factors) => Prod(factors.map(t => simplify(t)))
      case Sum(terms) => Sum(terms.map(t => simplify(t)))
      case IntDiv(n, d) => IntDiv(simplify(n), simplify(d))
      case Min(var1, var2) => Min(simplify(var1), simplify(var2))
      case Max(var1, var2) => Max(simplify(var1), simplify(var2))
      case _ => e
    }

    result = result match {
      case p: Pow => simplifyPow(p)
      case p: Prod => simplifyProd(p)
      case s: Sum => simplifySum(s)
      case m: Mod => simplifyMod(m)
      case f: IntDiv => simplifyFraction(f)
      case m: Min => simplifyMin(m)
      case m: Max => simplifyMax(m)
      case _ => result
    }

    result
  }

}
