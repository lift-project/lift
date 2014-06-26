package ir


object ExprSimplifier {

  private def simplifyPow(pow: Pow): Expr = {

    pow match {
      case Pow(Cst(0), Cst(0)) => throw new NotEvaluableException(pow.toString())
      case Pow(Cst(b), Cst(e)) => {
        val powDbl = scala.math.pow(b,e)
        if (powDbl.isValidInt)
          Cst(powDbl.toInt)
        else
          pow
      }
      case Pow(base, Cst(0)) => Cst(1)
      case Pow(base, Cst(1)) => base
      case Pow(Cst(0), _) => Cst(0)
      case Pow(Cst(1), _) => Cst(1)
      case Pow(Prod(terms), exp) => simplifyProd(Prod(terms.map(t => simplifyPow(Pow(t, exp)))))
      case Pow(_,_) => pow
    }
  }

  private def simplifySum(sum: Sum): Expr = sum match {
    case Sum(terms) => {

      if (terms.length == 1)
        return terms(0)

      var cst : Double = 0
      var result : Expr = Sum(List())
      terms.map(t => t match {
        case Cst(c) => cst = cst+c
        case _ => result = result + t
      })

      if (cst != 0)
        if (cst.isValidInt)
          if (result == Sum(List()))
            result = Cst(cst.toInt)
          else
            result = Cst(cst.toInt) + result // constants always first
        else
          throw new NotEvaluableException(cst.toString)

      result
    }
  }

  private def simplifyProd(prod: Prod): Expr = {
    prod match {

      case Prod(terms) => {

        if (terms.length == 1)
          return terms(0)

        /*val baseExpMap = terms.foldLeft(new HashMap[Expr, Expr]())((map, e) => {
          e match {
            case Pow(b, e) => map + (b -> simplifySum(map.getOrElse(b, Cst(0)) + e))
            case _ => map + (e -> simplifySum(map.getOrElse(e, Cst(0)) + Cst(1)))
          }
        })*/


        var csts = List[Expr]()
        var sums = List[Sum]()
        var vars = List[Expr]() // Var or TypeVar
        var others = List[Expr]()
        var zero = false

        terms.foreach(t => t match {
          case Cst(0) => zero = true
          //case Cst(1) => // nothing to do // this is wrong and simplifies (1 * 1) to ()
          case c: Cst => csts = csts :+ c
          case Pow(Cst(_),Cst(_)) => csts = csts :+ t
          case s: Sum => sums = sums :+ s
          case Var(_,_)|TypeVar(_)|Pow(Var(_,_),_)|Pow(TypeVar(_),_) => vars = vars :+ t
          case _ => others = others :+ t
        })

        if (zero)
          return Cst(0)

        var prod: Prod = Prod(List())

        // constant folding
        if (csts.nonEmpty) {
          val prodCst = csts.map( {
            case Pow(Cst(b),Cst(e)) => math.pow(b,e)
            case Cst(c) => c.toDouble
          }).reduce((x,y) => x*y)

          if (prodCst.isValidInt)
            prod = prod * Cst(prodCst.toInt)
          else
            prod = prod * Prod(csts)
        }

        prod = prod * Prod(others)

        // variable simplification
        val varExpMap = scala.collection.mutable.Map[Expr, Sum]()
        vars.foreach(v => v match {
          case Var(_,_)|TypeVar(_) => varExpMap += (v-> (varExpMap.getOrElse(v, Sum(List(0))) + 1))
          case Pow(b:Var,e) => varExpMap += (v-> (varExpMap.getOrElse(b, Sum(List(0))) + e))
          case Pow(b:TypeVar,e) => varExpMap += (v-> (varExpMap.getOrElse(b, Sum(List(0))) + e))
        })

        // TODO: bug here, need to simplify further without entering into infinite recursion
        //var result = simplifyProd( prod * Prod(varExpMap.map({case (e, s) => simplifyPow(Pow(e,simplifySum(s)))}).toList))
        var result : Expr = prod * Prod(varExpMap.map({case (e, s) => simplifyPow(Pow(e,simplifySum(s)))}).toList)

        //result = prod * Prod(vars)


        // distributivity
        if (sums.nonEmpty) {
          if (result != Prod(List()))
            sums = sums :+ Sum(List(result))
          result = simplifySum(sums.reduce((s1, s2) => Sum(s1.terms.map(t1 => s2.terms.map(t2 => simplifyProd(t1 * t2))).flatten)))
        }


        // commutativity: reorder elements, because with integer sematics e.g.: 1/M * N != N * 1/M
        result = result match {
          case Prod(terms) => {
            // parition into all the pows and the rest ...
            val (pows, rest) = terms.partition( {
              case p: Pow => true
              case _ => false
            })
            // ... put the rest first and then the pows
            Prod(rest ++ pows)
          }
          case _ => result
        }

        result
      }




      // TODO: implement simplifcation of Var

      /*case Prod(terms) => {
        val baseExpMap = terms.foldLeft(new HashMap[Expr, Expr]())((map, e) => {
          e match {
            case Pow(b, e) => map + (b -> simplifySum(map.getOrElse(b, Cst(0)) + e))
            case _ => map + (e -> simplifySum(map.getOrElse(e, Cst(0)) + Cst(1)))
          }
        })
        var cst: Double = 0
        var result: Expr = Prod(List())
        baseExpMap.map({ case (base, exp) => {
          val t = simplifyPow(Pow(base, exp))
          t match {
            case Cst(c) => cst = cst * c
            case _ => result = result * t
          }
        }
        })

        if (cst != 0)
          if (cst.isValidInt)
            result = Cst(cst.toInt) * result // constants always first
          else
            throw new NotEvaluableException(result.toString)

        result*/
    }
  }

  def simplify(e: Expr): Expr = {

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
