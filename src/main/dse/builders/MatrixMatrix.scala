package dse.builders


import dse.Explorer
import opencl.ir._
import rewriting.Rewrite
import opencl.executor._
import ir._
import ir.ast._

object MatrixMatrix {


  // TODO: Could use lift files instead...
  def generateMatrixMatrix( K : Int,
                            N : Int,
                            M : Int): Lambda = {

    val f = fun(
      ArrayTypeWSWC(ArrayTypeWSWC(Float, K), M),
      ArrayTypeWSWC(ArrayTypeWSWC(Float, N), K),
      (A, B) => {
        Map(fun(aRow =>
          Map(fun(bCol =>
            Reduce(add, 0.0f) o Map(fun(x => mult(Get(x, 0), Get(x, 1)))) $ Zip(aRow, bCol)
          )) o Transpose() $ B
        )) $ A
      })

    TypeChecker.check(f.body)
    f
  }

  def main(args: Array[String]): Unit = {

    /***
     * initialization and arguments...
     ***/

    // Arguments check Init

    assert(args.size == 4)


    // Get arguments and prepare variables

    val rules = Explorer.defaultRules


    val K = args(0).toInt
    val N = args(1).toInt
    val M = args(2).toInt
    val parameters = args(3).toString.split(",").map {
      _.split(":").map{
        _.toInt
      }
    }

    for( index <- 0 to (parameters.size - 1)){

      val parameter = parameters(index)
      val rule_index    = parameter(0)
      val rule_position = parameter(1)
      assert(parameter.size == 2)

      if (rules.size <= rule_index) {
        printf ("defaultRules.size = %d\n" , rules.size)
        printf ("rule_index  = %d\n" , rule_index)
        throw new IllegalArgumentException("Wrong argument: defaultRules.size <= rule_index")
      }

    }


    // OpenCL Init
    Executor.loadAndInit()


    /***
      * Print infos
      ***/

    for( index <- 0 to (rules.size - 1)) {
      val rule = rules(index)

      printf("Stored Rule %d : %s\n", index , rule)
    }

    printf("\n")

    for( index <- 0 to (parameters.size - 1)) {
      val parameter = parameters(index)

      val rule_index    = parameter(0)
      val rule_position = parameter(1)

      printf("Selected Rule %d : %d(%s) at %d\n", index , rule_index, rules(rule_index) , rule_position)
    }

    printf("\n")


    /***
      * Start transformation
      ***/


    var f = List(generateMatrixMatrix(K, N, M))

    for( index <- 0 to (parameters.size - 1)) {

      // Pick the rule
      //**************************

      val parameter     = parameters(index)
      val rule_index    = parameter(0)
      val rule_position = parameter(1)

      val selected_rule = rules(rule_index)
      val allRulesAt = Rewrite.listAllPossibleRewritesForRules(f(0), List(selected_rule))

      printf ("Can apply %s in %d slot(s), picked %d.\n",  selected_rule,allRulesAt.size, rule_position)

      if (allRulesAt.size == 0) { // TODO: Could continue anyway...
      	 printf ("There is no way to go.\n")
         printf ("DEPTH=%d\n" , index)
        return
      }

      val point_index = (rule_position % allRulesAt.size)


      // Apply the rule
      //**************************

      try {
        val temporary = Explorer.applyRule(f(0), selected_rule, point_index, rules)
        TypeChecker(temporary)
        f = temporary :: f
      } catch {
        case _: Throwable => printf("RULE FAILED\n")
      }

    }


    // Now all the rules are applied, try compiling it.
    //**************************************************

    try {
      Compile(f(0))
    } catch {
	    case _: Throwable =>	  printf ("DEPTH=%d\n" , 0)
    }


    // And running it.
    //**************************************************

    try {

      val matrixA = Array.tabulate(M, K)((r, c) => (((r * 3 + c * 2) % 10) + 1) * 1.0f)
      val matrixB = Array.tabulate(K, N)((r, c) => (((r * 7 + c * 3) % 10) + 1) * 1.0f)
      val (output, runtime) = Execute(1, 1, 1, N, M, 1, (true, true))[Array[Float]](Compile(f(0)), f(0), matrixA.transpose, matrixB)

      print (output)

      printf ("\n")
      printf ("DEPTH=%d\n" , 99)
      printf ("DURATION=%f\n", runtime)

    } catch {
      case r : Throwable =>  printf ("%s\nDEPTH=%d\n" , r, 0)

    }

	 
    Executor.shutdown()

  }

}
