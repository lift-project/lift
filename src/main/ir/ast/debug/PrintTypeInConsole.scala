package ir.ast.debug

/**
  * A pattern for debugging Lift code.
  * Identity function that prints the Lift type of its input.
  * Generates no OpenCL code.
  */
case class PrintTypeInConsole(msg: String = "") extends
  TypeOperator(actualType => {
    if (msg != "")
      print(msg + ": ")
    println(actualType.toString)
  })
