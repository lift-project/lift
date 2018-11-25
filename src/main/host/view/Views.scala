package host.view


import ir.ast.{Lambda}

object Views {

  def apply(lambda: Lambda): Unit = {

    InputView(lambda)

    OutputView(lambda)

  }

}

