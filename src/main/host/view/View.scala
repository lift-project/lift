package host.view


import ir.ast.{Lambda}

object View {

  def apply(lambda: Lambda): Unit = {

    InputView(lambda)

    OutputView(lambda)

  }

}

