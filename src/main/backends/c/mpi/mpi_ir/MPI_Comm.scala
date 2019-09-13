package backends.c.mpi.mpi_ir


import core.generator.GenericAST.{AstNode, RawCode}

class MPI_Comm {
  var astItem: AstNode = RawCode("MPI_COMM_NULL")
  var parent: Option[MPI_Comm] = None
}

object MPI_Comm{
  def apply(astItem: AstNode, parent: Option[MPI_Comm]): MPI_Comm = {
    val comm = new MPI_Comm
    comm.astItem = astItem
    comm.parent = parent
    comm
  }
}

case object MPI_Comm_world extends MPI_Comm
{
  astItem = RawCode("MPI_COMM_WORLD")
  parent = None
}

case object MPI_Comm_self extends MPI_Comm {
  astItem = RawCode("MPI_COMM_SELF")
  parent = None
}

case object MPI_Comm_null extends MPI_Comm
{
  astItem = RawCode("MPI_COMM_NULL")
  parent = None
}
