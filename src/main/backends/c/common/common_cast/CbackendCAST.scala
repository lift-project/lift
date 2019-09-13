package backends.c.common.common_cast

import core.generator.GenericAST.Block

object CbackendCAST {

  case class SourceFile(val path: String, val file: String, val cast: Block)

}
