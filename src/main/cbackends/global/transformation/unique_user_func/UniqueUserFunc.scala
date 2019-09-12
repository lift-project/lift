package cbackends.global.transformation.unique_user_func

import core.generator.GenericAST.{Block, FunctionPure}

import scala.collection.mutable

object UniqueUserFunc {

  def apply(list_blocks: List[Block]) : List[Block] = {

    val generated_user_func = mutable.Set.empty[String]

    list_blocks.map( b => {

      val uniqued = b.visitAndRebuild {
        case fp:FunctionPure => {
          generated_user_func.contains(fp.name) match {
            case true =>
              Block(global = true)
            case false =>
              generated_user_func += fp.name
              fp
          }
        }
        case x => x
      }

      uniqued.asInstanceOf[Block]
    }

    )

  }

}
