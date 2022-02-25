package ir.ast

import java.util.concurrent.atomic.AtomicInteger

object IDGenerator {
  private var id_counter = new AtomicInteger(0)
  def get_id(): Int = id_counter.incrementAndGet()
  def reset_id(): Unit = {
    id_counter = new AtomicInteger(0)
  }
}

class IgnoreChildrenExceptionUnit() extends Exception
class IgnoreChildrenException(val result: IRNode) extends Exception

trait IRNode {
  final var gid = IDGenerator.get_id()

  final def visitBy(pre: IRNode => Unit = { n => }, post: IRNode => Unit = { n => }) : Unit = {
    this.visit_pp((n:IRNode) => {
      try {
        pre(n)
      } catch {
        case iceu: IgnoreChildrenExceptionUnit =>
          post(n)
          throw iceu
      }

      val f : IRNode => Unit = {(_:IRNode) => post(n)}
      f
    } )
  }

  def _visit(prePost: IRNode => IRNode => Unit) : Unit = ()

  final def visit_pp(prePost: IRNode => IRNode => Unit) : Unit = {
    try {
      val post: IRNode => Unit = prePost(this)
      this._visit(prePost)
      post(this)
    } catch {
      case _ : IgnoreChildrenExceptionUnit => ()
    }
  }

  final def visitAndRebuild(pre: IRNode => IRNode = {n => n}, post: IRNode => IRNode = {n =>n} ) : IRNode =
    post( try {pre(this)._visitAndRebuild(pre,post)}
    catch {
      case ice : IgnoreChildrenException => ice.result // skipping the children and returning result of pre stored in the exception
    }
    )

  def _visitAndRebuild(pre: IRNode => IRNode, post: IRNode => IRNode) : IRNode = this


}

