package lift
package profiler

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context

class Profile(extraContext: String = "") extends StaticAnnotation {
  def macroTransform(annottees: Any*) = macro Profile.impl
}

object Profile {
  def impl(c: Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    val ctx: String = c.prefix.tree match {
      case q"new Profile($extraContext)" => c.eval[String](c.Expr(extraContext))
    }

    val result = {
      annottees.map(_.tree).toList match {
        case q"$mods def $methodName[..$tpes](...$args): $returnType = { ..$body }" :: Nil => {
          q"""$mods def $methodName[..$tpes](...$args): $returnType =  {
            val start = System.nanoTime()
            val profSpliceResultValueNoConflict = {..$body}
            val end = System.nanoTime()
            println("PROFILING_DATUM: (\""+${methodName.toString}+"\", \"" + ${ctx.toString} + "\", " + (end-start).toDouble/1000000 + ", \"Scala\")")
            profSpliceResultValueNoConflict
          }"""
        }
        case _ => c.abort(c.enclosingPosition, "Annotation @Profile can be used only with methods")
      }
    }
    c.Expr[Any](result)
  }

  def profile[T](name: String, context: String, f: () => T) : T = {
    val start = System.nanoTime()
    val r: T = f()
    val end = System.nanoTime()
    println("PROFILING_DATUM: (\"" + name + "\", \"" + context + "\"," + (end-start).toDouble/1000000 + ", \"Scala\")")
    r
  }
}
