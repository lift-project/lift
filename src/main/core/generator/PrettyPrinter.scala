package core.generator

import scala.collection.mutable.ArrayBuffer
import scala.language.implicitConversions

/*
A pretty printer vaguely based on Wadler's "A prettier printer" paper. Users
construct instances of a "document" ast, which is then (somewhat) efficiently
serialised into a string in the "layout" function. This implementation is
somewhat less functional than wadler's original, as it doesn't take into account
line lengths, but for our purposes this is sufficient: we generally lay out
lines ourselves, and don't care how long they get.
  */
object PrettyPrinter {
  /*
    The "interface" algebraic data type which users construct
    instances of from their own trees.
   */
  sealed abstract class Doc {
    // concatenation, with some optimisations for when we don't need extra
    // objects

    // side by side concatenation
    def <>(that: Doc): Doc = (this, that) match {
      case (Empty(), Empty())           ⇒ Empty()
      case (Empty(), Text(s))           ⇒ Text(s)
      case (Text(s), Empty())           ⇒ Text(s)
      case (Text(s1), Text(s2))         ⇒ Text(s1 + s2)
      case (Concat(ds1), Concat(ds2))   ⇒ Concat(ds1 ++ ds2)
      case (Concat(ds1), _)             ⇒ Concat(ds1 :+ that)
      case (_, Concat(ds2))             ⇒ Concat(this +: ds2)
      case _                            ⇒ Concat(ArrayBuffer(this, that))
    }

    // defining more interesting utility concatenations as operators
    // concatenation with a space
    def <+>(that: Doc): Doc = this <> text(" ") <> that

    // vertical concatenation with a line
    def </>(that: Doc): Doc = this <> line <> that

  }

  case class Empty() extends Doc

  case class Concat(ds: ArrayBuffer[Doc]) extends Doc

  case class Nest(level: Int, d: Doc) extends Doc

  case class Text(s: String) extends Doc

  case class Line() extends Doc

  /*
  Layout methods defined on a doc. These methods take a doc, and return a
  string of the doc "serialised" into a single block of text (a string), with
   '\n' defining newlines instead of algebraic data types.
   */
  // slow, purely functional layout implementation
  def layout(d: Doc): String = _layout(d, 0)
  private def _layout(d: Doc, n: Int): String = d match {
    case Empty()      ⇒ ""
    case Concat(ds)   ⇒ ds.tail.foldLeft(_layout(ds.head, n))((acc, next) => acc ++ _layout(next, n))
    case Nest(i, _d)  ⇒ _layout(_d, n + i)
    case Text(s)      ⇒ s
    case Line()       ⇒ "\n" ++ ("  " * n)
  }

  // faster, stateful layout implementation
  def layoutS(d: Doc) : String = {
    val sb = new StringBuilder()
    _layoutStateful(d, 0, sb)
    sb.toString()
  }
  private def _layoutStateful(d: Doc, n: Int, sb: StringBuilder) : Unit = d
  match {
    case Empty()      ⇒
    case Concat(ds)   ⇒ ds.foreach(_layoutStateful(_, n, sb))
    case Nest(i, _d)  ⇒ _layoutStateful(_d, n + i, sb)
    case Text(s)      ⇒ sb ++= s
    case Line()       ⇒ {
      sb ++= "\n"
      sb ++= "  " * n
    }
  }



  /*
    Some handy definitions and utility functions that make writing code
    cleaner and easier.
   */
  val empty = Empty()

  def nest(i: Int, x: Doc): Doc = Nest(i, x)

  def text(s: String): Doc = Text(s)

  def line: Doc = Line()

  // an implicit function to let us construct documents from trees
  implicit def stringToDoc(s: String) = text(s)

  // concatenation with a space
  def <+>(a: Doc, b: Doc): Doc = a <> text(" ") <> b

  // concatenation with a newline
  def </>(a: Doc, b: Doc): Doc = a <> line <> b

  /*
  Various interspersal methods defined in terms of a "fold" across a list of
  documents
   */
  private def folddoc(f: (Doc, Doc) ⇒ Doc, ds: List[Doc]): Doc = ds match {
    case Nil ⇒ Empty()
    case (x :: Nil) ⇒ x
    case (x :: xs) ⇒ xs.foldLeft(x)(f)
  }

  // generic intersperse, with a given separator
  def intersperse(ds: List[Doc], sep: Doc = text(", ")) =
    folddoc((a: Doc, b: Doc) ⇒ a <> sep <> b, ds)

  // spread documents out horizontally with spaces
  def spread(ds: List[Doc]) = folddoc(<+>, ds)

  // spread documents out vertically with lines
  def stack(ds: List[Doc]) = folddoc(</>, ds)

  // print a document surrounded in brackets, and nested
  def bracket(l: String, x: Doc, r: String): Doc =
    (text(l) <> nest(2, line <> x) <> line <> text(r))

}