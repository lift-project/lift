package generic.ast

import scala.language.implicitConversions

/*
A pretty printer vaugley based on Wadler's "A prettier printer" paper. Users
construct instances of a "document" ast, which is then (somewhat) efficiently
serialised into a string in the "layout" function. This implementation is
somewhat less functional than wadler's original, as it doesn't take into account
line lengths, but for our purposes this is sufficient: we generally lay out
lines ourselves, and don't care how long they get.
  */
object PrettyPrinter {

  /*
    The"interface" algebraic data type which users construct
    instances of from their own trees.
   */
  sealed abstract class DOC {
    // concatenation, with some optimisations for when we don't need extra
    // objects
    def <>(that: DOC): DOC = (this, that) match {
      case (NIL(), NIL())   ⇒ NIL()
      case (NIL(), TEXT(s)) ⇒ TEXT(s)
      case (TEXT(s), NIL()) ⇒ TEXT(s)
//      case (TEXT(s1), TEXT(s2)) ⇒ TEXT(s1 ++ s2)
      case _                    ⇒ Concat(this, that)
    }

    // defining more interesting utility concatenations as operators
    def <+>(that: DOC): DOC = this <> text(" ") <> that

    def </>(that: DOC): DOC = this <> line <> that

  }

  case class NIL() extends DOC

  case class Concat(a: DOC, b: DOC) extends DOC

  case class NEST(level: Int, d: DOC) extends DOC

  case class TEXT(s: String) extends DOC

  case class LINE() extends DOC

  /*
  Layout methods defined on a doc. These methods take a doc, and return a
  string of the doc "serialised" into a single block of text (a string), with
   '\n' defining newlines instead of algebraic data types.
   */
  // slow, purely functional layout implementation
  def layout(d: DOC): String = _layout(d, 0)
  private def _layout(d: DOC, n: Int): String = d match {
    case NIL()         ⇒ ""
    case Concat(a, b)  ⇒ _layout(a, n) ++ _layout(b, n)
    case NEST(i, _d)   ⇒ _layout(_d, n + i)
    case TEXT(s)       ⇒ s
    case LINE()        ⇒ "\n" ++ ("  " * n)
  }

  // faster, stateful layout implementation
  def layoutS(d: DOC) : String = {
    val sb = new StringBuilder()
    _layoutStateful(d, 0, sb)
    sb.toString()
  }
  private def _layoutStateful(d: DOC, n: Int, sb: StringBuilder) : Unit = d
  match {
    case NIL()         ⇒
    case Concat(a, b)  ⇒ {
      _layoutStateful(a, n, sb)
      _layoutStateful(b, n, sb)
    }
    case NEST(i, _d)   ⇒ _layoutStateful(_d, n + i, sb)
    case TEXT(s)       ⇒ sb ++= s
    case LINE()        ⇒ {
      sb ++= "\n"
      sb ++= "  " * n
    }
  }

  /*
    Some handy definitions and utility functions that make writing code
    cleaner and easier.
   */
  def nil = NIL()

  def nest(i: Int, x: DOC): DOC = NEST(i, x)

  def text(s: String): DOC = TEXT(s)

  def line: DOC = LINE()

  // an implicit function to let us construct documents from trees
  implicit def stringToDoc(s: String) = text(s)

  // concatenation with a space
  def <+>(a: DOC, b: DOC): DOC = a <> text(" ") <> b

  // concatenation with a newline
  def </>(a: DOC, b: DOC): DOC = a <> line <> b

  /*
  Various interspersal methods defined in terms of a "fold" across a list of
  documents
   */
  private def folddoc(f: (DOC, DOC) ⇒ DOC, ds: List[DOC]): DOC = ds.tail
    .foldLeft(ds
    .head
    : DOC
  )(f)

  // generic intersperse, with a given separator
  def intersperse(ds: List[DOC], sep: DOC = text(",")) =
    folddoc((a: DOC, b: DOC) ⇒ a <> sep <> b, ds)

  // spread documents out horizontally with spaces
  def spread(ds: List[DOC]) = folddoc(<+>, ds)

  // spread documents out vertically with lines
  def stack(ds: List[DOC]) = folddoc(</>, ds)

  // print a document surrounded in brackets, and nested
  def bracket(l: String, x: DOC, r: String): DOC =
    (text(l) <> nest(2, line <> x) <> line <> text(r))

}