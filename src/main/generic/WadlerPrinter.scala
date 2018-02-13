package generic.ast

import scala.language.implicitConversions

/**
  * For an explanation of how this printer works, I'd suggest reading the
  * paper "A Prettier Printer" (Wadler, 1997). The comments herin will give
  * some guidelines, but the paper gives a full explanation.
  */
object WadlerPrinter {

  /*
    The"interface" algebraic data type which users construct
    instances of from their own trees.
   */
  sealed abstract class DOC {
    // Concatenation
    //    def :<>(that: DOC): Concat = Concat(this, that)

    // concatenation, with some optimisations
    def <>(that: DOC): DOC = (this, that) match {
      case (NIL(), NIL())   ⇒ NIL()
      case (NIL(), TEXT(s)) ⇒ TEXT(s)
      case (TEXT(s), NIL()) ⇒ TEXT(s)
      //      case (TEXT(s), LINE()) ⇒ TEXT(s ++ "\n")
      //      case (LINE(), TEXT(s)) ⇒ TEXT("\n"++s)
      case (TEXT(s1), TEXT(s2)) ⇒ TEXT(s1 ++ s2)
      case _                    ⇒ Concat(this, that)
    }

    //    def <>(that: String) : DOC = this <> TEXT(that)

    // defining more interesting utility concatenations as operators
    def <+>(that: DOC): DOC = this <> text(" ") <> that

    def </>(that: DOC): DOC = this <> line <> that

    // Another kind of concatenation
    def :<|>(that: DOC): ConcatP = ConcatP(this, that)


  }

  case class NIL() extends DOC

  case class Concat(a: DOC, b: DOC) extends DOC

  case class NEST(level: Int, d: DOC) extends DOC

  case class TEXT(s: String) extends DOC

  case class LINE() extends DOC

  case class ConcatP(a: DOC, b: DOC) extends DOC

  /*
    Provide some handy definitions that make writing interface code cleaner
   */
  def nil = NIL()

  def nest(i: Int, x: DOC): DOC = NEST(i, x)

  def text(s: String): DOC = TEXT(s)

  def line: DOC = LINE()

  implicit def stringToDoc(s: String) = text(s)

  /*
    More handy character definitions
   */
  def << = text("(")

  def >> = text(")")


  /*
    The "backend" algebraic data type which is constructed, and pretty printed
    from the original "interface" algebraic data type.
   */
  sealed abstract class Doc {}

  case class DNil() extends Doc

  case class Text(s: String, d: Doc) extends Doc

  case class Line(n: Int, d: Doc) extends Doc


  /*
    Flattening constructs
   */
  def group(x: DOC): DOC = flatten(x :<|> x)

  def flatten(d: DOC): DOC = d match {
    case NIL()         ⇒ NIL()
    case Concat(a, b)  ⇒ flatten(a) <> flatten(b)
    case NEST(i, d)    ⇒ NEST(i, flatten(d))
    case TEXT(s)       ⇒ TEXT(s)
    case LINE()        ⇒ TEXT("\n")
    case ConcatP(a, b) ⇒ flatten(a)
  }

  /*
    Layout functionality
   */
  def layout(d: Doc): String = d match {
    case DNil()     ⇒ ""
    case Text(s, x) ⇒ s ++ layout(x)
    case Line(i, x) ⇒ "\n" ++ ("  " * i) ++ layout(x)
  }

  def layout(d: DOC, n: Int): String = d match {
    case NIL()         ⇒ ""
    case Concat(a, b)  ⇒ layout(a, n) ++ layout(b, n)
    case NEST(i, _d)   ⇒ layout(_d, n + i)
    case TEXT(s)       ⇒ s
    case LINE()        ⇒ "\n" ++ ("  " * n)
    case ConcatP(a, b) ⇒ layout(a, n) ++ layout(b, n)
  }

  def best(w: Int, k: Int, x: DOC) = be(w, k, List((0, x)))

  def be(w: Int, k: Int, d: List[(Int, DOC)]): Doc = d match {
    case Nil                       ⇒ DNil()
    case ((i, NIL()) :: z)         ⇒ be(w, k, z)
    case ((i, Concat(x, y)) :: z)  ⇒ be(w, k, ((i, x) :: (i, y) :: z))
    case ((i, NEST(j, x)) :: z)    ⇒ be(w, k, (i + j, x) :: z)
    case ((i, TEXT(s)) :: z)       ⇒ Text(s, be(w, k + s.length(), z))
    case ((i, LINE()) :: z)        ⇒ Line(i, be(w, i, z))
    case ((i, ConcatP(x, y)) :: z) ⇒ better(w, k,
      be(w, k, ((i, x) :: z)),
      be(w, k, ((i, y) :: z)))
  }

  def better(w: Int, k: Int, x: Doc, y: Doc): Doc = {
    if (fits((w - k), x)) {
      x
    } else {
      y
    }
  }

  def fits(w: Int, d: Doc): Boolean = (d, w < 0) match {
    case (_, true)       ⇒ false
    case (DNil(), _)     ⇒ true
    case (Text(s, x), _) ⇒ fits((w - s.length()), x)
    case (Line(i, x), _) ⇒ true
  }

  /*
    The code function that we use to actually turn a doc into a string
   */
  def pretty(width: Int, x: DOC): String = layout(best(width, 0, x))

  /*
    Utility functions
   */
  def <+>(a: DOC, b: DOC): DOC = a <> text(" ") <> b

  def </>(a: DOC, b: DOC): DOC = a <> line <> b


  def folddoc(f: (DOC, DOC) ⇒ DOC, ds: List[DOC]): DOC = ds.tail.foldLeft(ds
    .head
    : DOC
  )(f)

  def spread(ds: List[DOC]) = folddoc(<+>, ds)

  def stack(ds: List[DOC]) = folddoc(</>, ds)

  def intersperse(ds: List[DOC], sep: DOC = text(",")) =
    folddoc((a: DOC, b: DOC) ⇒ a <> sep <> b, ds)

  // note, this is not quite wadlers definition - we don't group, as
  // sometimes we want to handle the flattening ourselves!
  def bracket(l: String, x: DOC, r: String): DOC =
    (text(l) <> nest(2, line <> x) <> line <> text(r))

  //  group(text(l) <> nest(2, line <> x)) <> line <> text(r))

  def <+/>(a: DOC, b: DOC): DOC = a <> (text(" ") :<|> line) <> b

  // Don't bother to provide this definition.
  //  def fillwords(ds: List[DOC]) : DOC = ...


}