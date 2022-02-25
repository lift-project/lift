package rewriting

import lift.arithmetic.{ArithExpr, Cst, ExtensibleVar, Range, RangeAdd, RangeUnknown, SimplifiedExpr, Var}
import rewriting.RewriteParamValues.PrintFormat
import rewriting.RewriteParamValues.PrintFormat.PrintFormat
import rewriting.RewriteParamWithOrdinalValues.EnumeratedVar

import scala.collection.immutable.{ListMap, ListSet}

sealed trait RewriteParamValues {
  val valueList: List[Option[_]]
  def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String]
}

object RewriteParamValues {
  def toStrings(params: Seq[RewriteParam[_]],
                values: List[Option[_]],
                printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] = printFormat match {
    case PrintFormat.RewritePoint =>
      params.zip(values).map { case (p, v) => "p." + p.getFullName.replace(".", "_") + " -> " +
        v.toString.replace("global", "GlobalMemory").
        replace("local", "LocalMemory").
        replace("private", "PrivateMemory") }.toList

//    case PrintFormat.RewritePoint =>
//      params.zip(values).map { case (p, v) =>
//        p.getFullName.replace(".", "_") + ".getAEVar -> " +
//          p.getFullName.replace(".", "_") + ".encode(" +
//          v.toString.replace("global", "GlobalMemory").
//            replace("local", "LocalMemory").
//            replace("private", "PrivateMemory") + ")" }.toList

    case PrintFormat.RewritePass =>
      params.zip(values).filter {
        case (_, v) if !v.toString.contains("Some(Map(f) => MapSeq(f))") => true
        case _ => true
      }.map { case (p, v) =>
        val dim: Option[Int] = if (v.toString.contains("(0)")) Some(0)
          else if (v.toString.contains("(1)")) Some(1)
          else if (v.toString.contains("(2)")) Some(2)
          else None

        "\"" + p.getFullName.slice(0, p.getFullName.lastIndexOf(".") + 1) +
          "\" -> " + (
          if (v.toString.contains("Some(Map(f) => MapWrg")) s"(10*Code.mapWrg() + Code.parDims(${dim.get})())"
          else if (v.toString.contains("Some(Map(f) => MapLcl")) s"(10*Code.mapLcl() + Code.parDims(${dim.get})())"
          else if (v.toString.contains("Some(Map(f) => MapGlb")) s"(10*Code.mapGlb() + Code.parDims(${dim.get})())"
          else if (v.toString.contains("Some(Map(f) => MapSeq")) s"(0)"
          else if (v.toString.contains("Some(AbstractMap1(AbstractMap2(f)) => Split(I) o AbstractMap1(f) o Join())"))
            "(Code.replaceInnerMapWithOuter())"
          else if (v.toString.contains("Some(Map(Map(f)) $ <slided 2d array> => Slide() o Map(f) o Unslide() $ <slided 2d array>)"))
            "1"
          else if (v.toString.contains("Some(Map0(Map1(Map2(Map3(f)))) $ <2d-slided 4d array> => Slide() o Map0(Slide() o Map2(f) o Unslide()) o Unslide() $ <2d-slided 4d array>)"))
            "2"
          else if (v.toString.contains("Some(MapSeq(uf) $ (arg: contiguous data) => MapSeqVector(uf.vectorize, uf, vectorLen) $ arg)"))
            "1"
          else if (v.toString.contains("None"))
            "0"
          else throw new IllegalArgumentException(v.toString)) }.toList
  }

  object PrintFormat extends Enumeration {
    type PrintFormat = Value
    val RewritePoint, /*RewritePoint, */RewritePass = Value
  }
}

/** Rewrite params
 * Parameters that affect AST and, consequently, parameter constraints of a lambda.
 * Need to be explored separately from the tuning parameters in a higher level-exploration
 * */
sealed abstract class RewriteParams(val paramList: Seq[RewriteParam[_]]) {

  type RewriteParamValuesT <: RewriteParamValues

  val collectionName: String

  def decode(vals: Option[List[Cst]]): RewriteParamValuesT

  def getCompulsoryVals(vals: Option[List[Cst]]): List[Cst] = vals match {
    case None => throw new IllegalArgumentException()
    case Some(someVals) => someVals
  }

  val paramMap: ListMap[String, RewriteParam[_]] = ListMap(paramList.map(p => p.name -> p): _*)

  def matchValues(values: RewriteParamValues): Boolean

  override def equals(obj: Any): Boolean = obj match {
    case other: RewriteParams => this eq other
    case _ => false
  }
}

object NoRewriteParams extends RewriteParams(paramList = Seq()) {

  val collectionName: String = "NoRewriteParams"

  case class NoRewriteParamValues() extends RewriteParamValues {
    override val valueList: List[Option[_]] = List()
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] = List()
  }
  type RewriteParamValuesT = NoRewriteParamValues

  override def decode(vals: Option[List[Cst]]): NoRewriteParamValues = NoRewriteParamValues()

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParamValuesT => true
    case _ => false
  }
}


abstract class RewriteParamsOfVarArity[T](val params: List[RewriteParam[T]]) extends RewriteParams(paramList = params) {

  case class RewriteParamValuesOfVarArity(vs: List[Option[T]]) extends RewriteParamValues {
    def unapply(arg: RewriteParamValuesOfVarArity): Option[List[Option[T]]] = Some(vs)
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(params, vs, printFormat)

    override val valueList: List[Option[_]] = vs
  }
  override type RewriteParamValuesT = RewriteParamValuesOfVarArity

  override def decode(vals: Option[List[Cst]]): RewriteParamValuesOfVarArity = RewriteParamValuesOfVarArity(
    params.zip(getCompulsoryVals(vals)).map { case (p, v) => p decode v })

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParamValuesT => true
    case _ => false
  }
}


abstract class RewriteParams1[T0](val p0: RewriteParam[T0]) extends RewriteParams(paramList = List(p0)) {

  case class RewriteParamValues1(v0: Option[T0]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0)
    def unapply(arg: RewriteParamValues1): Option[Option[T0]] = Some(v0)
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues1

  override def decode(vals: Option[List[Cst]]): RewriteParamValues1 = RewriteParamValues1(
    p0 decode getCompulsoryVals(vals).head)

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParamValuesT => true
    case _ => false
  }
}


abstract class RewriteParams2[T0, T1](val p0: RewriteParam[T0],
                                      val p1: RewriteParam[T1]) extends RewriteParams(paramList = List(p0, p1)) {

  case class RewriteParamValues2(v0: Option[T0], v1: Option[T1]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0, v1)
    def unapply(arg: RewriteParamValues2): Option[(Option[T0], Option[T1])] = Some((v0, v1))
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0, v1), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues2

  override def decode(vals: Option[List[Cst]]): RewriteParamValues2 = RewriteParamValues2(
    p0 decode getCompulsoryVals(vals).head,
    p1 decode getCompulsoryVals(vals)(1))

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParamValuesT => true
    case _ => false
  }
}


abstract class RewriteParams3[T0, T1, T2](val p0: RewriteParam[T0],
                                          val p1: RewriteParam[T1],
                                          val p2: RewriteParam[T2]) extends RewriteParams(paramList = List(p0, p1, p2)) {

  case class RewriteParamValues3(v0: Option[T0], v1: Option[T1], v2: Option[T2]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0, v1, v2)
    def unapply(arg: RewriteParamValues3): Option[(Option[T0], Option[T1], Option[T2])] = Some((v0, v1, v2))
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0, v1, v2), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues3

  override def decode(vals: Option[List[Cst]]): RewriteParamValues3 = RewriteParamValues3(
    p0 decode getCompulsoryVals(vals).head,
    p1 decode getCompulsoryVals(vals)(1),
    p2 decode getCompulsoryVals(vals)(2))

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParamValuesT => true
    case _ => false
  }
}


abstract class RewriteParams4[T0, T1, T2, T3](val p0: RewriteParam[T0],
                                              val p1: RewriteParam[T1],
                                              val p2: RewriteParam[T2],
                                              val p3: RewriteParam[T3]) extends RewriteParams(paramList = List(p0, p1, p2, p3)) {

  case class RewriteParamValues4(v0: Option[T0], v1: Option[T1], v2: Option[T2], v3: Option[T3]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0, v1, v2, v3)
    def unapply(arg: RewriteParamValues4): Option[(Option[T0], Option[T1], Option[T2], Option[T3])] = Some((v0, v1, v2, v3))
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0, v1, v2, v3), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues4

  override def decode(vals: Option[List[Cst]]): RewriteParamValues4 = RewriteParamValues4(
    p0 decode getCompulsoryVals(vals).head,
    p1 decode getCompulsoryVals(vals)(1),
    p2 decode getCompulsoryVals(vals)(2),
    p3 decode getCompulsoryVals(vals)(3))

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParams4[T0, T1, T2, T3] # RewriteParamValues4 => true
    case _ => false
  }
}


abstract class RewriteParams5[T0, T1, T2, T3, T4](val p0: RewriteParam[T0],
                                                  val p1: RewriteParam[T1],
                                                  val p2: RewriteParam[T2],
                                                  val p3: RewriteParam[T3],
                                                  val p4: RewriteParam[T4]) extends RewriteParams(paramList = List(p0, p1, p2, p3, p4)) {

  case class RewriteParamValues5(v0: Option[T0], v1: Option[T1], v2: Option[T2], v3: Option[T3], v4: Option[T4]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0, v1, v2, v3, v4)
    def unapply(arg: RewriteParamValues5): Option[(Option[T0], Option[T1], Option[T2], Option[T3], Option[T4])] = Some((v0, v1, v2, v3, v4))
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0, v1, v2, v3, v4), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues5

  override def decode(vals: Option[List[Cst]]): RewriteParamValues5 = RewriteParamValues5(
    p0 decode getCompulsoryVals(vals).head,
    p1 decode getCompulsoryVals(vals)(1),
    p2 decode getCompulsoryVals(vals)(2),
    p3 decode getCompulsoryVals(vals)(3),
    p4 decode getCompulsoryVals(vals)(4))

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParams5[T0, T1, T2, T3, T4] # RewriteParamValues5 => true
    case _ => false
  }
}


abstract class RewriteParams6[T0, T1, T2, T3, T4, T5](val p0: RewriteParam[T0],
                                                      val p1: RewriteParam[T1],
                                                      val p2: RewriteParam[T2],
                                                      val p3: RewriteParam[T3],
                                                      val p4: RewriteParam[T4],
                                                      val p5: RewriteParam[T5]) extends RewriteParams(paramList = List(p0, p1, p2, p3, p4, p5)) {

  case class RewriteParamValues6(v0: Option[T0], v1: Option[T1], v2: Option[T2], v3: Option[T3], v4: Option[T4], v5: Option[T5]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0, v1, v2, v3, v4, v5)
    def unapply(arg: RewriteParamValues6): Option[(Option[T0], Option[T1], Option[T2], Option[T3], Option[T4], Option[T5])] = Some((v0, v1, v2, v3, v4, v5))
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0, v1, v2, v3, v4, v5), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues6

  override def decode(vals: Option[List[Cst]]): RewriteParamValues6 = RewriteParamValues6(
    p0 decode getCompulsoryVals(vals).head,
    p1 decode getCompulsoryVals(vals)(1),
    p2 decode getCompulsoryVals(vals)(2),
    p3 decode getCompulsoryVals(vals)(3),
    p4 decode getCompulsoryVals(vals)(4),
    p5 decode getCompulsoryVals(vals)(5))

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParams6[T0, T1, T2, T3, T4, T5] # RewriteParamValues6 => true
    case _ => false
  }
}


abstract class RewriteParams7[T0, T1, T2, T3, T4, T5, T6](val p0: RewriteParam[T0],
                                                          val p1: RewriteParam[T1],
                                                          val p2: RewriteParam[T2],
                                                          val p3: RewriteParam[T3],
                                                          val p4: RewriteParam[T4],
                                                          val p5: RewriteParam[T5],
                                                          val p6: RewriteParam[T6]) extends RewriteParams(paramList = List(p0, p1, p2, p3, p4, p5, p6)) {

  case class RewriteParamValues7(v0: Option[T0], v1: Option[T1], v2: Option[T2], v3: Option[T3], v4: Option[T4], v5: Option[T5], v6: Option[T6]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0, v1, v2, v3, v4, v5, v6)
    def unapply(arg: RewriteParamValues7): Option[(Option[T0], Option[T1], Option[T2], Option[T3], Option[T4], Option[T5], Option[T6])] = Some((v0, v1, v2, v3, v4, v5, v6))
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0, v1, v2, v3, v4, v5, v6), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues7

  override def decode(vals: Option[List[Cst]]): RewriteParamValues7 = RewriteParamValues7(
    p0 decode getCompulsoryVals(vals).head,
    p1 decode getCompulsoryVals(vals)(1),
    p2 decode getCompulsoryVals(vals)(2),
    p3 decode getCompulsoryVals(vals)(3),
    p4 decode getCompulsoryVals(vals)(4),
    p5 decode getCompulsoryVals(vals)(5),
    p6 decode getCompulsoryVals(vals)(6))

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParams7[T0, T1, T2, T3, T4, T5, T6] # RewriteParamValues7 => true
    case _ => false
  }
}


abstract class RewriteParams8[T0, T1, T2, T3, T4, T5, T6, T7](val p0: RewriteParam[T0],
                                                              val p1: RewriteParam[T1],
                                                              val p2: RewriteParam[T2],
                                                              val p3: RewriteParam[T3],
                                                              val p4: RewriteParam[T4],
                                                              val p5: RewriteParam[T5],
                                                              val p6: RewriteParam[T6],
                                                              val p7: RewriteParam[T7]) extends RewriteParams(paramList = List(p0, p1, p2, p3, p4, p5, p6, p7)) {

  case class RewriteParamValues8(v0: Option[T0], v1: Option[T1], v2: Option[T2], v3: Option[T3],
                                 v4: Option[T4], v5: Option[T5], v6: Option[T6], v7: Option[T7]) extends RewriteParamValues {
    override val valueList: List[Option[_]] = List(v0, v1, v2, v3, v4, v5, v6, v7)
    def unapply(arg: RewriteParamValues8): Option[
      (Option[T0], Option[T1], Option[T2], Option[T3],
        Option[T4], Option[T5], Option[T6], Option[T7])] = Some((v0, v1, v2, v3, v4, v5, v6, v7))
    def toStrings(printFormat: PrintFormat = PrintFormat.RewritePoint): List[String] =
      RewriteParamValues.toStrings(paramList, List(v0, v1, v2, v3, v4, v5, v6, v7), printFormat)
  }
  override type RewriteParamValuesT = RewriteParamValues8

  override def decode(vals: Option[List[Cst]]): RewriteParamValues8 = RewriteParamValues8(
    p0 decode getCompulsoryVals(vals).head,
    p1 decode getCompulsoryVals(vals)(1),
    p2 decode getCompulsoryVals(vals)(2),
    p3 decode getCompulsoryVals(vals)(3),
    p4 decode getCompulsoryVals(vals)(4),
    p5 decode getCompulsoryVals(vals)(5),
    p6 decode getCompulsoryVals(vals)(6),
    p7 decode getCompulsoryVals(vals)(7))

  def matchValues(values: RewriteParamValues): Boolean = values match {
    case _: RewriteParams8[T0, T1, T2, T3, T4, T5, T6, T7] # RewriteParamValues8 => true
    case _ => false
  }
}

/**
 * RewriteParam represents values of arbitrary type as Csts in range [0, <max number of values>)
 */
abstract class RewriteParam[T](val name: String,
                               val enumValues: List[Option[T]]) {
  // A full name is one that includes the name of the rewriting strategy instance which defines this
  // rewrite param, as well as names of strategies that generate this instance of rewriting strategy.
  // Full name is to be used for this rewrite param's ArithExpr var (aeVar) so that it is different from the
  // names of aeVars of other instances of this rewrite param that belong to rewrite strategy instances
  // generated by other sources.
  private var fullName: Option[String] = None

  def setFullName(newName: String): Unit = {
    fullName match {
      case Some(existName) if !existName.equals(newName) => throw new IllegalAccessError("Cannot change rewrite param full name " +
        "after it already has been set!")
      case Some(_) => // for the same new name, do nothing
      case None => fullName = Some(newName)
    }
  }

  def getFullName: String = fullName match {
    case None => throw new IllegalAccessError("The full name of this rewrite param hasn't been set yet!")
    case Some(someFullName) => someFullName
  }

  protected var aeVar: Option[Var] = None
  def getAEVar: Var

  def encode(value: Option[_]): Cst
  def decode(encodedValue: Cst): Option[T]

  override def equals(obj: Any): Boolean = obj match {
    case other: RewriteParam[_] => this eq other
    case _ => false
  }

  override def toString: String = fullName.getOrElse(name)
}

/**
 * Rewrite param with a set of enumerated values represented by a continuous range of integers.
 * The encoded values -- the continuous range of integers -- are categorical.
 */
case class RewriteParamWithCategoricalValues[T](override val name: String,
                                                override val enumValues: List[Option[T]])
  extends RewriteParam[T](name, enumValues) {

  def getAEVar: Var = {
    aeVar match {
      case Some(initializedVar) => initializedVar
      case None =>
        try {
          val fullName = getFullName
          aeVar = Some(Var(fullName, RangeAdd(0, enumValues.size, step = 1)))
        } catch {
          case e: IllegalAccessError =>
            throw new IllegalAccessError("Cannot get aeVar: " + e.getMessage)
        }
        aeVar.get
    }
  }

  def encode(value: Option[_]): Cst = {
    if (!enumValues.contains(value))
      throw new IllegalArgumentException()
    Cst(enumValues.indexOf(value))
  }
  def decode(encodedValue: Cst): Option[T] = {
    assert(encodedValue.c.toInt < enumValues.length)
    enumValues(encodedValue.c.toInt)
  }
}

object RewriteParamOptional {
  def apply[T](name: String, enumRange: List[T]): RewriteParam[T] = new RewriteParamWithCategoricalValues[T](name, None +: enumRange.map(Some(_)))
}

object RewriteParamCompulsory {
  def apply[T](name: String, enumRange: List[T]): RewriteParam[T] = new RewriteParamWithCategoricalValues[T](name, enumRange.map(Some(_)))
}

/**
 * Rewrite param with a set of enumerated values represented by a given discrete set of integers (enumValues).
 * Useful when the integer encoding is ordinal, e.g. for the sake of imposing constraints.
 *
 * @param orderedEnumValues A map of integers and the rewrite param values they represent
 */
case class RewriteParamWithOrdinalValues[T](override val name: String,
                                            orderedEnumValues: ListMap[Int, Option[T]])
  extends RewriteParam[T](name, orderedEnumValues.values.toList) {
  def getAEVar: Var = {
    aeVar match {
      case Some(initializedVar) => initializedVar
      case None =>
        try {
          val fullName = getFullName
          aeVar = Some(EnumeratedVar(fullName, enumValues = ListSet(orderedEnumValues.keys.toSeq: _*)))
        } catch {
          case e: IllegalAccessError =>
            throw new IllegalAccessError("Cannot get aeVar: " + e.getMessage)
        }
        aeVar.get
    }
  }

  def encode(value: Option[_]): Cst = {
    orderedEnumValues.find(_._2 == value) match {
      case Some((k, _)) => Cst(k)
      case None => throw new IllegalArgumentException()
    }
  }

  def decode(encodedValue: Cst): Option[T] = {

    if (!orderedEnumValues.contains(encodedValue.c.toInt))
      throw new IllegalArgumentException(s"Rewrite param $name cannot have a value $encodedValue. Allowed values: $orderedEnumValues")

    orderedEnumValues(encodedValue.c.toInt)
  }
}

object RewriteParamWithOrdinalValues {

  /**
   * A variable with enumerated domain -- this is to avoid extending ArithExpr with discrete ranges.
   * Used in ChocoSolver to represent some rewrite params only. Should not appear in AST.
   * There is no good reason for this to exist except for backward compatibility with ChocoSolver that
   * operates on ArithExpr variables. A better approach would be to extend ChocoSolver to not depend on ArithExpr vars.
   */
  case class EnumeratedVar(override val name: String,
                           override val fixedId: Option[Long] = None,
                           enumValues: ListSet[Int]) extends ExtensibleVar(name, RangeUnknown, fixedId) {
    override def copy(e: Range): EnumeratedVar = EnumeratedVar(name, Some(id), enumValues)

    override def cloneSimplified() = new EnumeratedVar(name, Some(id), enumValues) with SimplifiedExpr

    override def visitAndRebuild(f: (ArithExpr) => ArithExpr): ArithExpr = f(EnumeratedVar(name, Some(id), enumValues))

    override lazy val toString: String = "enum_v_" + name + "_" + id
  }
}