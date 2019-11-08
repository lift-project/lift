package backends.spatial

import scala.util.matching.Regex

package object generator {
  /**
   * Cleans the Spatial program by replacing the numeric in variable names with letters
   * in increasing alphabetic order. The letters are assigned to variables in their order
   * of occurrence in the program, thus replacing the ordering of the original numeric ids, which
   * is affected by the internal mechanics of the compiler. Example:
   * { val v_someName_37 = 1
   *   v_someName_37 + v__2
   * } is replaced with
   * { val v_someName_b = 1
   *   v_someName_b + v__a
   * }
   * TODO: clean duplicate function names (e.g. id_0, id_1)
   */
  def cleanSpatialProgram(code: String): String = {
    val regexVarNames = new Regex("""(v_(.*?)_[0-9]+(__[0-9]+(_[0-9]+)*)*)""", "varFullName", "varCoreName")
    val varSubstitutions = regexVarNames.
      findAllMatchIn(code).
      map(m => (m.group("varFullName"), m.group("varCoreName"))).
      toList.distinct.
      zipWithIndex.map {
      case ((varFullName: String, varCoreName: String), orderId: Int) =>
        if (!varCoreName.isEmpty) (varFullName, "v_" + varCoreName + "_"  + (97+orderId).toChar)
        else (varFullName, "v__" + (97+orderId).toChar)
    }
    val updatedCode = varSubstitutions.foldLeft(code) {
      case (partUpdatedCode, varSubst) => partUpdatedCode.replaceAllLiterally(varSubst._1, varSubst._2)
    }
    updatedCode
  }
}
