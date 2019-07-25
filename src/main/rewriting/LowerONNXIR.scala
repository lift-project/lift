package rewriting

import ir.ast.onnx.ONNXPattern
import ir.ast.{Expr, FunCall, Lambda}
import rewriting.Rewrite.applyRuleAt
import rewriting.rules.Rule
import rewriting.utils.NumberExpression

object LowerONNXIR {

  case class AppliedRule(rule: Rule, position: Int)
  case class RuleApplication(expr: Expr, appliedRule: AppliedRule)

  /**
    * Finds all the ONNX primitives in an expression.
    * It is important that the traversal happens in depth-first order for remapping
    * the rewritten tree later during lowering
    */
  def findONNXNodes(expr: Expr): List[Expr] = {
    Expr.visitWithStateDepthFirst(List[Expr]())(expr, (e, onnxNodes) => {
      e match {
        case FunCall(_: ONNXPattern, _*) => onnxNodes :+ e
        case _ => onnxNodes
      }})}

  /**
    * Lowers the (potentially parametric) expression into a set of (still parametric) expressions that have all ONNX
    * nodes replaced with equivalent Lift expressions. Each ONNX node can be lowered into one of multiple semantically
    * equivalent expressions, hence returning a collection.
    * TODO: update doc
    * @param expr the expression with ONNX nodes to lower
    * @param nodesToLower the list of ONNX nodes that need lowering; if a node is not in this list, it will not be
    *                       lowered
    * @param loweringRules the rules defining how each node can be rewritten
    * @return a collection  of semantically equivalent lowered expression candidates
    */
  def lower(expr: Expr,
            nodesToLower: List[Expr],
            loweringRules: List[Rule],
            rulesApplied: List[AppliedRule]): List[(Expr, List[AppliedRule])] = {
    nodesToLower match {
      case Nil =>
        List((expr, rulesApplied)) // No nodes to lower

      case node :: remainingNodes =>
        if (loweringRules.exists(rule => rule.isDefinedAt(node)))
        // Lower; for each lowering candidate, lower the remaining nodes
          loweringRules.
            filter(rule => rule.isDefinedAt(node)).
            map(rule => {
              // Since we are doing the depth-first numbering, the IDs of the previously applied rules must still be
              // correct even though they were computed based on the expression before rewriting. This is because
              // nodesToLower is ordered in the top-down order of occurrence on the AST
              val nodeId = NumberExpression.depthFirst(expr)(node)

              RuleApplication(
                expr = applyRuleAt(expr = expr, rule = rule, toBeReplaced = node),
                appliedRule = AppliedRule(rule, nodeId))
            }).
            foldLeft(List[(Expr, List[AppliedRule])]())((accumulatedRewrites, ruleApplication) =>

              accumulatedRewrites ++ lower(
                ruleApplication.expr,
                remainingNodes,
                loweringRules,
                rulesApplied :+ ruleApplication.appliedRule))

        else throw new RuntimeException(s"No lowering rewrite rules for the node $node")
    }
  }


  def apply(lambda: Lambda, loweringRules: List[Rule]): List[(Expr, List[AppliedRule])] = {
    // Find the nodes to lower
    val onnxNodes: List[Expr] = findONNXNodes(lambda.body)

    lower(lambda.body, onnxNodes, loweringRules, List())
  }
}
