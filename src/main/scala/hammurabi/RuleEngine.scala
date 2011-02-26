package hammurabi

import collection._
import Rule._

/**
 * @author Mario Fusco
 */

class RuleEngine(rule: Rule) {

  def execOn(workingMemory: WorkingMemory) = {
    new RuleEvaluator(rule, workingMemory) evaluate
  }
}

object RuleEngine {
  def apply(rule: Rule) = new RuleEngine(rule)
}

private[hammurabi] class RuleEvaluator(rule: Rule, workingMemory: WorkingMemory) {
  var isFirstExecution = true
  var ruleFinished = false
  var hasBoundValues = false
  var boundValuesIterator: Iterator[_] = _

  def evaluate = {
    execRule
    isFirstExecution = false
    ruleFinished = !hasBoundValues
    while (!ruleFinished) { execRule }
  }

  def execRule = {
    inContext {
      val ruleDef = rule.bind()
      if (!ruleFinished && ruleDef.condition()) ruleDef.execution()
    }
  }

  def inContext(block: => Unit) = {
    evaluationContext.set(this)
    block
    evaluationContext.set(null)
  }

  def any[A](clazz: Class[A]): Option[A] = {
    if (isFirstExecution) {
      boundValuesIterator = workingMemory.all(clazz).toIterator
      hasBoundValues = true
    }
    ruleFinished = !boundValuesIterator.hasNext
    if (ruleFinished) None else Some(boundValuesIterator.next.asInstanceOf[A])
  }

}