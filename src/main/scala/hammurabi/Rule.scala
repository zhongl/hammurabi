package hammurabi

/**
 * @author Mario Fusco
 */

import actors.Actor._

class Rule(val description: String, val bind: () => RuleDefinition) {

  def exec() = {
    val ruleDef = bind()
    if (ruleDef.condition()) ruleDef.execution()
  }
}

case class RuleDefinition(condition: () => Boolean, execution: () => Unit)

object Rule {
  implicit def stringToRule(s: String) = new {
    def let(letClause: => RuleDefinition) = new Rule(s, letClause _)
  }

  def when(condition: => Boolean) = new {
    def then(execution: => Unit) = RuleDefinition(condition _, execution _)
  }

  private[hammurabi] val evaluationContext = new ThreadLocal[RuleEvaluator]

  def any[A](clazz: Class[A]): A = evaluationContext.get().any(clazz).getOrElse(null.asInstanceOf[A])
}
