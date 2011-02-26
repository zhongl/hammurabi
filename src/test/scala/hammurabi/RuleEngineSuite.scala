package hammurabi

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test

import Rule._
import DSLHelper._

/**
 * @author Mario Fusco
 */

class RuleEngineSuite extends AssertionsForJUnit {

  @Test def allItemsFromWorkingSetBelongToExpectedType = {
    val joe = new Person("Joe")
    val fred = new Person("Fred")
    val tom = new Person("Tom")
    val bob = new Person("Bob")

    val workingSet = Set(joe, fred, tom, "tom", bob)
    val workingMemory = new WorkingMemory(workingSet)

    val allPersons = workingMemory.all(of[Person])
    assert(allPersons forall (_.isInstanceOf[Person]))
  }

  @Test def applyedRule = {
    val joe = new Person("Joe")

    val rule = "Joe is in position 2" let {
      val p: Person = joe
      when {
        p.name == "Joe"
      } then {
        p.pos = 2
      }
    }

    assert(joe.pos != 2)
    rule.exec
    assert(joe.pos == 2)
  }

  @Test def notApplyedRule = {
    val fred = new Person("Fred")

    val rule = "Joe is in position 2" let {
      val p: Person = fred
      when {
        p.name == "Joe"
      } then {
        p.pos = 2
      }
    }

    assert(fred.pos != 2)
    rule.exec
    assert(fred.pos != 2)
  }

  @Test def applyedRuleInRuleEngine = {
    val joe = new Person("Joe")

    val rule = "Joe is in position 2" let {
      val p: Person = joe
      when {
        p.name == "Joe"
      } then {
        p.pos = 2
      }
    }

    assert(joe.pos != 2)
    RuleEngine(rule) execOn new WorkingMemory(Set())
    assert(joe.pos == 2)
  }

  @Test def singleRuleInRuleEngine = {
    val rule = "Joe is in position 2" let {
      val p = any(of[Person])
      when {
        p.name equals "Joe"
      } then {
        p.pos = 2
      }
    }

    val ruleEngine = RuleEngine(rule)

    val tom = new Person("Tom")
    val joe = new Person("Joe")
    val fred = new Person("Fred")
    val bob = new Person("Bob")
    val workingMemory = new WorkingMemory(Set(tom, joe, fred, bob))

    assert(joe.pos != 2)
    assert(fred.pos != 2)

    ruleEngine execOn workingMemory

    assert(fred.pos != 2)
    assert(joe.pos == 2)
  }
}

