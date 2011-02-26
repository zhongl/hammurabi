package hammurabi

/**
 * @author Mario Fusco
 */

class Person(n: String, var pos: Int = 0, var color: String = "") {
  val name = n

  override def toString() = name + " is in pos " + pos + " with color " + color
}

abstract class ClassProvider[T] {
  def toClass: Class[T]
}

object PersonClassProvider extends ClassProvider[Person] {
  def toClass: Class[Person] = classOf[Person]
}

object DSLHelper {
  implicit val personClassProvider = PersonClassProvider

  def of[T](implicit cp: ClassProvider[T]) = cp.toClass
}
