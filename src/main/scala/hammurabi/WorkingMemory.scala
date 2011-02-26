package hammurabi

import collection.mutable.HashMap

/**
 * @author Mario Fusco
 */

class WorkingMemory(workingSet: Traversable[_]) {

  val workingSetsByType = new HashMap[Class[_], Traversable[_]]

  def all[A](clazz: Class[A]): Traversable[A] = {
    (workingSetsByType get clazz match {
      case objects: Some[_] => objects get
      case None => {
        val t = findObjectsOfClass(clazz)
        workingSetsByType + (clazz -> t)
        t
      }
    }).asInstanceOf[Traversable[A]]
  }

  private def findObjectsOfClass[A](clazz: Class[A]) = workingSet filter (_.asInstanceOf[Object].getClass() == clazz)
}