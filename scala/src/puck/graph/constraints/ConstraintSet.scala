package puck.graph.constraints

import scala.collection.mutable
import puck.graph.{NodeKind, AGNode}

/**
 * Created by lorilan on 11/06/14.
 */
class ConstraintSet[Kind <: NodeKind[Kind], T <: Constraint[Kind]] extends Iterable[T]{

  class Success extends Exception

  private val content = mutable.Buffer[T]()

  override def toString = content.mkString("\n")

  def iterator = content.iterator

  def += (ct : T) = content += ct
  def -= (ct : T) = content -= ct

  def clear() = content.clear()

  def friendScopeThatContains_*(n: AGNode[Kind]) = {
    var res : Option[AGNode[Kind]] = None
    try {
      content.foreach {
        _.friends.scopeThatContains_*(n) match {
          case None => false
          case s =>
            res = s
            throw new Success()
        }
      }
      None
    } catch {
      case _: Success => res
    }
  }
  def hasFriendScopeThatContains_*(n : AGNode[Kind])=
    content.exists( _.friends.hasScopeThatContains_*(n))
}
