/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck

import puck.graph.constraints.search.AutomataState
import puck.graph.transformations.Recordable
import puck.util.{Logged, LoggedEither}

import scalaz.Scalaz._
import scalaz._

package object graph {

  type MutabilitySet = transformations.MutabilitySet.T
  val MutabilitySet = transformations.MutabilitySet

  type Try[+T] = PuckError \/ T


  type Error = PuckError

  def error(str : String) : Nothing = puck.error(str)
  def error() : Nothing = puck.error()

  type LoggedG = Logged[DependencyGraph]
  type LoggedTry[+A] = LoggedEither[String, A]
  type LoggedTG = LoggedTry[DependencyGraph]

  type NodePredicate = (DependencyGraph, DGNode) => Boolean

  type NodeId = Int

  type NodeIdP = (NodeId, NodeId)

  type Parameterization = Map[NodeId, Type]
  type ParameterizationId = Int

  type SetValueMap[K,V] = CollectionValueMap[K,Set,V]
  type ListValueMap[K,V] = CollectionValueMap[K,List,V]

  implicit class NodeIdPOps( val p : NodeIdP) extends AnyVal{

    def user = p._1
    def used = p._2

    def source = p._1
    def target = p._2

    def selfUse = p._1 == p._2

  }


  type TypedNode = (ConcreteNode, Type)

  type Recording = Seq[Recordable]
  val Recording = transformations.Recording

  type DecoratedGraph[T] = (DependencyGraph, T)
  type SResult = DecoratedGraph[Option[(ConcreteNode, AutomataState)]]



  implicit class DecoratedGraphOps[T](val dg : DecoratedGraph[T]) extends  AnyVal {
    def graph : DependencyGraph = dg._1
    def decoration : T = dg._2
  }


  implicit class LoggedOps[A](val lg: Logged[A]) extends AnyVal {
    def toLoggedEither[E] : LoggedEither[E, A] = LoggedEither(lg.written, lg.value.right[E])
    def toLoggedTry : LoggedTry[A] = toLoggedEither[String]
  }

  object LoggedError{
    def apply[A]( e: String): LoggedTry[A] =
      LoggedEither(e, -\/(e))

    def apply[A](msg : String, e: String): LoggedTry[A] =
      LoggedEither(msg, -\/(e))

    def unapply[A](arg: LoggedTry[A]): Option[(String, String)] =
      arg.value match {
        case -\/(err) => Some((arg.log, err))
        case _ => None
      }

  }

  object LoggedSuccess{
    def apply[A]( a : A): LoggedTry[A] =
      LoggedSuccess("", a)

    def apply[A](msg : String, a : A): LoggedTry[A] =
      LoggedEither(msg, \/-(a))


    def unapply[A](arg: LoggedTry[A]): Option[(String, A)] =
      arg.value match {
        case \/-(success) => Some((arg.log, success))
        case _ => None
      }
  }





}
