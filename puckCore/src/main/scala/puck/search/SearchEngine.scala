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

package puck.search

import puck._
import puck.graph.{LoggedSuccess, LoggedTry}

import scala.collection.mutable
import scalaz.{-\/, \/-}

trait Search[Result]{
  def initialState : SearchState[Result]
  def results : Seq[SearchState[Result]]
  def failures : Seq[SearchState[Result]]
  def exploredStates : Int

  def failuresByDepth : Map[Int, Seq[SearchState[Result]]] =
    failures.foldLeft(Map[Int, Seq[SearchState[Result]]]()){
      (m, state) =>
        val seq = m.getOrElse(state.depth, Seq())
        m + (state.depth -> (state +: seq))
    }

}


class SearchEngine[T]
(val searchStrategy: SearchStrategy[T],
 val control : SearchControl[T],
 val maxResults : Option[Int] = None, // default = all result
 val evaluator : Option[Evaluator[T]] = None
) extends Search[T] {

  val results = mutable.ListBuffer[SearchState[T]]()
  val failures = mutable.ListBuffer[SearchState[T]]()

  val initialState : SearchState[T] = new SearchState(0 , None, LoggedSuccess(control.initialState))


  val enoughResults : () => Boolean = {
    maxResults match {
      case None => () => false
      case Some(i) => () => results.length >= i
    }
  }

  private [this] var idSeed : Int = 0
  private def idGen() : Int = {idSeed += 1; idSeed}

  val storeSuccess : SearchState[T] => Unit =
    evaluator match {
      case None => resState =>
        ignore(results += resState)
      case Some(ev) =>
        resState =>
          if(results.forall(!ev.equals(_, resState)) && !enoughResults()) {
            println(s"result: $resState")
            ignore(results += resState)
          }
    }

  protected var numExploredStates = 0

  def addState(state : SearchState[T]) : Unit =
    ignore(
      try state.loggedResult.value match {
        case \/-(t) if control isTerminalState t =>
          storeSuccess(state)
        case \/-(_) => searchStrategy addState state
        case -\/(err) => failures += state // case added for completude but eliminated in oneStep
      }catch {
        case e :PuckError =>
          println(state.loggedResult.log)
          throw e
      }
    )

  def createState(i : Int, parent : SearchState[T], c : LoggedTry[T]) = new SearchState[T](i, Some(parent), c)

  def addChoicesStates(parent : SearchState[T], choices : Seq[LoggedTry[T]]) : Unit = {
    numExploredStates = numExploredStates + 1
    //println("add children of " + parent.uuid())
    choices.zipWithIndex.foreach {
      case (c,i) =>
      //  println("adding child " + i)
        addState(createState(i, parent, c))
    }
    //println("children added")
  }

  def init() : Unit = ()

  def exploredStates = numExploredStates

  def oneStep() : Unit= {
    val state = searchStrategy.popState()
    //println("popping "+state.uuid())
    state.loggedResult.value match {
      case \/-(cc) =>
        addChoicesStates(state, control.nextStates(cc) map (ltnext => state.loggedResult.log <++: ltnext))
      case _ => ignore(failures += state)
    }
  }

  def explore() : Unit = {
    searchStrategy.addState(initialState)

    numExploredStates = 1

    do oneStep()
    while (searchStrategy.canContinue && !enoughResults())
  }

}

trait SearchControl[T]{
  def initialState : T
  def nextStates(t : T) : Seq[LoggedTry[T]]
  def isTerminalState(t : T) : Boolean = nextStates(t).isEmpty
}

trait SearchStrategy[T] {
  def addState(s : SearchState[T]) : Unit
  def canContinue : Boolean
  def popState() : SearchState[T]
}

class Tagged[T] (var t: T, var tag : String)

object Tagged {
    implicit def tag[T](t: T) : Tagged[T] = new Tagged[T](t, "_")
    implicit def tag[T] (seq : Seq[LoggedTry[T]]) : Seq[LoggedTry[Tagged[T]]] =
      seq map (_ map (tag(_)))
    implicit def untag[T](tt : Tagged[T]) :  T = tt.t
    implicit def untag[T](seq : Seq[LoggedTry[Tagged[T]]]) : Seq[LoggedTry[T]] =
      seq map (_ map (untag(_)))
}

/*
trait TSearchControl[T <: Tagged[T]] extends SearchControl[Tagged[T]] {
  def nextTStates(t : Tagged[T]) : Seq[LoggedTry[Tagged[T]]] = nextStates(t)
} */

trait TSearchStrategy[T] extends SearchStrategy[T]{

}


/*class TSearchEngine[T] (
      override val searchStrategy: TSearchStrategy[T],
      override val control : TSearchControl[T],
      maxResults : Option[Int] = None, // default = all result
      valuator : Option[Evaluator[T]] = None
                       )
  extends SearchEngine[T] (searchStrategy, control, maxResults, valuator)  {

}*/



