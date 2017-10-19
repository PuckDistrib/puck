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

import puck.graph.{LoggedSuccess, LoggedError, LoggedTry}

import scala.collection.mutable.ListBuffer
import scalaz.{-\/, \/-}

class IntControl
(val initialState: Tagged[Int],
  target : Tagged[Int])
  extends SearchControl[Tagged[Int]]{

  import IntSearch._

   override def nextStates(ti: Tagged[Int]): Seq[LoggedTry[Tagged[Int]]] = {
     if(ti.t == target.t) Seq()
     else actionMoins(ti.t) ++ actionPlus(ti.t)
   }
}


object IntSearch{

  val actionPlus : Int => Seq[LoggedTry[Tagged[Int]]] =
    i => Seq(LoggedSuccess(new Tagged[Int](i + 1,"+")))

  val actionMoins : Int => Seq[LoggedTry[Tagged[Int]]] =
    i => Seq(LoggedSuccess(new Tagged[Int](i - 1,"-")))

}

object IntSearchTest extends App {

  val se = new SearchEngine[Tagged[Int]](new BreadthFirstSearchStrategy(),
    new IntControl(new Tagged(0,""), new Tagged(5,"")), Some(5))
    println("launching search ... ")
    se.explore()
    println("Explored States : " + se.exploredStates)
    println("Success depth : " + se.results.head.depth)
    PrintResults.printListRes(se.results)
}

object PrintResults {

  def printListRes[T](res: ListBuffer[SearchState[Tagged[T]]]): Unit = {
    res map (printRes(_))
  }

  def printRes[T](ss: SearchState[Tagged[T]]): Unit  = {
        ss.loggedResult map (println(_))
        ss.prevState map (printRes(_))
  }
}