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

import puck.graph.{LoggedSuccess, LoggedTry}

class IntControl
(val initialState: Int,
  target : Int)
  extends SearchControl[Int]{

  import IntSearch._


  def nextStates(i: Int): Seq[LoggedTry[Int]] = {
    if(i == target) Seq()
    else actionMoins(i) ++ actionPlus(i)
  }
}

object IntSearch{

  val actionPlus : Int => Seq[LoggedTry[Int]] =
    i => Seq(LoggedSuccess(i + 1))

  val actionMoins : Int => Seq[LoggedTry[Int]] =
    i => Seq(LoggedSuccess(i - 1))

}

object IntSearchTest extends App {

  val se = new SearchEngine[Int](new BreadthFirstSearchStrategy(),
    new IntControl(0, 5), Some(1))
    println("launching search ... ")
    se.explore()
    println("Explored States : " + se.exploredStates)
    println("Success depth : " + se.results.head.depth)
}
