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

package puck.graph
package constraints.search


import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search._


class ControlWithHeuristic
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[DecoratedGraph[Option[(ConcreteNode, AutomataState)]]]
  with Heuristic
  with TargetFinder {

  def initialState: DecoratedGraph[Option[(ConcreteNode, AutomataState)]] = (initialGraph, None)

  def nextStates(g : DependencyGraph,
                 violationTarget : ConcreteNode,
                 automataState: AutomataState) : Seq[LoggedTry[DecoratedGraph[Option[(ConcreteNode, AutomataState)]]]] =
    if(!isViolationTarget(g, violationTarget.id)) Seq(LoggedSuccess((g, None)))
    else mapSeqLoggedTry[DecoratedGraph[AutomataState], DecoratedGraph[Option[(ConcreteNode, AutomataState)]]](
      nextStates(violationTarget)(g, automataState),
      { case (g1, automataState1) => (g1, Some((violationTarget, automataState1)))})



  def nextStates(state : DecoratedGraph[Option[(ConcreteNode, AutomataState)]]) : Seq[LoggedTry[DecoratedGraph[Option[(ConcreteNode, AutomataState)]]]] = {
    state match {
      case (g, Some((violationTarget, automataState))) => nextStates(g, violationTarget, automataState)
      case (g, None) => findTargets(g) flatMap (nextStates(g, _, 0))
    }
  }

}



