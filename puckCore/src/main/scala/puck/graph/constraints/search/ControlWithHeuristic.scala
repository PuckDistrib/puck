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


import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search._


class ControlWithHeuristic
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val virtualNodePolicicy : VirtualNodePolicy,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[DecoratedGraph[Option[(ConcreteNode, AutomataState)]]]
  with Heuristic
  with TargetFinder
  with TerminalStateWhenNoForbiddenDependencies[Option[(ConcreteNode, AutomataState)]]{

  def initialState: DecoratedGraph[Option[(ConcreteNode, AutomataState)]] = (initialGraph, None, "")

  def nextStates(g : DependencyGraph,
                 violationTarget : ConcreteNode,
                 automataState: AutomataState) : Seq[LoggedTry[DecoratedGraph[Option[(ConcreteNode, AutomataState)]]]] =
    if(!isForbidden(g, violationTarget.id)) Seq(LoggedSuccess((g, None, "")))
    else mapSeqLoggedTry[DecoratedGraph[AutomataState], DecoratedGraph[Option[(ConcreteNode, AutomataState)]]](
      hNextStates(g, violationTarget, automataState),
      //{ case (g1, automataState1, transfo) => if (automataState1 == 3)  (g1, Some((violationTarget, 0)),transfo    ) else (g1, Some((violationTarget, automataState1)),transfo    )})
      { case (g1, automataState1, transfo) => (g1, Some((violationTarget, automataState1)),transfo    )})



  def nextStates(state : DecoratedGraph[Option[(ConcreteNode, AutomataState)]]) :
                                          Seq[LoggedTry[DecoratedGraph[Option[(ConcreteNode, AutomataState)]]]] = {
    state match {
      case (g, Some((violationTarget, automataState)), _) => nextStates(g, violationTarget, automataState)
      case (g, None, _) => findTargets(g) flatMap (nextStates(g, _, 0))
    }
  }

}



