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

package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search.SearchControl

/**
  * Created by Loïc Girault on 10/05/16.
  */

trait Heuristic extends ActionGenerator {
  def assertNonEmpty(s : Seq[LoggedTry[DecoratedGraph[AutomataState]]]) = {
    assert(s.nonEmpty)
    s
  }

  def hNextStates
  (g : DependencyGraph,
   violationTarget : ConcreteNode,
   automataState : AutomataState) : Seq[LoggedTry[DecoratedGraph[AutomataState]]] = {
 /*   val vtt = violationTarget.kind.kindType match {
      case TypeDecl => "td"
      case NameSpace => "ns"
      case TypeVariableKT => "kt"
      case InstanceTypeDecl => "it"
      case TypeConstructor => "tc"
      case InstanceValue => "iv"
      case StableValue => "sv"
      case LocalValue => "lv"
      case Parameter => "p"
      case ValueDef => "vd"
      case UnknownKindType => "u"
    }*/

    val vt = violationTarget.name

    automataState match {
      case 0 =>
        assertNonEmpty(decorate(moveAction(g, violationTarget), 2, " M:"+vt)
          ++ decorate(abstractAction(g, violationTarget), 1, " A:"+vt)
          ++ decorate(abstractContainerAction(g, violationTarget), 1, " K:"+vt)
          ++ decorate(moveContainerAction(g, violationTarget), 2, " C:"+vt)
        )
      case 1 =>
        assertNonEmpty(decorate(redirectTowardAbstractions(g, violationTarget), 2, " R:"+vt))
      case 2 => decorate(epsilon(g), 0, ".") // Seq()
      case _ => puck.error()
    }
  }


/*  def hNextStates
  (g : DependencyGraph,
   violationTarget : ConcreteNode,
   automataState : AutomataState) : Seq[LoggedTry[DecoratedGraph[AutomataState]]] =
    automataState match {
      case 0 =>
        assertNonEmpty(
          decorate(abstractAction(g, violationTarget), 2)
            ++ decorate(moveContainerAction(g, violationTarget), 3)
            ++ decorate(moveAction(g, violationTarget), 3))
      case 2 =>
        assertNonEmpty(decorate(redirectTowardAbstractions(g, violationTarget),3))

      case 3 => Seq()

      case _ => puck.error()
    }
*/

  /* def hNextStates
   (g : DependencyGraph,
    violationTarget : ConcreteNode,
    automataState : AutomataState) : Seq[LoggedTry[DecoratedGraph[AutomataState]]] =
     automataState match {
       case 0 =>
         assertNonEmpty(
           (if (violationTarget.kind.kindType.equals(TypeConstructor))
             Seq()
               else
             decorate(epsilon(g), 1) ++
             decorate(moveAction(g, violationTarget), 1)) ++
           decorate(abstractAction(g, violationTarget), 2) ++
           decorate(moveContainerAction(g, violationTarget), 3)
         )

       case 1 =>
         assertNonEmpty(
           decorate(
             abstractContainerAction(g, violationTarget)
             , 2)
         )

       case 2 =>
         assertNonEmpty(decorate(redirectTowardAbstractions(g, violationTarget),3))

       case 3 => Seq()

       case _ => puck.error()
     }*/

//
//  def nextStates
//  (violationTarget : ConcreteNode )
//  (g : DependencyGraph,
//   automataState : AutomataState) : Seq[LoggedTry[DecoratedGraph[AutomataState]]] =
//    automataState match {
//      case 0 =>
//        assertNonEmpty(
//          decorate(epsilon(g) ++ abstractionHostIntroAction(g, violationTarget), 4) ) ++
//          decorate(epsilon(g), 2) ++
//          decorate(epsilon(g) ++ hostIntroAction(violationTarget)(g), 1) ++
//          decorate(epsilon(g) ++ containerHostIntroAction(g, violationTarget), 5)
//
//      case 1 =>
//        assertNonEmpty(decorate(moveAction(violationTarget)(g), 2) )
//
//      case 2 =>
//        assertNonEmpty(decorate(epsilon(g) ++
//          abstractedContainerHostIntroAction(g, violationTarget), 7))
//
//      case 3 =>
//        assertNonEmpty(decorate(redirectTowardAbstractions(g, violationTarget),6))
//
//      case 4 =>
//        assertNonEmpty(decorate( abstractAction(g, violationTarget), 3))
//
//      case 5 =>
//        assertNonEmpty(decorate(moveContainerAction(g, violationTarget), 6))
//
//      case 6 => Seq()
//
//      case 7 =>
//        assertNonEmpty(decorate(abstractContainerAction(g, violationTarget), 3))
//    }
}

class TargetedControlWithHeuristic
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val virtualNodePolicicy : VirtualNodePolicy,
 val violationTarget : ConcreteNode
) extends SearchControl[DecoratedGraph[AutomataState]]
  with Heuristic
  with CheckForbiddenDependency
  with TerminalStateWhenTargetedForbiddenDependencyRemoved[AutomataState] {

  def initialState: DecoratedGraph[AutomataState] = (initialGraph, 0, "")

  def nextStates(state : DecoratedGraph[AutomataState]) : Seq[LoggedTry[DecoratedGraph[AutomataState]]] =
    if(!isForbidden(state.graph, violationTarget.id)) Seq()
    else hNextStates(state.graph, violationTarget, state.decoration)


}