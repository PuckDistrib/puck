package puck.graph.constraints.search

import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.TransformationRules
import puck.search.SearchControl


trait Blind extends ActionGenerator {
  def log(s : Seq[LoggedTG], preMsg : String)  : Seq[LoggedTG]  =
    s map (preMsg <++: _)

  def nextStates(violationTarget : ConcreteNode)(g: DependencyGraph) : Seq[LoggedTG] =
    log(
      log(moveAction(g, violationTarget), "nextStates - move action\n") ++
      //log(moveContainerAction(g, violationTarget), "nextStates - move COntainer action\n") ++
      log(abstractAction(g, violationTarget), "nextStates - abstract action\n")  ++
        redirectTowardAbstractions(g, violationTarget), "nextStates - redirect toward abstractions\n") //++
     // log(abstractContainerAction(g, violationTarget), "nextStates - abstract container action\n")
}

class TargetedBlindControl
( val rules: TransformationRules,
  val initialGraph: DependencyGraph,
  val constraints: ConstraintsMaps,
  val virtualNodePolicicy : VirtualNodePolicy,
  val violationTarget : ConcreteNode
) extends SearchControl[DecoratedGraph[Unit]] //DecoratedGraph[Unit] let us share the DecoratedGraphEvalutaor
  with Blind
  with CheckForbiddenDependency
  with TerminalStateWhenTargetedForbiddenDependencyRemoved[Unit] {
  def initialState: DecoratedGraph[Unit] = (initialGraph, ())

  def nextStates(t: DecoratedGraph[Unit]): Seq[LoggedTry[DecoratedGraph[Unit]]] =
    if(!isForbidden(t.graph, violationTarget.id)) Seq()
    else decorate(nextStates(violationTarget)(t.graph), ())


}

class BlindControl
(val rules: TransformationRules,
 val initialGraph: DependencyGraph,
 val constraints: ConstraintsMaps,
 val virtualNodePolicicy : VirtualNodePolicy,
 val violationsKindPriority : Seq[NodeKind]
) extends SearchControl[DecoratedGraph[Option[ConcreteNode]]]
  with Blind
  with TargetFinder
  with TerminalStateWhenNoForbiddenDependencies[Option[ConcreteNode]] {

  def initialState: DecoratedGraph[Option[ConcreteNode]] = (initialGraph, None)

  def nextStates(g: DependencyGraph)(violationTarget : ConcreteNode) : Seq[LoggedTry[DecoratedGraph[Option[ConcreteNode]]]] =
    if(constraints.noForbiddenDependencies(g)) Seq()
    else if(!isForbidden(g, violationTarget.id)) Seq(LoggedSuccess((g, None)))
    else decorate(nextStates(violationTarget)(g.mileStone), Some(violationTarget))


  def nextStates(state : DecoratedGraph[Option[ConcreteNode]]) : Seq[LoggedTry[DecoratedGraph[Option[ConcreteNode]]]] =
    state match {
      case (g, Some(violationTarget)) => nextStates(g)(violationTarget)
      case (g, None) => findTargets(g) flatMap nextStates(g)
    }

}



