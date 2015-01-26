package puck.javaAG

import puck.graph.{NodeKind, DependencyGraph}
import puck.graph.constraints.{AbstractionPolicy, DecisionMaker, Solver, SupertypeAbstraction}
import puck.javaAG.nodeKind._


/**
 * Created by lorilan on 28/05/14.
 */

object JavaSolver{
  def apply(graph : DependencyGraph,
            decisionMaker : DecisionMaker) = new JavaSolver(graph, decisionMaker)

  val violationPrioritySeq =
              Seq[JavaNodeKind]( Field, Constructor, Class, Interface)
}

class JavaSolver(val graph : DependencyGraph,
                 val decisionMaker : DecisionMaker) extends Solver{

  val logger = graph.logger

  val rules = JavaTransformationRules

  override def absIntroPredicate( graph : GraphT,
                                  implId : NIdT,
                                  absPolicy : AbstractionPolicy,
                                  absKind : NodeKind) : PredicateT = {
    (graph.getNode(implId).kind, absPolicy) match {
      case (Method, SupertypeAbstraction)
           | (AbstractMethod, SupertypeAbstraction) =>
        (graph, potentialHost) => !graph.interloperOf(graph.container(implId).get, potentialHost)
      case _ => super.absIntroPredicate(graph, implId, absPolicy, absKind)
    }
  }
}
