package puck.javaAG

import puck.graph.constraints._
import puck.graph._
import puck.graph.constraints.SupertypeAbstraction
import puck.javaAG.nodeKind._
import puck.util.Logger

/**
 * Created by lorilan on 28/05/14.
 */

class JavaDefaultDecisionMaker(graph : AccessGraph[JavaNodeKind]) extends DefaultDecisionMaker[JavaNodeKind](graph){
  val violationsKindPriority = List[JavaNodeKind](Field(), Constructor(), Class(), Interface())
}

class JavaSolver(val graph : AccessGraph[JavaNodeKind],
                 val logger : Logger[Int],
                 val decisionMaker : DecisionMaker[JavaNodeKind]) extends Solver[JavaNodeKind]{

  override  def singleAbsIntroPredicate(impl : NodeType,
                                        absPolicy : AbstractionPolicy,
                                        absKind : JavaNodeKind) : NodeType => Boolean =
    (impl.kind, absPolicy) match {
    case (Method(), SupertypeAbstraction())
    | (AbstractMethod(), SupertypeAbstraction()) =>
      potentialHost => !(impl.container interloperOf potentialHost)
    case _ => super.singleAbsIntroPredicate(impl, absPolicy, absKind)
  }

}
