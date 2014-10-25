package puck.graph.constraints.search

import puck.graph.NodeKind
import puck.graph.backTrack.Recording
import puck.graph.constraints.Solver
import puck.search.SearchEngine

import scala.collection.mutable

/**
 * Created by lorilan on 25/10/14.
 */
class CSInitialSearchState[Kind <: NodeKind[Kind]](e : SearchEngine[Recording[Kind]],
                                                   solver : Solver[Kind])
  extends ConstraintSolvingNodeChoiceSearchState[Kind](0, solver.graph.transformations.recording, e,
    new ConstraintSolvingNodesChoice[Kind](null, mutable.Set(), mutable.Set()), None){

  var executedOnce = false
  override def triedAll = executedOnce

  override def executeNextChoice(){
    //solver.solve(() => printTrace(e.currentState))
    solver.solve()
    executedOnce = true
  }
}
