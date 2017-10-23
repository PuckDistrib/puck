package puck.javaGraph

import puck.graph.{LoggedSuccess, Metrics, _}
import puck.graph.constraints.ConstraintsMaps
import puck.javaGraph.search.PrintResults
import puck.search._

/**
  * Created by Loïc Girault on 10/24/16.
  */
class SearchEngineWithLoggedFitness[D]
(override val searchStrategy: AStarSearchStrategy[DecoratedGraph[D]],
 control : SearchControl[DecoratedGraph[D]],
 val constraints : ConstraintsMaps,
 maxResult : Option[Int] = None,// default = all result
 evaluator : Option[Evaluator[DecoratedGraph[D]]] = None)
  extends SearchEngine[DecoratedGraph[D]](searchStrategy, control, maxResult, evaluator) {

  import searchStrategy.SearchStateOrdering

  override def createState(i : Int, parent : SearchState[DecoratedGraph[D]],
                           c : LoggedTry[DecoratedGraph[D]]) = {
    val tmp = new SearchState(i, Some(parent), c)
    val c1 = c match {
      case LoggedSuccess(log, (g, sn, transfo)) =>
        val v = Metrics.numViolations(g, constraints)
        LoggedSuccess(log + s" ${PrintResults.printRes(tmp)}: $tmp (${SearchStateOrdering.evaluateWithDepthPenalty(tmp)}, ${v}V)\n", (g, sn, transfo))
      case le => le
    }
    new SearchState(i, Some(parent), c1)
  }

}
