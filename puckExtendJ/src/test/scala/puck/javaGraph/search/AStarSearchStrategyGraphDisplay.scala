package puck.javaGraph.search

import puck.TestUtils
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io.DotHelper
import puck.graph.{DecoratedGraphOps, _}
import puck.search.{AStarSearchStrategy, Evaluator, SearchState}
import puck.Quick

import scalaz.\/-

/**
  * Created by cedric on 18/05/2016.
  */
class AStarSearchStrategyGraphDisplayOnly[T]
(evaluator: Evaluator [DecoratedGraph[T]],
 scm : Option[ConstraintsMaps] = None,
 maxDepth : Int = 100, // ajouté par Mikal
 maxSize : Int = 10,
 dir : String)
(implicit dotHelper : DotHelper)
  extends AStarSearchStrategy[DecoratedGraph[T]](evaluator, maxDepth, maxSize) {

  TestUtils.initDir(dir)

  override def addState(s: SearchState[DecoratedGraph[T]]): Unit = {
    super.addState(s)
    if (s.isSuccess) {
      //Quick.frame(dg.graph, "Best current solution", scm)
       //   println ("size = " + remainingStates.length +" "+  remainingStates.mkString("/") )
      remainingStates foreach {
        rs =>
          val \/-(dg) = rs.loggedResult.value
          val v = Metrics.numViolations(dg.graph, scm.get)
          print(s"$rs (${SearchStateOrdering.evaluateWithDepthPenalty(rs)}, ${v}V) / ")
      }
      println()
    }
  }

}

class AStarSearchStrategyGraphDisplay[T]
(evaluator: Evaluator [DecoratedGraph[T]],
 scm : Option[ConstraintsMaps] = None,
 maxDepth : Int = 100, // ajouté par Mikal
 maxSize : Int = 10,
 dir : String)(implicit dotHelper : DotHelper)
  extends AStarSearchStrategyGraphDisplayOnly[T](
    evaluator, scm, maxDepth, maxSize, dir) {

  var i : Integer = 0

  override def addState(s: SearchState[DecoratedGraph[T]]): Unit = {
    super.addState(s)
    if (s.isSuccess && remainingStates.nonEmpty) {
        i+=1
        val name =  "#_"+ i + "_#" + remainingStates.head.toString
        TestUtils.printSuccessState(dir, name, remainingStates.head)
    }
  }
}
