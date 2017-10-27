package puck.javaGraph.search

import java.io.File

import org.extendj.ExtendJGraphUtils.{Rules, dotHelper, violationsKindPriority}
import puck.TestUtils._
import puck.graph.constraints.search.{ControlWithHeuristic, DecoratedGraphEvaluator, NoVirtualNodes}
import puck.graph.{ConcreteNode, _}
import puck.javaGraph.search.SearchTest.solsDir
import puck.javaGraph.{ScenarioFactory, SearchEngineWithLoggedFitness}
import puck.search.{AStarSearchOrdering, PrintResults, SearchControl}

/**
  * Created by cedric on 23/05/2016.
  */

object BridgeTestSearch {
  val path = getClass.getResource("/bridge/hannemann_simplified/").getPath

  val outDir = SearchTest.outDir + File.separator + "DG-Imgs"

  def main(args : Array[String]) : Unit = {
    val filePaths = Seq(
      s"$path/screen/BridgeDemo.java",
      s"$path/screen/Screen.java")
    val scenario = new ScenarioFactory(filePaths:_*)

    val constraints = scenario.parseConstraintsFile(s"$path/decouple.wld")

    //      val res = solveAll_targeted(graph, constraints, blindControlBuilder,
    //        () => new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(_.numNodes)),
    //        Some(100),Some(5))


    val fitness1: DependencyGraph => Double =
      Metrics.fitness1(_, constraints, 1, 1).toDouble

    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Any](fitness1)
    val strategy = new AStarSearchStrategyGraphDisplay[Any](
      evaluator, Some(constraints),
      100, 20, solsDir)


    val control = new ControlWithHeuristic(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
      constraints, NoVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]
    //val control = new BlindControl(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
    //   constraints, NoVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]

    // SearchEngine(strategy, control, Some(1)) :  Some(n) => n sol(s), None => all sols
    val engine = new SearchEngineWithLoggedFitness(strategy, control, constraints, Some(1), Some(evaluator))
    engine.explore()

    PrintResults.printListRes(engine.results)
    SearchTest.printResult(engine.results,
      engine.searchStrategy.SearchStateOrdering,
      scenario.fullName2id, constraints, filePaths:_*)


  }
}
