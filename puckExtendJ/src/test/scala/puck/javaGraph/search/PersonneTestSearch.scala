package puck.javaGraph.search

import java.io.File

import puck.graph.constraints.ConstraintsMapsUtils._
import puck.graph.constraints.search.{ControlWithHeuristic, DecoratedGraphEvaluator, WithVirtualNodes}
import puck.graph.{ConcreteNode, _}
import puck.javaGraph.{ScenarioFactory, SearchEngineWithLoggedFitness}
import puck.search.{AStarSearchOrdering, SearchControl}
import org.extendj.ExtendJGraphUtils.{Rules, dotHelper, violationsKindPriority}
import puck.javaGraph.search.NanoTestSearch.path
import puck.javaGraph.search.SearchTest.solsDir

/**
  * Created by cedric on 18/05/2016.
  */

object PersonneTestSearch {
  val path = getClass.getResource("/projetPersonne/").getPath

  val outDir = SearchTest.outDir + File.separator + "DG-Imgs"

  def main(args : Array[String]) : Unit = {
    val filePaths = Seq(
      s"$path/personne/Personne.java",
      s"$path/personne/Client.java")
    val scenario = new ScenarioFactory(filePaths: _*)

    val constraints = scenario.parseConstraintsFile(s"$path/decouple.wld")

    //      val res = solveAll_targeted(graph, constraints, blindControlBuilder,
    //        () => new AStarSearchStrategy[(DependencyGraph, Int)](SResultEvaluator.equalityByMapping(_.numNodes)),
    //        Some(100),Some(5))


    val nodesSet = scenario.graph nodesIn constraints

    val fitness2: DependencyGraph => Double =
      Metrics.fitness2(_, nodesSet).toDouble

    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Any](fitness2)
    val strategy = new AStarSearchStrategyGraphDisplayOnly[Any](
      evaluator, Some(constraints),
      10, 1000, solsDir)

    val control = new ControlWithHeuristic(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
      constraints, WithVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]

    // SearchEngine(strategy, control, Some(1)) :  Some(n) => n sol(s), None => all sols
    val engine = new SearchEngineWithLoggedFitness(strategy, control, constraints)
    engine.explore()

    SearchTest.printResult(engine.results,
      engine.searchStrategy.SearchStateOrdering,
      scenario.fullName2id, constraints, filePaths: _*)
    }
  }
