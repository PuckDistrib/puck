package puck.javaGraph.search

import puck.graph.constraints.ConstraintsMapsUtils._
import puck.graph.constraints.search.{ControlWithHeuristic, DecoratedGraphEvaluator, WithVirtualNodes}
import puck.javaGraph.{ScenarioFactory, SearchEngineWithLoggedFitness}
import puck.graph._
import puck.search._
import org.extendj.ExtendJGraphUtils.{Rules, dotHelper, violationsKindPriority}

/**
  * Created by Mikal on 09/06/2016.
  */
object NanoTestSearch {



  val path = getClass.getResource("/nanoPersonne/").getPath

  import SearchTest.solsDir

  def main(args : Array[String]) : Unit = {
    val filePaths = Seq(
      s"$path/nano/Personne.java",
      s"$path/nano/Client.java")
    val scenario = new ScenarioFactory(filePaths:_*)


    val constraints = scenario.parseConstraintsFile(s"$path/decouple.wld")

//    val fitness1: DependencyGraph => Double =
//      Metrics.fitness1(_, constraints, 1, 1).toDouble

    val nodesSet = scenario.graph nodesIn constraints

    val fitness2: DependencyGraph => Double =
      Metrics.fitness2(_, nodesSet).toDouble

//    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Option[ConcreteNode]](fitness1)
    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Any](fitness2)
    val strategy = new AStarSearchStrategyGraphDisplayOnly[Any](
      evaluator, Some(constraints),
      10, 1000, solsDir)


//    val control = new BlindControl(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
//      constraints, WithVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]

    val control = new ControlWithHeuristic(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
      constraints, WithVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]

    // SearchEngine(strategy, control, Some(1)) :  Some(n) => n sol(s), None => all sols
    val engine = new SearchEngineWithLoggedFitness(strategy, control, constraints)
    engine.explore()

    SearchTest.printResult(engine.results,
      engine.searchStrategy.SearchStateOrdering,
      scenario.fullName2id, constraints, filePaths:_*)


  }
}
