package puck.javaGraph.search

import org.extendj.ExtendJGraphUtils.{Rules, dotHelper, violationsKindPriority}
import puck.graph._
import puck.graph.constraints.ConstraintsMapsUtils._
import puck.graph.constraints.search._
import puck.javaGraph.{ScenarioFactory, SearchEngineWithLoggedFitness}
import puck.search._

/**
  * Created by Mikal on 09/06/2016.
  */
object PicoTestSearch {



  val path = getClass.getResource("/picoPersonne/").getPath

  import SearchTest.solsDir

  def main(args : Array[String]) : Unit = {
    val filePaths = Seq(
      s"$path/src/pico/Personne.java",
      s"$path/src/pico/Client.java",
      s"$path/src/pico/Main.java"
    )

    val scenario = new ScenarioFactory(filePaths:_*)


    val constraints = scenario.parseConstraintsFile(s"$path/decouple.wld")

    val fitness1: DependencyGraph => Double =
     Metrics.fitness1(_, constraints,10,2).toDouble

    val nodesSet = scenario.graph nodesIn constraints

//    val fitness2: DependencyGraph => Double =
//      Metrics.fitness2(_, nodesSet).toDouble
//scenario.graph.

//    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Option[ConcreteNode]](fitness1)
    val evaluator = DecoratedGraphEvaluator.equalityByMapping[Any](fitness1)
    val strategy = new AStarSearchStrategyGraphDisplay[Any](
      evaluator, Some(constraints),
      1000, 1000, solsDir)


//    val control = new BlindControl(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
//      constraints, WithVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]

    val control = new ControlWithHeuristic(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
      constraints, NoVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]
 //   val control = new BlindControl(Rules, scenario.graph.newGraph(mutabilitySet = scenario.initialMutability),
 //     constraints, NoVirtualNodes, violationsKindPriority).asInstanceOf[SearchControl[DecoratedGraph[Any]]]

    // SearchEngine(strategy, control, Some(1)) :  Some(n) => n sol(s), None => all sols
    val engine = new SearchEngineWithLoggedFitness(strategy, control, constraints, Some(1), Some(evaluator))
    engine.explore()

    SearchTest.printResult(engine.successes,
      engine.searchStrategy.SearchStateOrdering,
      scenario.fullName2id, constraints, filePaths:_*)


  }
}
