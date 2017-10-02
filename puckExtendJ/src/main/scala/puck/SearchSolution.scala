package puck

import org.extendj.ExtendJGraphUtils._
import puck.graph.MutabilitySet.MutabilitySetOps
import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search._
import puck.graph.transformations.{Immutable, Recording}
import puck.graph.{DecoratedGraph, DependencyGraph, InstanceTypeDecl, InstanceValue, LoggedSuccess, Metrics, Parameter, StableValue, TypeDecl, ValueDef}
import puck.search._
import puck.util.{PuckFileLogger, PuckLog, PuckLogger}

/**
  * Created by Loïc Girault on 29/07/16.
  */
sealed abstract class StrategyChoice {

  def apply(): SearchStrategy[DecoratedGraph[Any]]
}
object StrategyChoice {
  case object BreadthFirst extends StrategyChoice {

    override val toString ="breadthFirst"

    def apply() : SearchStrategy[DecoratedGraph[Any]] =
      new BreadthFirstSearchStrategy[DecoratedGraph[Any]]
  }

  case object DepthFirst extends StrategyChoice {

    override val toString ="depthFirst"

    def apply() : SearchStrategy[DecoratedGraph[Any]] =
      new DepthFirstSearchStrategy[DecoratedGraph[Any]]

  }

  case class AStart(cm : ConstraintsMaps, kForbid : Int = 1, kComplex : Int = 1) extends StrategyChoice {

    override def toString =s"depthFirst_${kForbid}_$kComplex"

    val f = Metrics.fitness1(_: DependencyGraph, cm, kForbid, kComplex).toDouble
    def apply() : SearchStrategy[DecoratedGraph[Any]] =
      new AStarSearchStrategy[DecoratedGraph[Any]](DecoratedGraphEvaluator.equalityByMapping(f))
  }
}

sealed abstract class ControlChoice {
  def apply(g: DependencyGraph, cm: ConstraintsMaps,
            virtualNodePolicy: VirtualNodePolicy): SearchControl[DecoratedGraph[Any]]
}
object ControlChoice {
  case object Heuristic extends ControlChoice {
    override val toString ="heuristic"
    def apply(g : DependencyGraph, cm : ConstraintsMaps,
              virtualNodePolicy: VirtualNodePolicy) : SearchControl[DecoratedGraph[Any]] =
      new ControlWithHeuristic(Rules,
        g.mileStone, cm, virtualNodePolicy,
        violationsKindPriority).
        asInstanceOf[SearchControl[DecoratedGraph[Any]]]
  }
  case object Blind extends ControlChoice {
    override val toString ="blind"
    def apply(g : DependencyGraph, cm : ConstraintsMaps,
              virtualNodePolicy: VirtualNodePolicy) : SearchControl[DecoratedGraph[Any]] =
      new BlindControl(Rules,
        g.mileStone, cm, virtualNodePolicy,
        violationsKindPriority).
        asInstanceOf[SearchControl[DecoratedGraph[Any]]]
  }
}

object SearchSolution {

  def apply(p : Project, outputFolder : String,
            strategyChoice : StrategyChoice,
            controlChoice: ControlChoice,
            virtualNodePolicy: VirtualNodePolicy) : Unit = {

    val vnPostFix = virtualNodePolicy match {
      case WithVirtualNodes => "-withVN"
      case NoVirtualNodes => ""
    }

    val basename = s"$controlChoice-$strategyChoice$vnPostFix"

    import puck.util.FileHelper.FileOps
    val dir = p.workspace \ outputFolder
    dir.mkdirs()
    implicit val logger : PuckLogger = new PuckFileLogger(_ => true, dir \  s"$basename.log")

    val dg2ast = p.loadGraph()

    val mutabilitySet = dg2ast.initialGraph.nodes.foldLeft(dg2ast.initialMutability) {
      case (s, n) => n.kind.kindType match {
        case TypeDecl
             | InstanceTypeDecl
             | InstanceValue
             | StableValue
             | Parameter
             | ValueDef => s.setMutability(n.id, Immutable)
        case _ => s
      }
    }


    p.parseConstraints(dg2ast) match {
      case None => logger.writeln("no output constraints")
      case Some(cm) =>
        val g = dg2ast.initialGraph.newGraph(mutabilitySet = mutabilitySet)
        val engine = new SearchEngine(strategyChoice(),
          controlChoice(g, cm, virtualNodePolicy), Some(1))
        logger.writeln("search start !")
        puck.util.Time.time(logger, PuckLog.defaultVerbosity)(engine.explore())

        logger.writeln(engine.results.size + " solutions found")
        engine.results  map (st => (st.uuid(), st.loggedResult)) foreach {
          case (id, LoggedSuccess(_, (g,_))) =>
            import puck.util.FileHelper.FileOps
            val recFile = p.workspace \  outputFolder \  s"$basename-solution-$id.pck"
            Recording.write(recFile.getAbsolutePath, dg2ast.nodesByName, g)
        }
    }
  }
}
