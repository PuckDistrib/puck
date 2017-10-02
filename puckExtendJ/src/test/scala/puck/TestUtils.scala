package puck

import java.awt.event.WindowEvent
import java.io.{File, FileWriter}

import org.extendj.ExtendJGraphUtils.{dotHelper, Rules, violationsKindPriority}
import puck.control.actions.Choose
import puck.graph.Metrics._
import puck.graph.{ConcreteNode, DecoratedGraph, DependencyGraph, LoggedTG, MutabilitySet, NodeId}
import puck.graph.transformations.Recording
import puck.search._
import puck.util.LoggedEither
import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success
import scalaz.\/-

/**
  * Created by cedric on 26/04/2016.
  */
object TestUtils {

  def initDir(name: String): Unit = {
    val dir = new File(name)

    if (!dir.exists())
      ignore(dir.mkdirs())
    else {
      val files: Array[String] = dir.list()
      files foreach {
        case f: String =>
          //println(f)
          import puck.util.FileHelper.FileOps
          (dir \ f).delete()
      }
      ignore(dir.mkdirs())
    }
  }

  def showSuccess[T](ss : SearchState[DecoratedGraph[T]]) = {
    val LoggedEither(_, \/-(dg)) = ss.loggedResult
    Quick.frame(dg.graph, "G")
  }

  def printSuccessState[T](outDir : String, name : String,
                           ss : SearchState[DecoratedGraph[T]]) : Unit = {
    val LoggedEither(log, \/-(dg)) = ss.loggedResult
    //    Quick.svg(dg.graph, dir + File.separator + name + ".svg", scm)
    val fw = new FileWriter(outDir + File.separator + name + ".transfos")
    fw.write(log)
    fw.close()
  }

  def showEngineSuccesses[T](engine : SearchEngine[DecoratedGraph[T]]) =
    engine.results foreach showSuccess

  def removeVirtualNodes(gWithoutVirtualNodes: DependencyGraph,
                         gWithVirtualNodes: DependencyGraph,
                         scm: Option[ConstraintsMaps] = None): DependencyGraph = {
    var g0 = gWithVirtualNodes
    while (g0.virtualNodes.nonEmpty) {
      val ff = Quick.frame(g0, "G", scm)
      val vn = g0.virtualNodes.head
      Choose("Concretize node",
        s"Select a concrete value for the virtual node $vn :",
        vn.potentialMatches.toSeq map g0.getConcreteNode) match {
        case None => ()
        case Some(cn) =>
          import Recording.RecordingOps
          val r2 = g0.recording.subRecordFromLastMilestone.concretize(vn.id, cn.id)
          g0 = r2 redo gWithoutVirtualNodes
      }
      ff onComplete {
        case Success(jframe) =>
          jframe.dispatchEvent(new WindowEvent(jframe, WindowEvent.WINDOW_CLOSING));
        case _ => println("failure")
      }
    }
    g0
  }

  def solveAllBlind(graph : DependencyGraph, constraints : ConstraintsMaps, mutabilitySet: MutabilitySet,
                       strategy : SearchStrategy[DecoratedGraph[Option[ConcreteNode]]],
                       maxResult : Option[Int]) : Seq[SearchState[DecoratedGraph[Option[ConcreteNode]]]] = {
    val control = new BlindControl(Rules, graph.newGraph(mutabilitySet = mutabilitySet),
      constraints, WithVirtualNodes, violationsKindPriority)
    val engine = new SearchEngine(strategy, control, maxResult)
    engine.explore()
    engine.results
  }

  implicit def toOption( s : Seq[LoggedTG]) : Option[DependencyGraph] = s match {
    case Nil => None
    case LoggedEither(_, \/-(g))  +: _ => Some(g)
  }

  def printMetrics(g : DependencyGraph) : Unit = {
    println(s"############################################")
    println(s"############################################")
    def p[T](name : String, metric : (DependencyGraph, NodeId) => T, avg : Seq[T] => Double) : Unit = {
      println(s"******\t$name\t******")
      val lcoms = apply_metric_on_types(metric, g, Seq("@primitive", "java"))
      println(lcoms mkString "\n")
      println("average = " + avg(lcoms.map(_._2)))
    }
    p("LCOM", LCOM, averageI)

    p("LCOM hs", LCOM_hs, averageD)

    p("LCOM4", LCOM4, averageI)

    p("CBO", CBO, averageI)

    println()
  }

}
