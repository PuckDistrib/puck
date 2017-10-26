package puck.javaGraph.search

import java.io.File

import puck.{Settings, TestUtils}
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.Recording
import puck.javaGraph.ScenarioFactory
import puck.search.{AStarSearchOrdering, SearchState, Tagged}

import scala.collection.mutable.ListBuffer
import scalaz.\/-

/**
  * Created by Loïc Girault on 10/24/16.
  */
object SearchTest {

  val outDir = Settings.tmpDir + File.separator + "out-puck"

  var solsDir = outDir + File.separator + "sols"
  val srcDir = outDir + File.separator + "src"

  def printResult[T](res : Iterable[SearchState[DecoratedGraph[T]]],
                     ordering : AStarSearchOrdering[DecoratedGraph[T]],
                     fullName2id : Map[String, NodeId],
                     cm : ConstraintsMaps,
                     filePaths : String*) : Unit =

    if (res.isEmpty) println("no results")
    else {
      println(res.size + " result(s)")
      res foreach {
        ss =>
          val fit = ordering.evaluateWithDepthPenalty(ss)
          TestUtils.printSuccessState(solsDir, "result#" + fit + "#" +ss.uuid(), ss)
          val \/-(dg) = ss.loggedResult.value
          val result = solsDir + File.separator +"result#" + fit + "#" + ss.uuid() + ".pck"
          Recording.write(result, fullName2id, dg.graph)

          val s = new ScenarioFactory(filePaths:_*)
          val r = Recording.load(s"$result", s.fullName2id)(s.logger)
          import Recording.RecordingOps
          val resdir = new File( srcDir + File.separator + "testPuck" + fit + "#" +ss.uuid())
          //println(resdir.getAbsolutePath)
          val sf = s.applyChangeAndMakeExample(r.redo(s.graph),resdir, cm)
        //Quick.dot(sf.graph, resdir + File.separator + "nano.dot", Some(constraints))
        //println("path: "+resdir + File.separator + "nano.dot")
        //          println("Graphs equality? "+Mapping.equals(sf.graph, s.graph))

      }
    }

  def printResult[T](res : Iterable[SearchState[DecoratedGraph[T]]],
                     ordering : AStarSearchOrdering[DecoratedGraph[T]],
                     fullName2id : Map[String, NodeId],
                     filePaths : String*) : Unit =

    if (res.isEmpty) println("no results")
    else {
      println(res.size + " result(s)")
      res foreach {
        ss =>
          val fit = ordering.evaluateWithDepthPenalty(ss)
          TestUtils.printSuccessState(outDir, "result#" + fit + "#" +ss.uuid(), ss)
          val \/-(dg) = ss.loggedResult.value
          val result = solsDir + File.separator +"result#" + fit + "#" + ss.uuid() + ".pck"
          Recording.write(result, fullName2id, dg.graph)

          val s = new ScenarioFactory(filePaths:_*)
          val r = Recording.load(s"$result", s.fullName2id)(s.logger)
          import Recording.RecordingOps
          val resdir = new File( srcDir + File.separator + "testPuck" + fit + "#" +ss.uuid())
          //println(resdir.getAbsolutePath)
          val sf = s.applyChangeAndMakeExample(r.redo(s.graph),resdir)
        //Quick.dot(sf.graph, resdir + File.separator + "nano.dot", Some(constraints))
        //println("path: "+resdir + File.separator + "nano.dot")
        //          println("Graphs equality? "+Mapping.equals(sf.graph, s.graph))

      }
    }


}

object PrintResults {

  def printListRes[T](res: ListBuffer[SearchState[DecoratedGraph[T]]]): Unit = {
    res map (printResDeco(_))
    ()
  }

  def printRes[T](ss: SearchState[DecoratedGraph[T]]): String  = {
    printTags(ss)

  }

  def printTags[T](ss: SearchState[DecoratedGraph[T]]): String  = {
    val v1 = ss.loggedResult map (x =>  x._3)
    val v2 = ss.prevState map (printTags(_))
    (v1,v2) match {
      case (LoggedSuccess(_,s1),Some(s2)) => s2+s1
      case (LoggedSuccess(_,s1),None) => s1
      case (_,_) => ""
    }
  }

  /*
    def printTags[T](ss: SearchState[DecoratedGraph[T]]): Unit  = {

    ss.loggedResult map (x => print(x._3))
    ss.prevState map (printTags(_))
    ()
  }
*/

  def printResDeco[T](ss: SearchState[DecoratedGraph[T]]) : Unit = {
    println ("Solution:")
    print(printRes(ss))
    println()
  }
}