package puck.search

import puck.graph.{DecoratedGraph, LoggedSuccess}

import scala.collection.mutable.ListBuffer

object PrintResults {

  def printListRes[T](res: ListBuffer[SearchState[DecoratedGraph[T]]]): Unit = {
    res map (printResDeco(_))
    ()
  }

  def getTags[T](ss: SearchState[DecoratedGraph[T]]): String  = {
    val v1 = ss.loggedResult map (x =>  x._3)
    val v2 = ss.prevState map (getTags(_))
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
    print(ss.uuid()+": ")
    print(getTags(ss))
    println()
  }
}