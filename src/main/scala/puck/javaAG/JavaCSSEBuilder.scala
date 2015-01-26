package puck.javaAG

import puck.graph.constraints.search.{TryAllCSSE, FunneledCSSE, FindFirstCSSE}
import puck.graph._
import puck.graph.io.ConstraintSolvingSearchEngineBuilder
import puck.search.SearchEngine

/**
 * Created by lorilan on 12/09/14.
 */
//CSSE : Constraint Solving Search Engine
trait JavaCSSEBuilder
  extends ConstraintSolvingSearchEngineBuilder{
  val violationsKindPriority = JavaSolver.violationPrioritySeq
}

object JavaFindFirstCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "First solution"

  def apply(initialRecord : Recording, graph : DependencyGraph) : SearchEngine[ResultT] =
    new FindFirstCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}

object JavaFunneledCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Funneled"

  def apply(initialRecord : Recording, graph : DependencyGraph) : SearchEngine[ResultT] =
    new FunneledCSSE(initialRecord, violationsKindPriority, graph, JavaSolverBuilder)
}

object JavaTryAllCSSEBuilder
  extends JavaCSSEBuilder{

  override def toString = "Try all"

  def apply(initialRecord : Recording, graph : DependencyGraph) : SearchEngine[ResultT] =
    new TryAllCSSE(violationsKindPriority, graph, JavaSolverBuilder)
}