package puck.graph.transformations

import puck.graph.{DependencyGraph, _}


/**
  * Created by Loïc Girault on 24/08/16.
  */

sealed abstract class Mutability {
  def opposite : Mutability
}
case object Mutable extends Mutability {
  override val toString = "mutable"
  def opposite = Immutable
}
case object Immutable extends Mutability {
  override val toString = "immutable"
  def opposite = Mutable
}

object MutabilitySet {

  type T = Set[NodeId]

  def allMutable(graph : DependencyGraph) : T = Set[NodeId]().setMutability(graph.nodesId, Mutable)

  // changed by Cedric and Mikal
  // def allImmutable(graph : DependencyGraph) : T = Set[NodeId]()
  def allImmutable(graph : DependencyGraph) : T = Set[NodeId]().setMutability(graph.nodesId, Immutable)

  def setMutableWithName
  (graph : DependencyGraph, mutableSet : T,
   nodeId : NodeId, name : Seq[String], m : Mutability) : T =
    name match {
      case Nil => mutableSet
      case hd +: tl =>
        graph.content(nodeId) find (graph.getConcreteNode(_).name == hd) match {
          case None => mutableSet
          case Some(nid) =>
            setMutableWithName(graph, mutableSet.setMutability(nid, m), nid, tl, m)
        }
    }



  implicit class MutabilitySetOps(val immutables: T) extends AnyVal {

    def setMutability(graph : DependencyGraph, name : Seq[String], m : Mutability) : T =
      MutabilitySet.setMutableWithName(graph, immutables, graph.rootId, name, m)

    def setMutability(id : NodeId, v : Mutability) : T = v match {
      case Immutable => immutables + id
      case Mutable => immutables - id
    }

    def setMutability(ids : Iterable[NodeId], v : Mutability) : T = v match {
      case Immutable => immutables ++ ids
      case Mutable => immutables -- ids
    }

    def mutableNodes(graph: DependencyGraph) : T = graph.nodesId.toSet -- immutables
    def immutableNodes(graph: DependencyGraph) : T = immutables

    def toggle(id : NodeId): T =
      setMutability(id, mutability(id).opposite)


    def isImmutable : NodeId => Boolean = immutables.contains
    def isMutable : NodeId => Boolean = id => !isImmutable(id)

    def mutability(id : NodeId) : Mutability =
      if(isImmutable(id)) Immutable
      else Mutable

  }

}