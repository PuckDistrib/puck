/*
 * Puck is a dependency analysis and refactoring tool.
 * Copyright (C) 2016 Loïc Girault loic.girault@gmail.com
 *               2016 Mikal Ziane  mikal.ziane@lip6.fr
 *               2016 Cédric Besse cedric.besse@lip6.fr
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License published
 * by the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 *   Additional Terms.
 * Author attributions in that material or in the Appropriate Legal
 * Notices displayed by works containing it is required.
 *
 * Author of this file : Loïc Girault
 */

package puck.graph

import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.{Comment, Transformation}
import puck.util.UnionFind


object Metrics {

  // ajouté par Mikal
  // nombre de noeuds contenus dans root
  def numSons(graph: DependencyGraph, root : NodeId) =
    graph.content(root).size

  // ajouté par Mikal
  def numViolations(graph : DependencyGraph, cm : ConstraintsMaps) = {
    (cm forbiddenDependencies graph).size
  }

  def fitness0(graph : DependencyGraph) = graph.numNodes

  // ajouté par Mikal & Cédric
  def fitness1(graph : DependencyGraph, cm : ConstraintsMaps, kViols : Int = 10, kComplex : Int = 1) = {

    val gr = graph.recording
    val grfc = gr.filter(_.toString.contains("Comment")).filter(_.toString.contains("Abstract.createAbstraction")).filter(_.toString.contains("Class, SupertypeAbstraction"))

    val s = grfc.map {
      case _ => 1
    } sum


    //println(s + kViols*numViolations(graph, cm) + kComplex*graph.numNodes)

    s + kViols*numViolations(graph, cm) + kComplex*graph.numNodes
  }

  // ajouté par Cédric

  def fitness2(graph : DependencyGraph, nodesSet: Set[NodeId]): Int = {
    val filtered = graph.edges.typeMemberUses2typeUsesMap.content.filter{
      case ((_, used), _) => nodesSet contains used
    }
    filtered map {
      case (_, typeUses) => typeUses.size
    } sum
  }

  //prerequis : \forall n in nodes, graph.contains*(root, n)
  def outgoingDependencies(graph: DependencyGraph, root : NodeId, nodes: Seq[NodeId]): Set[NodeIdP] =
    nodes.foldLeft(Set[NodeIdP]()) {
      (acc0, id) => graph.usedBy(id).foldLeft(acc0) {
        (acc, used) =>
          if (graph.contains_*(root, used)) acc
          else acc + ((id, used))
      }
    }

  def outgoingDependencies(graph: DependencyGraph, root: NodeId): Set[NodeIdP] =
    outgoingDependencies(graph, root, graph.subTree(root, includeRoot = true))
  //prerequis : \forall n in nodes, graph.contains*(root, n)
  def incomingDependencies(graph: DependencyGraph, root: NodeId, nodes: Seq[NodeId]): Set[NodeIdP] =
    nodes.foldLeft(Set[NodeIdP]()) {
      (acc0, id) => graph.usersOf(id).foldLeft(acc0) {
        (acc, user) =>
          if (graph.contains_*(root, user)) acc
          else acc + ((user, id))
      }
    }

  def incomingDependencies(graph: DependencyGraph, root: NodeId): Set[NodeIdP] =
    incomingDependencies(graph, root, graph.subTree(root, includeRoot = true))
  //prerequis : \forall n in nodes, graph.contains*(root, n)
  def internalDependencies(graph: DependencyGraph, root: NodeId, nodes: Seq[NodeId]): Set[NodeIdP] =
    nodes.foldLeft(Set[NodeIdP]()){
      (acc0, id) => graph.usedBy(id).foldLeft(acc0) {
        (acc, used) =>
          if (graph.contains_*(root, used)) acc + ((id, used))
          else acc
      }
    }

  def internalDependencies(graph: DependencyGraph, root: NodeId): Set[NodeIdP] =
    internalDependencies(graph, root, graph.subTree(root, includeRoot = true))
  //prerequis : \forall n in nodes, graph.contains*(root, n)
  def outgoingAndInternalDependencies(graph: DependencyGraph,
                                root: NodeId, nodes: Seq[NodeId]
                               ) : (Set[NodeIdP], Set[NodeIdP]) =
    nodes.foldLeft((Set[NodeIdP](), Set[NodeIdP]())) {
      (acc, id) => graph.usedBy(id).foldLeft(acc) {
        case ((out, internals), used) =>
          if (graph.contains_*(root, used)) (out, internals + ((id, used)))
          else (out + ((id, used)), internals)
    }
  }

  def provides(graph: DependencyGraph, provider: NodeId, other: NodeId) : Boolean =
    provides(graph, graph.subTree(provider), other, graph.subTree(other))


  def provides(graph: DependencyGraph,
                these: Seq[NodeId],
                othersRoot : NodeId, others: Seq[NodeId]) : Boolean =
    these.exists { t =>
      others.exists { o =>
        graph.uses(o, t) &&
          !(graph.contains_*(othersRoot, o) && graph.contains_*(othersRoot, t))
      }
    }


  private def connection(graph: DependencyGraph, node: DGNode, p: DGNode => Boolean) =
    graph.nodes.foldLeft(Set[NodeId]()) { (acc, n) =>
      if (n.id == node.id || n.kind.kindType != node.kind.kindType) acc
      else if (p(n)) acc + n.id
      else acc
    }


  def providers(graph: DependencyGraph, id: NodeId): Set[NodeId] =
    connection(graph, graph.getNode(id), n => provides(graph, n.id, id))

  def clients(graph: DependencyGraph, id: NodeId): Set[NodeId] =
    connection(graph, graph.getNode(id), n => provides(graph, id, n.id))


  def providersAndClients(graph: DependencyGraph, id : NodeId) : (Set[NodeId], Set[NodeId]) = {
    val thisNode = graph.getNode(id)
    val thisSubtree = graph.subTree(id)
    graph.nodes.foldLeft((Set[NodeId](), Set[NodeId]())) {
      case (acc @ (ps, cs), otherNode) =>
      if (otherNode.id == thisNode.id || otherNode.kind.kindType != thisNode.kind.kindType) acc
      else {
        val otherSubtree = graph.subTree(otherNode.id)
        if (provides(graph, otherSubtree, thisNode.id, thisSubtree)) (ps + otherNode.id, cs)
        else if (provides(graph, thisSubtree, otherNode.id, otherSubtree)) (ps, cs + otherNode.id)
        else acc
      }

    }
  }

  def relativeCohesion(graph: DependencyGraph, userTree: NodeId, candidate: NodeId): Double = {
    val usedElts = outgoingDependencies(graph, userTree).map(_.used)
    relativeCohesion(graph, usedElts, candidate)
  }

  def relativeCohesion(graph: DependencyGraph, usedElts: Set[NodeId], candidate: NodeId): Double = {
    val subTree = graph.subTree(candidate).toSet
    usedElts.intersect(subTree).size.toDouble / usedElts.union(subTree).size
  }

  def cohesion(graph: DependencyGraph, root: NodeId): Double =
    cohesion(internalDependencies(graph, root).size,
      outgoingDependencies(graph, root).size,
      incomingDependencies(graph, root).size)

  def cohesion(internalDcies : Int, outgoingDcies : Int, incomingDcies : Int): Double =
    internalDcies.toDouble / (outgoingDcies + incomingDcies + internalDcies).toDouble


  def coupling(graph: DependencyGraph, root: NodeId): Double =
    coupling0(providers(graph, root).size, clients(graph, root).size,
      internalDependencies(graph, root).size,
      outgoingDependencies(graph, root).size,
      incomingDependencies(graph, root).size)
  
  def coupling0(providers : Int, clients : Int,
                internalDcies : Int,
                outgoingDcies : Int,
                incomingDcies : Int): Double =
    1 - (providers + clients ).toDouble / (outgoingDcies + incomingDcies + internalDcies).toDouble


  def nameSpaceCoupling(graph : DependencyGraph) =
    graph.concreteNodes.foldLeft(0 : Double){
      (acc, n) => n.kind.kindType match {
        case NameSpace =>
          val c = Metrics.coupling(graph, n.id)
          if(c.isNaN) acc
          else acc + c
        case _ => acc
      }}



  def typeWeight(g: DependencyGraph, typeId: NodeId): Double = {
    val t = g.getNode(typeId)


    def usesConnectedComponent
    (border: Set[NodeId],
     component: Set[NodeId],
     unreached: Set[NodeId]
      ): (Set[NodeId], Set[NodeId]) =
      if (border.isEmpty) (component, unreached)
      else {
        val n = border.head
        val neighbors = (g.usedByExcludingTypeUse(n) ++ g.usersOfExcludingTypeUse(n) - n) intersect unreached

        //        println(s" n = ${g.getNode(n)}\n neighbors = ${neighbors.map(g.getNode)}")

        val (newBorder, newUnreached) =
          neighbors.foldLeft((border - n, unreached)) {
            case ((accBorder, accUnreached), neighbor) =>
              if (component contains neighbor) (accBorder, accUnreached)
              else (accBorder + neighbor, unreached - neighbor)
          }
        usesConnectedComponent(newBorder, component + n, newUnreached)
      }

    def components(i: Int, unreached: Set[NodeId]): Int =
      if (unreached.isEmpty) i
      else {
        val n = unreached.head
        val (_, newUnreached) = usesConnectedComponent(Set(n), Set(), unreached - n)
        components(i + 1, newUnreached)
      }

    //    println("######################################################")
    //    println("######################################################")
    //    println("######################################################")

    val content = g.content(typeId)
    val s = Math.max(1, content.size)
    components(0, content).toDouble / s.toDouble
  }



  def numberOfCycles(edges : Set[NodeIdP]) : Int = {
    val nodes = edges.foldLeft(Set[NodeId]()){
      case (acc, (s,t)) => acc + s + t
    }

    val sortedEdges = edges.groupBy(_.user)
    var components = List[List[Int]]()

    //tarjan algorithm cf https://en.wikipedia.org/wiki/Tarjan%27s_strongly_connected_components_algorithm

    var index : Int= 0
    var indexes = Map[NodeId, Int]()
    var lowLinks = Map[NodeId, Int]()
    var s = List[Int]()


    def strongConnect(nid : NodeId) : Unit = {
      indexes += (nid -> index)
      lowLinks += (nid -> index)
      index += 1
      s ::= nid

      sortedEdges.get(nid) match {
        case None => ()
        case Some(es) =>
          es.foreach{
            e =>
              if(!(indexes isDefinedAt e.used)) {
                strongConnect(e.used)
                val newLowLink = Math.min(lowLinks(nid), lowLinks(e.used))
                lowLinks += (nid -> newLowLink)
              }
              else if(s contains e.used){
                val newLowLink = Math.min(lowLinks(nid), indexes(e.used))
                lowLinks += (nid -> newLowLink)
              }
          }
        if(lowLinks(nid) == indexes(nid)){
          var w : Int = -1
          var c = List[Int]()
          do {
            w = s.head
            s = s.tail
            c ::= w
          } while(nid != w)
          components ::= c
        }
      }

    }

    nodes.foreach {
      nid =>
        if(!(indexes isDefinedAt nid))
          strongConnect(nid)
    }

    components.size
  }

  def averageI(ds : Seq[Int]) : Double =
    if(ds.isEmpty) 0
    else ds.sum.toDouble / ds.size

  def averageD(ds : Seq[Double]) : Double =
    if(ds.isEmpty) 0
    else ds.sum / ds.size

  def apply_metric_on_types[T]
  (metric : (DependencyGraph, NodeId) => T,
   g : DependencyGraph,
   ignoresPrefix : Seq[String]): Seq[(String,T)] = {
    val types = g.nodes filter (_.kind.kindType == TypeDecl)

    for{
      t <- types.toSeq
      fname = g fullName t.id
      if ignoresPrefix forall (p => !(fname startsWith p))
    } yield (fname, metric(g, t.id))

  }
  // define in "A Metrics Suite for Object Oriented Design" - Chidamber & Kemerer - TSE 1994
  def LCOM(g : DependencyGraph, tid : NodeId): Int = {
    val m = DependencyGraph.splitByKind (g, g.content(tid).toSeq)
    val fields =  m.getOrElse("Field", Seq())

    val methods =
      m.getOrElse("StaticMethod", Seq()) ++
      m.getOrElse("AbstractMethod", Seq()) ++
      m.getOrElse("Constructor", Seq()) ++
      m.getOrElse("Method", Seq())

    val fs = fields.toSet

    def usedFields(m : NodeId) :Set[NodeId] =
      g definitionOf m map (mdef => g.usedBy(mdef)  intersect fs) getOrElse Set()
    val indexedIs = (methods map usedFields).zipWithIndex // cf bellow comment for the use of index
    var p = 0
    var q = 0

    for {
      (i, idxI) <- indexedIs
      (j, idxJ) <- indexedIs
      if idxI != idxJ //cannot use !( i eq j ) because
    // "different instances" of empty set are merged
    } {
      if((i intersect j).isEmpty)
        p = p + 1
      else
        q = q + 1
    }

    if(p > q) p - q
    else 0
  }

  // define in "A Metrics Suite for Object Oriented Design" - Chidamber & Kemerer - TSE 1994
  def CBO(g : DependencyGraph, tid :NodeId) : Int ={
    val (ps, cs) = providersAndClients(g, tid)
    val duplicate = ps intersect cs
    ps.size + cs.size - duplicate.size
  }

  // Hitz & Montazeri - Measuring Coupling and cohesion in object oriented system
  def LCOM4(g : DependencyGraph, tid : NodeId): Int = {
    val childrens = g.content(tid)


    val roots = childrens filter (c => g.usersOf(c).forall(!g.contains_*(tid, _)) )


    val uf = new UnionFind(childrens)

    def dfs(n : NodeId, usedByUsers : Set[NodeId]) : Unit =
      if(!(usedByUsers contains n))//prevent uses cycles
      g.definitionOf(n) foreach {
        ndef =>
        val usedBy = g.usedBy(ndef)
          usedBy.foreach {
          used =>
            if(childrens contains used) {
              if (!uf.find(n, used))
                uf.union(n, used)

              dfs(used, usedBy ++ usedByUsers)
            }
        }
      }

    roots foreach (dfs(_, Set()))

    uf.size
  }

  //LCOM Brian Henderson-Sellers - Object-Oriented Metrics: Measures of Complexity - Prentice Hall 1996
  def LCOM_hs(g : DependencyGraph, tid : NodeId): Double = {
    val m = DependencyGraph.splitByKind (g, g.content(tid).toSeq)
    val fields =  m.getOrElse("Field", Seq())

    val methods =
      m.getOrElse("StaticMethod", Seq()) ++
        m.getOrElse("AbstractMethod", Seq()) ++
        m.getOrElse("Constructor", Seq()) ++
        m.getOrElse("Method", Seq())

    val ms = methods.toSet

    def usersOf(f : NodeId) : Int = (g.usersOf(f) intersect ms).size

    val numMethods = methods.size
    (((fields map usersOf).sum.toDouble / fields.size) - numMethods) / ( 1 - numMethods)

  }




}

