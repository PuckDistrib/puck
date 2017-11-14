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

import puck.graph.DependencyGraph.AbstractionMap
import puck.graph.comparison.RecordingComparatorControl
import puck.graph.constraints.{Constraint, ConstraintsMaps, NamedRangeSet}
import puck.search.{DepthFirstSearchStrategy, SearchEngine}
import puck.util.{PuckLog, PuckLogger, PuckNoopLogger}
import puck.graph.transformations.Transformation
import puck.graph.transformations.Recording.RecordingOps
import puck.graph.transformations.Mutability
import MutabilitySet.MutabilitySetOps

object DependencyGraph {

  val rootId : NodeId = 0
  val dummyId = Int.MinValue

  val rootName = "root"
  val unrootedStringId = "<DETACHED>"
  val scopeSeparator : String = "."

  val definitionName : String = "Definition"

  type AbstractionMap = SetValueMap[NodeId, Abstraction]
  val AbstractionMap = SetValueMap


  def areEquivalent[Kind <: NodeKind, T](initialRecord : Seq[Transformation],
                                         graph1 : DependencyGraph,
                                         graph2 : DependencyGraph,
                                         logger : PuckLogger = PuckNoopLogger) : Boolean = {
    val recordingComparatorControl =
      new RecordingComparatorControl(initialRecord, graph1, graph2, logger)

    val engine =
      new SearchEngine(new DepthFirstSearchStrategy,
        recordingComparatorControl, maxResults = Some(1))
    engine.explore()
    engine.results.nonEmpty
  }

  def subGraph(fullGraph : DependencyGraph,
               focus : Set[NodeId]): DependencyGraph = {
    val kw = fullGraph.nodeKindKnowledge
    val g0 = DependencyGraph(kw, NodeIndex(kw.root), EdgeMap(),
      AbstractionMap(), Recording(), Set())

    focus.foldLeft(g0){
      case (g, id) =>
        val g1 = g.addConcreteNode(fullGraph.getConcreteNode(id))
        val path = fullGraph.containerPath(id)
        val g2 = path.foldLeft(g1)( (g,id) => g.addConcreteNode(fullGraph.getConcreteNode(id)))
        path.tail.foldLeft((g2, path.head)){
          case ((g, cter), cted) =>
            (g.addContains(cter,cted), cted)
        }._1
    }
  }


  def findElementByName(g : DependencyGraph, fn : String) : Option[ConcreteNode] = {
    val splitted = fn split s"\\${DependencyGraph.scopeSeparator}"

    import ShowDG._
    def aux(names : List[String], current : Option[NodeId]) : Option[ConcreteNode] =
      (names, current) match {
        case (_, None) => None
        case (Nil, Some(id)) => Some(g getConcreteNode id)
        case (hd :: tl, Some(id)) => aux(tl,
          g content id find ( cid => (g, g.getNode(cid)).shows(desambiguatedLocalName) == hd))
      }

    aux(splitted.toList, Some(rootId))
  }

  def splitByKind(g : DependencyGraph, ns: Seq[NodeId]) : Map[String, Seq[NodeId]] = {
    def add(m: Map[String, Seq[NodeId]], key: String, n: NodeId) = {
      val newVal = n +: m.getOrElse(key, Seq())
      m + (key -> newVal)
    }
    ns.foldLeft( Map[String, Seq[NodeId]]() ){
      case (m, n) =>
        val kind = g.getNode(n).kind
        add(m, kind.toString, n)
    }
  }

  private var idSeed : Int = 0

  def apply( nodeKindKnowledge: NodeKindKnowledge,
             nodesIndex : NodeIndex,
             edges : EdgeMap,
             abstractionsMap : AbstractionMap,
             recording : Recording,
             mutabilitySet: MutabilitySet) : DependencyGraph ={

    idSeed +=1
    new DependencyGraph(idSeed,
      nodeKindKnowledge, nodesIndex,
      edges, abstractionsMap,
      recording,
      mutabilitySet)
  }

}


class DependencyGraph private
(val id : Int,
 val nodeKindKnowledge : NodeKindKnowledge,
 val nodesIndex : NodeIndex,
 val edges : EdgeMap,
 val abstractionsMap : AbstractionMap,
 val recording : Recording,
 val mutabilitySet: MutabilitySet) {

  override def hashCode(): NodeId = id

  def newGraph(nodesIndex : NodeIndex = nodesIndex,
               edges : EdgeMap = edges,
               abstractionsMap : AbstractionMap = abstractionsMap,
               recording : Recording = recording,
               mutabilitySet: MutabilitySet = mutabilitySet) : DependencyGraph =
    DependencyGraph(
      nodeKindKnowledge,
      nodesIndex, edges,
      abstractionsMap, recording, mutabilitySet)

  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.InGraph, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) : PuckLog.Verbosity =
    (PuckLog.InGraph, lvl)


  def setMutability(nodeId: NodeId, mutability: Mutability) =
    newGraph(mutabilitySet = mutabilitySet.setMutability(nodeId, mutability))
  def setMutability(nodeId: Iterable[NodeId], mutability: Mutability) =
    newGraph(mutabilitySet = mutabilitySet.setMutability(nodeId, mutability))

  def mutableNodes : Set[NodeId] = mutabilitySet.mutableNodes(this)

  def immutableNodes : Set[NodeId] = mutabilitySet.immutableNodes(this) // added by Cedric

  def isMutable(n : NodeId) : Boolean = mutabilitySet isMutable n

  def comment(msg : String) = newGraph(recording = recording.comment(msg))

  def constraintChange
  ( constraintsMaps : ConstraintsMaps) : DependencyGraph =
    newGraph(recording = recording.constraintChange(constraintsMaps))

  def constraintChange
  (namedSets : Map[String, NamedRangeSet],
   friendConstraints : List[Constraint],
   hideConstraints : List[Constraint]) : DependencyGraph  =
    newGraph(recording = recording.constraintChange(namedSets, friendConstraints, hideConstraints))

  def mileStone = newGraph(recording = recording.mileStone)

  val rootId : NodeId = 0
  def root : ConcreteNode = getConcreteNode(rootId)
  def isRoot(id : NodeId) = id == rootId


  private [graph] def addConcreteNode(n : ConcreteNode) : DependencyGraph =
    newGraph(nodesIndex = nodesIndex.addConcreteNode(n),
      recording = recording.addConcreteNode(n))



  def addConcreteNode
  ( localName : String,
    kind : NodeKind
  ) : (ConcreteNode, DependencyGraph) = {
    val(n, nIndex) = nodesIndex.addConcreteNode(localName, kind)
    (n, newGraph(nodesIndex = nIndex,
      recording = recording.addConcreteNode(n)))
  }

  private [graph] def addVirtualNode
  ( n : VirtualNode ) : DependencyGraph =
    newGraph(nodesIndex = nodesIndex.addVirtualNode(n),
      recording = recording.addVirtualNode(n))


  def addVirtualNode(ns : Set[NodeId], k : NodeKind) : (VirtualNode, DependencyGraph) = {
    val (vn, nIndex) = nodesIndex.addVirtualNode(ns, k)
    (vn, newGraph(nodesIndex = nIndex,
      recording = recording.addVirtualNode(vn)))
  }

  def nodes : Iterable[DGNode] = nodesIndex.nodes
  def concreteNodes : Iterable[ConcreteNode] = nodesIndex.concreteNodes
  def virtualNodes : Iterable[VirtualNode] = nodesIndex.virtualNodes

  def nodesId : Iterable[NodeId] = nodesIndex.nodesId

  def concreteNodesId : Iterable[NodeId] = nodesIndex.concreteNodesId

  def numNodes : Int = nodesIndex.numNodes
  def numRemovedNodes : Int = nodesIndex.numRemovedNodes

  def getNode(id : NodeId): DGNode = nodesIndex.getNode(id)

  def getConcreteNode(id : NodeId): ConcreteNode =
    nodesIndex.getConcreteNode(id)


  def removeConcreteNode(n : ConcreteNode) : DependencyGraph =
    newGraph(nodesIndex = nodesIndex removeConcreteNode n,
      recording = recording removeConcreteNode n)


  def removeVirtualNode(n : VirtualNode) : DependencyGraph =
    newGraph(nodesIndex = nodesIndex removeVirtualNode n,
      recording = recording removeVirtualNode n)


  def removeNode(id: NodeId) : (DGNode, DependencyGraph) = {
    getNode(id) match {
      case vn : VirtualNode => (vn, removeVirtualNode(vn))
      case cn : ConcreteNode => (cn, removeConcreteNode(cn))
    }
  }

  def setName(id : NodeId, newName : String) : DependencyGraph = {
    val (oldName, index) = nodesIndex.setName(id, newName)
    newGraph( nodesIndex = index,
      recording = recording.changeNodeName(id, oldName, newName))
  }

  def setRole(id : NodeId, srole : Option[Role]) : DependencyGraph = {
    newGraph(nodesIndex = nodesIndex.setRole(id, srole),
      recording = recording.addRoleChange(id, getRole(id), srole))
  }
  def getRole(id: NodeId) : Option[Role] = nodesIndex.getRole(id)

  def styp(id : NodeId) : Option[Type] = edges.types get id
  def typ(id : NodeId) : Type =
    try edges.types(id)
    catch {
      case e : NoSuchElementException =>
        import ShowDG._
        val nse = new NoSuchElementException(e.getMessage + " - " + (this, id).shows(desambiguatedFullName))
        nse.setStackTrace(e.getStackTrace)
        throw nse
    }


  def structuredType(id : NodeId) : Option[Type] = {
    //assert node is a typed value
    nodeKindKnowledge.structuredType(this, id, parametersOf(id))
  }

  def instanceValuesWithType(typeId : NodeId) : List[TypedNode] =
    for {
      m <- (this content typeId).toList map getConcreteNode
      if m.kind.kindType == InstanceValue
    } yield (m, this typ m.id)


  def addType(id : NodeId, t : Type) : DependencyGraph =
    newGraph(edges = edges.setType(id, t),
      recording = recording.addType(id, t))

  def removeType(id : NodeId) : DependencyGraph =
    edges.types get id map {
      t => newGraph(edges = edges.removeType(id),
        recording = recording.removeType(id, t))
    } getOrElse this

  def changeType(tu : NodeIdP, newTypeToUse : NodeId) : DependencyGraph =
    tu match {
      case  (typed, oldType) =>
        //val newType = typ(typed).changeNamedType(oldType, newTypeToUse)
        newGraph(edges = edges.redirectTypeUse(typed, oldType, newTypeToUse),
          recording = recording.changeType(typed, oldType, newTypeToUse))
    }


  def exists(e : DGEdge) : Boolean = edges exists e

  def addEdge(e : DGEdge, register : Boolean = true): DependencyGraph =
    newGraph(edges = edges.add(e),
      recording =
        if (register) recording.addEdge(e)
        else recording)


  def removeEdge(e : DGEdge, register : Boolean = true): DependencyGraph =
    newGraph( edges = edges.remove(e),
      recording =
        if(register) recording.removeEdge(e)
        else recording)

  def typedBy(tid : NodeId) : List[NodeId] = edges.typedBy(tid)

  def addContains(containerId: NodeId, contentId : NodeId, register : Boolean = true): DependencyGraph = {
    getNode(contentId).kind.kindType match {
      case Parameter | TypeVariableKT =>
        addEdge(ContainsParam(containerId, contentId), register)
      case _ => addEdge(Contains(containerId, contentId), register)
    }
  }


  def logComment(msg : String) : LoggedTG =
    LoggedSuccess(msg, comment(msg))

  def canContain(container : DGNode, content : ConcreteNode): Boolean =
    nodeKindKnowledge.canContain(this, container, content)

  def canContain(container : DGNode, contentKind : NodeKind): Boolean =
    nodeKindKnowledge.canContain(this, container, contentKind)

  def canBe(sub : DGNode, sup : ConcreteNode): Boolean =
    mutabilitySet.isMutable(sub.id) &&
      nodeKindKnowledge.canBe(this, sub, sup)

  def removeContains(containerId: NodeId, contentId :NodeId, register : Boolean = true): DependencyGraph =
    removeEdge(Contains(containerId, contentId), register)

  def addUses(userId: NodeId, useeId: NodeId,
              register : Boolean = true): DependencyGraph =
    addEdge(Uses(userId, useeId))


  def addIsa(subType: Type, superType: Type, register : Boolean = true) : DependencyGraph =
    newGraph(edges = edges.addIsa(subType, superType),
      recording =
        if (register) recording.addIsa(subType, superType)
        else recording)

  def removeIsa(subType: Type, superType: Type, register : Boolean = true) : DependencyGraph =
    newGraph(edges = edges.removeIsa(subType, superType),
      recording =
        if (register) recording.removeIsa(subType, superType)
        else recording)



  def addBinding
  ( typeUse : NodeIdP,
    typeMemberUse : NodeIdP) : DependencyGraph =
    newGraph(edges = edges.addUsesDependency(typeUse, typeMemberUse),
      recording = recording.addTypeBinding(typeUse, typeMemberUse))

  def addAccessKind(br : (NodeIdP, NodeIdP),
                    accK : UsesAccessKind) =
    newGraph(edges = edges.changeAccessKind(br, Some(accK)),
      recording = recording.addAccessKind(br, accK))


  def rmAccessKind(br : (NodeIdP, NodeIdP)) =
    edges.getAccessKind(br) match {
      case Some(accK) =>
        newGraph(edges = edges.changeAccessKind(br, None),
          recording = recording.removeAccessKind(br, accK))
      case None => this
    }



  def removeBinding
  ( typeUse : NodeIdP,
    typeMemberUse : NodeIdP) : DependencyGraph =
    newGraph(edges = edges.removeUsesDependency(typeUse, typeMemberUse),
      recording = recording.removeTypeBinding(typeUse, typeMemberUse))

  def addTypeConstraint(constraint : TypeConstraint) : DependencyGraph =
    newGraph(edges = edges addTypeConstraint constraint,
      recording = recording addTypeConstraint constraint)

  def removeTypeConstraint(constraint : TypeConstraint) : DependencyGraph =
    newGraph(edges = edges removeTypeConstraint constraint,
      recording = recording removeTypeConstraint constraint)


  def changeTypeUseOfTypeMemberUse
  ( oldTypeUse : NodeIdP,
    newTypeUse : NodeIdP,
    tmUse : NodeIdP) : DependencyGraph =
    newGraph(edges =
      edges.removeUsesDependency(oldTypeUse, tmUse).
        addUsesDependency(newTypeUse, tmUse),
      recording = recording.changeTypeUseOfTypeMemberUse(oldTypeUse, newTypeUse, tmUse))


  def changeTypeUseForTypeMemberUseSet
  ( oldTypeUse : NodeIdP,
    newTypeUse : NodeIdP,
    tmus : Set[NodeIdP]) : DependencyGraph =
    tmus.foldLeft(this)(_.changeTypeUseOfTypeMemberUse(oldTypeUse, newTypeUse, _))


  def changeTypeMemberUseOfTypeUse
  ( oldTmUse : NodeIdP,
    newTmUse : NodeIdP,
    typeUse : NodeIdP) : DependencyGraph =
    newGraph(edges =
      edges.removeUsesDependency(typeUse, oldTmUse).
        addUsesDependency(typeUse, newTmUse),
      recording = recording.changeTypeMemberUseOfTypeUse(oldTmUse, newTmUse, typeUse))


  def changeTypeMemberUseOfTypeUseSet
  ( oldTmUse : NodeIdP,
    newTmUse : NodeIdP,
    tus : Set[NodeIdP]) : DependencyGraph =
    tus.foldLeft(this)(_.changeTypeMemberUseOfTypeUse(oldTmUse, newTmUse, _))


  private def isChangeType(edge : DGEdge, newTarget : NodeId) : Boolean =
    edge.kind == Uses && (getNode(edge.user).kind.kindType match {
      case InstanceValue
           | StableValue
           | Parameter =>
        val oldUsedKind = getNode(edge.used).kind.kindType
        val newUsedKind = getNode(newTarget).kind.kindType

        oldUsedKind == TypeDecl && newUsedKind == TypeDecl
      case kt => false
    })

  def splitUsesWithTargets
  (tu : NodeIdP,
   edge : NodeIdP,
   readTarget : NodeId,
   writeTarget : NodeId
  ) : DependencyGraph = {
    val u : DGEdge = Uses(edge)
    val g1 = removeEdge(u, register = false)
    val readEdge = Uses(edge.source, readTarget)
    val writeEdge = Uses(edge.source, writeTarget)

    val newRecording =
      recording.changeEdgeTarget(u, readTarget, withMerge = exists(readEdge))
        .changeEdgeTarget(u, readTarget, withMerge = exists(readEdge))

    g1.addEdge(readEdge, register = false)
      .addEdge(writeEdge, register = false)
      .newGraph(recording = newRecording)
      .removeBinding(tu, edge)
      .rmAccessKind((tu, edge))
      .addBinding(tu, readEdge)
      .addBinding(tu, writeEdge)

  }

  def changeTarget(edge : DGEdge, newTarget : NodeId) : DependencyGraph = {
    val newEdge : DGEdge = edge.copy(target = newTarget)
    val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = exists(newEdge))

    if (isChangeType(edge, newTarget))
      newGraph(
        edges = edges.redirectTypeUse(edge.user, edge.used, newTarget),
        recording = newRecording)
    else
      removeEdge(edge, register = false)
        .addEdge(newEdge, register = false)
        .newGraph(recording = newRecording)

  }


  def changeSource(edge : DGEdge, newSource : NodeId) : DependencyGraph = {
    val g1 = edge.deleteIn(this, register = false)
    val newEdge: DGEdge = edge.copy(source = newSource)
    val newRecording = recording.changeEdgeSource(edge, newSource, withMerge = exists(newEdge))
    newEdge.createIn(g1, register = false).newGraph(recording = newRecording)
  }

  /*
   * Read-only queries
   */

  def nodeKinds : List[NodeKind] = nodeKindKnowledge.nodeKinds

  def container(contentId : NodeId) : Option[NodeId] =
    edges.containers.get(contentId)

  def containerOfKindType(kt: KindType, nid : NodeId) : Option[NodeId] =
    getNode(nid).kind.kindType match {
      case `kt` => Some(nid)
      case _ => container(nid) flatMap (containerOfKindType(kt, _))
    }

  def container_!(contentId : NodeId) : NodeId =
    container(contentId) match {
      case Some(cterId) => cterId
      case None => error(getNode(contentId).name + " has no container")
    }

  def containerOfKindType_!(kt: KindType, nid : NodeId) : NodeId =
    getNode(nid).kind.kindType match {
      case `kt` => nid
      case _ => containerOfKindType_!(kt, container_!(nid))
    }

  def hostNameSpace(nid : NodeId) : NodeId =
    containerOfKindType_!(NameSpace, nid)

  def hostTypeDecl(nid : NodeId) : NodeId =
    containerOfKindType_!(TypeDecl, nid)

  def content(containerId: NodeId) : Set[NodeId] =
    edges.contents.getFlat(containerId) ++
      edges.parameters.getFlat(containerId)
  /*++
      (definitionOf(containerId) map (Set(_))).getOrElse(Set()) ++
      edges.parameters.getFlat(containerId)*/


  //special case alias for readibility
  def declarationOf(defId : NodeId) : NodeId =
  getNode(defId).kind.kindType match {
    case ValueDef => container_!(defId)
    case _ => defId
  }


  //special cases of content
  def definitionOf(declId : NodeId) : Option[NodeId] =
  edges.contents get declId flatMap { ctent =>
    val s : Set[NodeId] = ctent filter (id =>
      getNode(id).kind.kindType == ValueDef )

    //definition may be preceded by type variable decl with gen methods
    s.headOption
  }

  def definitionOf_!(declId : NodeId) : NodeId =
    definitionOf(declId).get

  def isMethodInHierarchy(n: ConcreteNode): Boolean = {
    if (n.kind.kindType == InstanceValue) {
      val mc = container(n.id)
      val sigMethod = signature(n)
      for (cn <- concreteNodes) {
        mc match {
          case Some(mcn) => if (isa_*(mcn,cn.id) || isa_*(cn.id, mcn)) {
            val ccn = content(cn.id)
            for (elcn <- ccn) {
              val node = getNode(elcn)
              if (node.kind.kindType == InstanceValue) {
                val sigNode = signature(node)
                if (sigNode == sigMethod) return true
              }
            }
          }
        }
      }
    }
    false
  }

  def signature(node: DGNode) = {
    node.kind.kindType.toString + '.' + node.name + '(' + parametersOf(node.id) + ')'
  }

  def parametersOf(declId : NodeId)  : List[NodeId] = edges.parameters.getFlat(declId)

  def nodePlusDefAndParams(nodeId : NodeId) : List[NodeId] =
    parametersOf(nodeId) ++ definitionOf(nodeId).toList :+ nodeId

  def contains(containerId : NodeId, contentId : NodeId) : Boolean =
    edges.contains(containerId, contentId)

  def containsList : List[(NodeId, NodeId)] = edges.contents.flatList


  //heavily used by the solver need to be optimized
  def contains_*(containerId : NodeId, contentId : NodeId) : Boolean =
  containerId == contentId || {
    var visited = Set[NodeId]()
    var scontent0 : Option[NodeId] = edges.containers get contentId
    while(scontent0.nonEmpty && scontent0.get != containerId){
      if(visited.contains(scontent0.get))
        puck.error("loop !" + getNode(scontent0.get).name )
      visited += scontent0.get
      scontent0 = edges.containers get scontent0.get
    }
    scontent0.nonEmpty
  }


  def containerPath(id : NodeId)  : Seq[NodeId] = {
    def aux(current : NodeId, acc : Seq[NodeId]) : Seq[NodeId] = {
      val cter = container(current)
      if (cter.isEmpty) current +: acc
      else aux(cter.get, current +: acc)
    }

    aux(id, Seq())
  }

  def depth(id : NodeId) : Int = {
    val cp = containerPath(id)
    if(cp.head == id) 0
    else cp.length
  }

  def fullName(id : NodeId) : String = {
    val path = containerPath(id).map{n => getNode(n).name(this)}

    (if (path.head == DependencyGraph.rootName) path.tail
    else
      DependencyGraph.unrootedStringId +: path ).mkString(DependencyGraph.scopeSeparator)
  }


  def directSuperTypes(sub: NodeId) : Set[Type] = edges.superTypes getFlat sub
  def directSubTypes(sup: NodeId) : Set[Type] = edges.subTypes getFlat sup

  def directSuperTypesId(sub: NodeId) : Set[NodeId] = this directSuperTypes sub map Type.mainId
  def directSubTypesId(sup: NodeId) : Set[NodeId] = this directSubTypes sup map Type.mainId

  def superTypes(sub : NodeId) : Set[NodeId]= {
    val dst = directSuperTypesId(sub)
    dst.foldLeft(dst) { _ ++ superTypes(_) }
  }

  def subTypes(sup : NodeId) : Set[NodeId]= {
    val dst = directSubTypesId(sup)
    dst.foldLeft(dst) {  _ ++ subTypes(_) }
  }


  def typeConstraints(typedNode : NodeId) : Set[TypeConstraint] =
    edges.typeConstraints getFlat typedNode

  def typeConstraintsIterator : Iterator[(NodeId, TypeConstraint)] =
    edges.typeConstraints.iterator



  def isa(sub : Type, sup: Type): Boolean = edges.isa(sub, sup)

  def isa_*(sub : Type, sup: Type): Boolean =  edges.isa_*(sub, sup)

  def isa(subId : NodeId, superId: NodeId): Boolean =
    edges.isa(subId, superId)

  def isa_*(subId : NodeId, superId: NodeId): Boolean =
    edges.isa_*(subId, superId)

  def isaList  : List[(NodeId, NodeId)] = edges.superTypes.flatList map (e => (e._1, Type.mainId(e._2)))

  def usesAccessKind(br: (NodeIdP, NodeIdP)) : Option[UsesAccessKind] =
    edges getAccessKind br

  def uses(userId: NodeId, usedId: NodeId) : Boolean =
    edges.uses(userId, usedId)

  def usesList : List[NodeIdP] = edges.allUsesList
  def typeUsesList : List[NodeIdP] = edges.typeUsesList
  def usesListExludingTypeUses : List[NodeIdP] = edges.usesListExludingTypeUses

  def usedByExcludingTypeUse(userId : NodeId) : Set[NodeId] =
    edges usedByExcludingTypeUse userId

  def usedBy(userId : NodeId) : Set[NodeId] =
    edges usedBy userId


  def usersOfExcludingTypeUse(usedId: NodeId) : Set[NodeId] =
    edges usersOfExcludingTypeUse usedId

  def usersOf(usedId: NodeId) : Set[NodeId] =
    edges usersOf usedId

  def typeUsesOf(typeMemberUse : NodeIdP) : Set[NodeIdP] =
    edges typeUsesOf typeMemberUse

  def typeMemberUsesOf(typeUse : NodeIdP) : Set[NodeIdP] =
    edges typeMemberUsesOf typeUse

  def typeUsesOf(tmUser : NodeId, tmUsed : NodeId) : Set[NodeIdP] =
    edges typeUsesOf (tmUser, tmUsed)

  def typeMemberUsesOf(typeUser : NodeId, typeUsed : NodeId) : Set[NodeIdP] =
    edges typeMemberUsesOf (typeUser, typeUsed)

  def typeMemberUses2typeUses : Seq[(NodeIdP, Set[NodeIdP])] =
    edges.typeMemberUses2typeUsesMap.toSeq

  def typeUses2typeMemberUses : Seq[(NodeIdP, Set[NodeIdP])] =
    edges.typeUses2typeMemberUsesMap.toSeq

  def bind(typeUse : NodeIdP,
           typeMemberUse : NodeIdP) : Boolean =
    typeMemberUsesOf( typeUse ) contains typeMemberUse

  def addAbstraction(id : NodeId, abs : Abstraction) : DependencyGraph =
    newGraph(abstractionsMap = abstractionsMap + (id, abs),
      recording = recording.addAbstraction(id, abs))

  def removeAbstraction(id : NodeId, abs : Abstraction) : DependencyGraph =
    newGraph(abstractionsMap = abstractionsMap - (id, abs),
      recording = recording.removeAbstraction(id, abs))

  def abstractions(id : NodeId) : Set[Abstraction] = {

    var visited : Set[Abstraction] = Set()
    var toVisit : Set[Abstraction] = abstractionsMap getFlat id

    while(toVisit.nonEmpty) {
      val newlyReached = (toVisit.head.nodes map abstractionsMap.getFlat).reduce(_ ++ _)
      visited = visited + toVisit.head
      toVisit = toVisit.tail ++ (newlyReached diff visited)
    }

    val target = getNode(id)
    def superTypeAbstractions : Set[AccessAbstraction] =
      (target.kind.kindType match {
        case TypeDecl =>
          superTypes(target.id)
        case InstanceValue =>
          Abstraction.typeMemberOverridenAbstractions(this, id)
        case _ => Set()
      }) map (AccessAbstraction(_, SupertypeAbstraction))

    visited ++ superTypeAbstractions
  }


  def isAbstraction(implId : NodeId, absId : NodeId, pol : AbstractionPolicy) : Boolean =
    isAbstraction(implId, absId).exists{_.policy == pol}

  def isAbstraction(implId : NodeId, absId : NodeId) : Option[Abstraction] =
    abstractions(implId) find {
      case AccessAbstraction(id, _) => id == absId
      case ReadWriteAbstraction(rId, wId) =>
        rId.contains(absId) || wId.contains(absId)
    }


  def subTree(root : NodeId, includeRoot : Boolean = true) : List[NodeId] = {

    def aux(acc : List[NodeId])( roots : List[NodeId]): List[NodeId] = roots match {
      case List() => acc
      case r +: tail =>
        val children = content(r)
        aux(children ++: acc)(children ++: tail)
    }

    val seqInit =
      if(includeRoot) List(root)
      else List()

    aux(seqInit)(List(root))
  }

  def kindType(nid : NodeId) : KindType =
    getNode(nid).kind.kindType

  def getDefaultConstructorOfType(typeId : NodeId) : Option[NodeId] =
    nodeKindKnowledge.getConstructorOfType(this, typeId)


  def typeVariableValue(tvid : NodeId, binder : NodeId) : Type = {
    val ParameterizedType(genType, tvs) =  typ(binder)
    tvs(parametersOf(genType).indexOf(tvid))
  }

  def typeVariableValue(tvid : NodeId, tmUse : NodeIdP) : Type = {
    val tuses = typeUsesOf(tmUse)
    if(tuses.size > 1) error()
    else typeVariableValue(tvid, tuses.head.user)
  }

  var constraintsMapsCache : Option[(ConstraintsMaps, Set[NodeIdP])] = None


}