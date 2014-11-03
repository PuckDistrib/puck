package puck.graph.immutable

import puck.graph.{RedirectionError, AGError}
import puck.graph.constraints._
import puck.graph.immutable.constraints._
import puck.graph.edgeToPair
import puck.util.{Logger, PuckLog, PuckNoopLogger, PuckLogger}
import puck.graph.immutable.transformations.Recording

import scala.language.existentials

object AccessGraph {

  val rootId = 0
  /*val dummyId = 0
  val dummyNamedType = NamedType(0, "DummyType")
  val dummyArrowType = Arrow(dummyNamedType, dummyNamedType)*/
  val rootName = "root"
  val unrootedStringId = "<DETACHED>"
  val scopeSeparator : String = "."

  //phantom type of NodeId used to ease Mutable/Immutable transition
  //in mutable NodeId[K] =:= AGNode[K]
  type NodeId[Kind <: NodeKind[Kind]] = Int
  type EdgeMap[Kind <: NodeKind[Kind]] = SetValueMap[NodeId[Kind], NodeId[Kind]]
  val EdgeMap = SetValueMap
  type UseDependencyMap[Kind <: NodeKind[Kind]] =
       SetValueMap[(NodeId[Kind],NodeId[Kind]), (NodeId[Kind],NodeId[Kind])]
  val UseDependencyMap = SetValueMap
  type AbstractionMap[Kind <: NodeKind[Kind]] =
       SetValueMap[NodeId[Kind], (NodeId[Kind], AbstractionPolicy)]
  val AbstractionMap = SetValueMap
  type Node2NodeMap[Kind <: NodeKind[Kind]] = Map[NodeId[Kind], NodeId[Kind]]
  val Node2NodeMap = Map

  type NodeT[K <: NodeKind[K], T] = (NodeId[K], String, K, TypeHolder[K], Mutability , T)
  type NodeIndex[Kind <: NodeKind[Kind], T] = Map[NodeId[Kind], NodeT[Kind, T]]
  val NodeIndex = Map

  implicit def idToNode[Kind <: NodeKind[Kind], T](implicit graph : AccessGraph[Kind, T],
                                                   id : NodeId[Kind]) : AGNode[Kind, T] =
               graph.getNode(id)

  type Mutability = Boolean

  //for compliace with mutable version
  type Breakpoint[Kind <: NodeKind[Kind], T] = AccessGraph[Kind, T]

}
import AccessGraph._


class AccessGraph[NK <: NodeKind[NK], T]
( private [this] val nodeBuilder : AGNodeBuilder[NK, T],
  val logger : PuckLogger = PuckNoopLogger,
  private [this] val idSeed : () => Int,
  private [this] val nodesIndex : NodeIndex[NK, T],
  private [this] val usersMap : EdgeMap[NK],
  private [this] val usesMap  : EdgeMap[NK],
  private [this] val contentsMap  : EdgeMap[NK],
  private [this] val containerMap : Node2NodeMap[NK],
  private [this] val superTypesMap : EdgeMap[NK],
  private [this] val subTypesMap : EdgeMap[NK],
  private [this] val dominantUsesMap : UseDependencyMap[NK],
  private [this] val dominatedUsesMap : UseDependencyMap[NK],
  private [this] val abstractionsMap : AbstractionMap[NK],
  private [this] val constraints : ConstraintsMaps[NK],
  val recording : Recording[NK, T]) {

  type NIdT = NodeId[NK]
  type NT = NodeT[NK, T]
  type EdgeT = AGEdge[NK]
  type GraphT = AccessGraph[NK, T]
  type STyp = TypeHolder[NK]

  def newGraph(nLogger : PuckLogger = logger,
               nNodesSet : NodeIndex[NK, T] = nodesIndex,
               nUsersMap : EdgeMap[NK] = usersMap,
               nUsesMap  : EdgeMap[NK] = usesMap,
               nContentMap  : EdgeMap[NK] = contentsMap,
               nContainerMap : Node2NodeMap[NK] = containerMap,
               nSuperTypesMap : EdgeMap[NK] = superTypesMap,
               nSubTypesMap : EdgeMap[NK] = subTypesMap,
               nDominantUsesMap : UseDependencyMap[NK] = dominantUsesMap,
               nDominatedUsesMap : UseDependencyMap[NK] = dominatedUsesMap,
               nAbstractionsMap : AbstractionMap[NK] = abstractionsMap,
               nConstraints : ConstraintsMaps[NK] = constraints,
               nRecording : Recording[NK, T] = recording) : AccessGraph[NK, T] =
    new AccessGraph[NK, T](nodeBuilder, nLogger,
                        idSeed,
                        nNodesSet, nUsersMap, nUsesMap,
                        nContentMap, nContainerMap, nSuperTypesMap, nSubTypesMap,
                        nDominantUsesMap, nDominatedUsesMap,
                        nAbstractionsMap, constraints, nRecording)

  def withLogger(l : PuckLogger) = newGraph(nLogger = l)
  implicit val defaulVerbosity : PuckLog.Verbosity =
    (PuckLog.InGraph, PuckLog.Debug)
  import scala.language.implicitConversions
  implicit def logVerbosity(lvl : PuckLog.Level) =
    (PuckLog.InGraph, lvl)


  private [immutable] def addNode(id : NIdT,
                                  localName:String,
                                  kind: NK,
                                  styp: STyp,
                                  mutable : Mutability,
                                  t : T) : AccessGraph[NK, T] =
    newGraph(nNodesSet = nodesIndex + (id -> (id, localName, kind, styp, mutable, t)),
             nRecording = recording.addNode(id, localName, kind, styp, mutable, t))



  val rootId : NIdT = 0
  def root = getNode(rootId)
  def isRoot(id : NIdT) = container(id) == id

  def addNode(localName:String, kind: NK, th : TypeHolder[NK], mutable : Mutability = true) : (NIdT, GraphT) = {
    val id = idSeed()
    (id, addNode(id, localName, kind, th, mutable, nodeBuilder.createT()))
  }

  /*def nodes : Seq[NodeIdT] = Range(0, idSeed + 1) */
  def nodes : Iterable[AGNode[NK, T]] = nodesIndex.values map {
    case (id, name, kind, styp, mutable, t) =>
      new AGNode(this, id, name, kind, styp, mutable, t)
  }

  private def sortedMap : Seq[(NIdT,  NT)] = nodesIndex.toSeq sortBy(_._1)

  def nodesId : Iterable[NodeId[NK]] = nodesIndex.keys

  def getNode(id : NIdT): AGNode[NK, T] = nodesIndex get id match {
    case None =>
      val msg = "AccessGraph.getNode : no node has id " + id.toString
      logger.writeln(msg)(PuckLog.Error)
      logger.writeln("nodes of graph : ")(PuckLog.Error)
      logger.writeln(sortedMap.mkString("\n", "\n\t", ""))(PuckLog.Error)
      throw new AGError("illegal node request : no node has id " + id.toString)
    case Some( (`id`, name, kind, styp, mutability, t) ) =>
      nodeBuilder(this, id, name, kind, styp, mutability, t)
    case _ => throw new AGError("incoherent index left and right id are different")
    }

  def removeNode(id : NIdT) = newGraph(nNodesSet = nodesIndex - id)

  def setNode(id : NIdT, name : String, k : NK, styp : STyp, mutable : Boolean, t : T) : GraphT =
    newGraph(nNodesSet = nodesIndex + (id -> (id, name, k, styp, mutable, t)))

  def setKind(id : NIdT, k : NK) = nodesIndex get id match {
    case None => throw new AGError("illegal node request : no node has id " + id.toString)
    case Some( (`id`, name, _, styp, mutability, t) ) => setNode(id, name, k, styp, mutability, t)
    case _ => throw new AGError("incoherent index left and right id are different")
  }
  def setType(id : NIdT, st : STyp) = nodesIndex get id match {
    case None => throw new AGError("illegal node request : no node has id " + id.toString)
    case Some( (`id`, name, k, _, mutability, t) ) => setNode(id, name, k, st, mutability, t)
    case _ => throw new AGError("incoherent index left and right id are different")
  }

  def setMutability(id : NIdT, mutable : Boolean) = nodesIndex get id match {
    case None => throw new AGError("illegal node request : no node has id " + id.toString)
    case Some(  (`id`, name, kind, styp, _, t) ) => setNode(id, name, kind, styp, mutable, t)
    case _ => throw new AGError("incoherent index left and right id are different")
  }

  def setInternal(id : NIdT, t : T) = nodesIndex get id match {
    case None => throw new AGError("illegal node request : no node has id " + id.toString)
    case Some(  (`id`, name, kind, styp, mutable, _) ) => setNode(id, name, kind, styp, mutable, t)
    case _ => throw new AGError("incoherent index left and right id are different")
  }

  def addContains(containerId: NIdT, contentId :NIdT, register : Boolean = true): GraphT =
     newGraph(nContentMap = contentsMap + (containerId, contentId),
              nContainerMap = containerMap + (contentId -> containerId),
              nRecording = if(register) recording.addEdge(AGEdge.contains(containerId, contentId))
                           else recording)

  def removeContains(containerId: NIdT, contentId :NIdT, register : Boolean = true): GraphT =
      newGraph( nContentMap = contentsMap - (containerId, contentId),
                nContainerMap = containerMap - contentId,
                nRecording = if(register) recording.removeEdge(AGEdge.contains(containerId, contentId))
                             else recording )

  def addUses(userId: NIdT, useeId: NIdT, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap + (useeId , userId),
             nUsesMap = usesMap + (userId, useeId),
             nRecording = if(register) recording.addEdge(AGEdge.uses(userId, useeId))
                          else recording)

  def removeUses(userId: NIdT, useeId: NIdT, register : Boolean = true): GraphT =
    newGraph(nUsersMap = usersMap - (useeId , userId),
             nUsesMap = usesMap - (userId, useeId),
             nRecording = if(register) recording.removeEdge(AGEdge.uses(userId, useeId))
                          else recording)

  def addIsa(subTypeId: NIdT, superTypeId: NIdT, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap + (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap + (subTypeId, superTypeId),
            nRecording = if(register) recording.addEdge(AGEdge.isa(subTypeId,superTypeId))
                         else recording)

  def removeIsa(subTypeId: NIdT, superTypeId: NIdT, register : Boolean = true) : GraphT=
    newGraph(nSubTypesMap = subTypesMap - (superTypeId, subTypeId),
             nSuperTypesMap = superTypesMap - (subTypeId, superTypeId),
             nRecording = if(register) recording.removeEdge(AGEdge.isa(subTypeId,superTypeId))
                          else recording)


  def addUsesDependency(dominantEdge : (NIdT, NIdT),
                        dominatedEdge : (NIdT, NIdT)) : GraphT =
    newGraph(nDominantUsesMap = dominantUsesMap + (dominatedEdge, dominantEdge),
      nDominatedUsesMap = dominatedUsesMap + (dominantEdge, dominatedEdge))

  def removeUsesDependency(dominantEdge : EdgeT,
                           dominatedEdge :EdgeT) : GraphT =
    removeUsesDependency((dominantEdge.source, dominantEdge.target),
      (dominatedEdge.source, dominatedEdge.target))

  def removeUsesDependency(dominantEdge : (NIdT, NIdT),
                           dominatedEdge : (NIdT, NIdT)) : GraphT =
    newGraph(nDominantUsesMap = dominantUsesMap - (dominatedEdge, dominantEdge),
      nDominatedUsesMap = dominatedUsesMap - (dominantEdge, dominatedEdge))

  def addAbstraction(id : NIdT, abs : (NIdT, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap + (id, abs))

  def removeAbstraction(id : NIdT, abs : (NIdT, AbstractionPolicy)) : GraphT =
    newGraph(nAbstractionsMap = abstractionsMap - (id, abs))

  /*def addEdge(edge : EdgeType, register : Boolean = true) : AccessGraph[NK] = edge.kind match {
    case Uses() => addUses(edge.user, edge.usee, register)
    case Contains() => addContains(edge.source, edge.target, register)
    case Isa() => addIsa(edge.source, edge.target, register)
  }

  def removeEdge(edge : EdgeType, register : Boolean = true) : AccessGraph[NK] = edge.kind match {
    case Uses() => removeUses(edge.user, edge.usee, register)
    case Contains() => removeContains(edge.source, edge.target, register)
    case Isa() => removeIsa(edge.source, edge.target, register)
  }*/

  def changeTarget(edge : EdgeT, newTarget : NIdT) : GraphT = {
    val g1 = edge.delete(this, register = false)
    val newEdge : EdgeT = new AGEdge(edge.kind, edge.source, newTarget)
    val newRecording = recording.changeEdgeTarget(edge, newTarget, withMerge = newEdge.exists(this))
    newEdge.create(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeSource(edge : EdgeT, newSource : NIdT) : GraphT = {
    val g1 = edge.delete(this, register = false)
    val newEdge: EdgeT = new AGEdge(edge.kind, newSource, edge.target)
    val newRecording = recording.changeEdgeSource(edge, newSource, withMerge = newEdge.exists(this))
    newEdge.create(g1, register = false).newGraph(nRecording = newRecording)
  }

  def changeType(id : NIdT, typ : STyp, oldUsee: NIdT, newUsee : NIdT) : GraphT = {
    val newTyp= typ.redirectUses(oldUsee, getNode(newUsee))

    setType(id, newTyp).
      newGraph(nRecording = recording.addTypeChange(id, typ, oldUsee, newUsee))

  }
  /*
   * Read-only queries
   */

  def nodeKinds = nodeBuilder.kinds

  def container(contentId : NIdT) : NIdT =
    containerMap.get(contentId) match {
      case None => contentId
        /*val msg = "AccessGraph.container : no container for " + getNode(contentId)
        logger.writeln(msg)(PuckLog.Error)
        logger.writeln("nodes of graph : ")(PuckLog.Error)
        logger.writeln(sortedMap.mkString("\n", "\n\t", ""))(PuckLog.Error)
        logger.writeln("container map of graph : ")(PuckLog.Error)
        logger.writeln(containerMap.toSeq.sortBy(_._1).mkString("\n", "\n\t", ""))
        throw new AGError(msg)*/
      case Some(id) => id
    }
  def content(containerId: NIdT) : Iterable[NIdT] = contentsMap.getFlat(containerId)

  def contains(containerId : NIdT, contentId : NIdT) : Boolean =
    container(contentId) == containerId

  def contains_*(containerId : NIdT, contentId : NIdT) : Boolean =
    containerId == contentId || {
      val containerOfContentId = container(contentId)
      !(containerOfContentId == contentId) && //not isRoot
        contains_*(containerId, containerOfContentId)
    }

  def directSuperTypes(sub: NIdT) : Iterable[NIdT] = superTypesMap getFlat sub
  def directSubTypes(sup: NIdT) : Iterable[NIdT] = subTypesMap getFlat sup

  def isa(subId : NIdT, superId: NIdT): Boolean = superTypesMap.bind(subId, superId)

  def uses(userId: NIdT, useeId: NIdT) : Boolean = usersMap.bind(useeId, userId)

  def usedBy(userId : NIdT) : Iterable[NIdT] = usesMap getFlat userId
  def users(useeId: NIdT) : Iterable[NIdT] = usersMap getFlat useeId

  def dominantUses(dominatedEdge : (NIdT, NIdT)) : Iterable[(NIdT, NIdT)] =
    dominantUsesMap getFlat dominatedEdge

  def dominatedUses(dominantEdge : (NIdT, NIdT)) : Iterable[(NIdT, NIdT)] =
    dominatedUsesMap  getFlat dominantEdge

  def dominates(dominantEdge : (NIdT, NIdT),
                dominatedEdge : (NIdT, NIdT)) : Boolean =
    dominatedUses( dominantEdge ).exists(_ == dominatedEdge)

  def abstractions(id : NIdT) : Iterable[(NIdT, AbstractionPolicy)] =
    abstractionsMap getFlat id

  def violations() : Seq[EdgeT] =
    nodesIndex.keys.flatMap {n =>
      val wu = constraints.wrongUsers(this, n).map(AGEdge.uses[NK](_,n))
      if(constraints.isWronglyContained(this, n))
         AGEdge.contains[NK](container(n), n) +: wu
      else wu
    }.toSeq

  def wrongUsers(id : NIdT) : Seq[NIdT] = constraints.wrongUsers(this, id)
  def isWronglyContained(id : NIdT) = constraints.isWronglyContained(this, id)
  def interloperOf(id1 : NIdT, id2 :NIdT) = constraints.interloperOf(this, id1, id2)

  def printConstraints[V](logger : Logger[V], v : V) : Unit =
    constraints.printConstraints(this, logger, v)


  def startSequence() : Breakpoint[NK, T] = this
  def undo(bp : Breakpoint[NK, T]) : GraphT = bp

  def coupling = nodes.foldLeft(0 : Double){ (acc, n) => acc + n.coupling }

  def subTree(root : NIdT) : Seq[NIdT] = {
    def aux(roots : Seq[NIdT], acc : Seq[NIdT]): Seq[NIdT] = roots match {
      case Seq() => acc
      case r +: tail =>
        val children = content(r)
        aux(children ++: tail, children ++: acc)

    }
    aux(Seq(root), Seq(root))
  }

  /*
   * High level modifications
   */

  def abstractionName(implId: NIdT, abskind : NK, policy : AbstractionPolicy) : String =
    getNode(implId).name + "_" + policy

  def createNode(implId: NIdT, abskind : NK, policy : AbstractionPolicy) : (NIdT, GraphT) = {
    val (id, g) = addNode(abstractionName(implId, abskind, policy), abskind, NoType())
    (id, g.addAbstraction(implId, (id, policy)))
  }

  def createAbstraction(implId: NIdT,
                        abskind : NK ,
                        policy : AbstractionPolicy) : (NIdT, GraphT) = {
    val (absId, g) = createNode(implId, abskind, policy)
    val g2 = g.setType(absId, getNode(implId).styp)
    (absId, policy match {
      case SupertypeAbstraction => g2.addUses(implId, absId)
      case DelegationAbstraction => g2.addUses(absId, implId)
    })

  }

  def abstractionCreationPostTreatment(implId : NIdT,
                                       absId : NIdT,
                                       policy : AbstractionPolicy) : GraphT = this

  def redirectUses(oldEdge : EdgeT, newUsee : NIdT,
                   policy : RedirectionPolicy,
                   propagateRedirection : Boolean = true,
                   keepOldUse : Boolean = false ) : (EdgeT, GraphT) = {
    if(oldEdge.usee == newUsee) (oldEdge, this)
    else if(oldEdge.exists(this)) {

      logger.writeln("redirecting %s target to %s (%s)".format(oldEdge, newUsee, policy))

      val newUse : EdgeT = AGEdge.uses(oldEdge.user, newUsee)

      val g2 =
        if(keepOldUse)
          newUse.create(this)
        else
          oldEdge.changeTarget(this, newUsee)


      val g3 = getNode(oldEdge.user).styp match {
        case NoType() => g2
        case sTyp => g2.changeType(oldEdge.user, sTyp, oldEdge.usee, newUsee)
      }

      val g4 =
      if(propagateRedirection) {
        g3.redirectPrimaryUses(oldEdge, newUsee, policy).
        redirectSideUses(oldEdge, newUsee, policy)
      }
      else g3

      (newUse, g4)
    }
    else if (uses(oldEdge.user, newUsee)) {
      //if m uses  both A and A.mi, the first uses dominate the second
      //if both are identified as violations and are in a wrongusers list
      //redirecting the one will redirect the other
      // when iterating on the wrongusers, the next call to redirectuses will arrive here
      logger.writeln("redirecting uses' %s target to %s (%s) : FAILURE !! %s is not used".
        format(oldEdge, newUsee, policy, oldEdge.usee))
      (AGEdge.uses(oldEdge.user, newUsee), this)
    }
    else {
      if(users(oldEdge.usee).exists(_ == oldEdge.user) ||
        users(newUsee).exists(_==oldEdge.user))
        throw new AGError("incoherent state !!!!!!!!!!!!")

      throw new AGError(("redirecting uses' %s target to %s (%s)\n" +
        "!!! nor the oldUsee or the newUsee is really used !!! ").
        format(oldEdge, newUsee, policy))
    }
  }

  def redirectPrimaryUses(currentSideUse : EdgeT,
                          newSideUsee : NIdT,
                          policy : RedirectionPolicy,
                          propagateRedirection : Boolean = true) : GraphT = {


    logger.writeln("redirecting primary uses of side use %s (new side usee is %s) ".
      format(currentSideUse, newSideUsee))

     val primaryUses = dominantUses(currentSideUse)
     if(primaryUses.isEmpty) {
       logger.writeln("no primary uses to redirect")
       this
     }
     else{
       logger.writeln("uses to redirect:%s".format(primaryUses.mkString("\n\t", "\n\t","\n")))

       primaryUses.foldLeft(this){
         case (g, primary0) =>
           val primary = AGEdge.uses[NK](primary0)
           val g1 = g.removeUsesDependency(primary, currentSideUse)

           val keepOldUse = dominatedUses(primary).nonEmpty //is empty if primary had only one side use

           val (newPrimary, g2) =
             g1.redirectUses(primary,
                g1.findNewPrimaryUsee(primary.usee, newSideUsee, policy),
                policy, propagateRedirection, keepOldUse)


           g2.addUsesDependency(newPrimary, (currentSideUse.user, newSideUsee))
       }
     }
  }

  def findNewPrimaryUsee(currentPrimaryUsee : NIdT,
                         newSideUsee : NIdT,
                         policy : RedirectionPolicy) : NIdT = {

    logger.writeln("searching new primary usee ("+ policy + ") : currentPrimaryUsee is " +
      currentPrimaryUsee + ", new side usee " + newSideUsee)

    val newPrimaryUsee =
      policy match {
        case Move => container(newSideUsee)
        case absPolicy : AbstractionPolicy =>
          abstractions(currentPrimaryUsee).find {
            case (node, `absPolicy`) => contains_*(node, newSideUsee)
            case _ => false
          } match {
            case Some((n, _)) => n
            case None =>
              val abstractKinds =
                      getNode(currentPrimaryUsee).kind.
                          abstractKinds(absPolicy)

              nodesId.find{node =>
                contains_*(node, newSideUsee) && {
                  abstractKinds.contains(getNode(node).kind)
                }

              } match {
                case Some(n) =>
                  logger.writeln(n + " found as primary usee")

                  n
                case None =>
                  val msg = "no correct primary abstraction found !"
                  logger.writeln(msg)(PuckLog.Error)
                  throw new RedirectionError(msg)
              }
          }
      }
    logger.writeln("new primary usee found : " + newPrimaryUsee)
    newPrimaryUsee
  }


  def redirectSideUses(currentPrimaryUse: EdgeT,
                       newPrimaryUsee : NIdT,
                       policy : RedirectionPolicy) : GraphT = {
    logger.writeln("redirecting side uses of primary use %s (new primary usee is %s) ".
      format(currentPrimaryUse, newPrimaryUsee))

    val sideUses = dominatedUses(currentPrimaryUse)
    if(sideUses.isEmpty){
      logger.writeln("no side uses to redirect")
      this
    }
    else{
      logger.writeln("uses to redirect:%s".format(sideUses.mkString("\n\t", "\n\t","\n")))

      sideUses.foldLeft(this){
        case (g, side0) =>
          val side = AGEdge.uses[NK](side0)
          abstractions(side.usee).find {
            case (abs, _) => contains(newPrimaryUsee, abs)
            case _ => false
          } match {
            case None =>
              val msg = ("While redirecting primary uses %s target to %s\n" +
                "no satisfying abstraction to redirect side use %s").
                format(currentPrimaryUse, newPrimaryUsee, side)
              logger.writeln(msg)(PuckLog.Error)

              throw new RedirectionError(msg)

            case Some( (new_side_usee, _) ) =>

              val g1 = removeUsesDependency(currentPrimaryUse, side)

              val (newSide, g2) = g1.redirectUses(side, new_side_usee, policy)

              g2.addUsesDependency((currentPrimaryUse.user, newPrimaryUsee), newSide)

          }
      }
    }
  }

  def moveTo(movedId : NIdT, newContainer : NIdT): GraphT = {
    val oldContainer = container(movedId)
    logger.writeln("moving " + movedId +" from " + oldContainer + " to " + newContainer)
    val g2 = changeSource(AGEdge.contains(oldContainer, movedId), newContainer)
    users(movedId).foldLeft(g2){
      case (g0, userId) =>
        g0.redirectPrimaryUses(AGEdge.uses(userId, movedId), movedId,
                                      Move, propagateRedirection = false)
    }
  }

  def addHideFromRootException(node : NIdT, friend : NIdT): GraphT =
      newGraph(nConstraints = constraints.addHideFromRootException(node, friend))

  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(this, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(this, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/



}