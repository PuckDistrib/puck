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
package constraints.search
import puck.graph.constraints.ConstraintsMaps
import puck.graph.transformations.{Mutable, TransformationRules}
import puck.graph.transformations.rules.{CreateParameter, CreateTypeMember, CreateVarStrategy}
import puck.util.LoggedEither
import puck.util.LoggedEither._

import scalaz.std.list._
import scalaz.syntax.foldable._
import scalaz.{-\/, NonEmptyList, \/-}
import ShowDG._

/**
  * Created by Loïc Girault on 10/05/16.
  */

object SolvingActions {
  //predicate for host of newly introduced abstraction
  def newAbsFindHostPredicate
  ( impl : DGNode,
    absPolicy : AbstractionPolicy,
    absKind : NodeKind)
  ( implicit constraints: ConstraintsMaps) : NodePredicate =

    (absKind.kindType, absPolicy) match {
      case (InstanceValue, SupertypeAbstraction) =>
        (graph, potentialHost) => {
          val typeDecl = graph.container(impl.id).get
          val potentialSuperType = potentialHost.id
          val canExtends = !constraints.isForbidden(graph, typeDecl, potentialSuperType)
          canExtends && graph.canContain(potentialHost, absKind)
        }
      case (_, SupertypeAbstraction) =>
        (graph, potentialHost) =>
          val authorizedByContraint = !constraints.isForbidden(graph, impl.id, potentialHost.id)
          val structuralyLegal =  graph.canContain(potentialHost, absKind)
//          println(s"impl = $impl potentialHost = $potentialHost " +
//            s"is authorized + $authorizedByContraint"+
//            s"canContain = $structuralyLegal")
          structuralyLegal && authorizedByContraint


      case (_, DelegationAbstraction) =>
        (graph, potentialHost) => !constraints.isForbidden(graph, potentialHost.id, impl.id) &&
          graph.canContain(potentialHost, absKind)
    }
}

sealed abstract class VirtualNodePolicy {
  def virtualizableKindFor(toBeContainedKind : NodeKind) : Set[KindType]
}
case object WithVirtualNodes extends VirtualNodePolicy {
  override val toString = "Use virtual nodes"
  def virtualizableKindFor(toBeContainedKind : NodeKind) : Set[KindType] =
    if(toBeContainedKind.kindType == StableValue) Set(NameSpace, TypeDecl)
    else Set(NameSpace)
}
case object NoVirtualNodes extends VirtualNodePolicy {
  override val toString = "No virtual nodes"
  def virtualizableKindFor(toBeContainedKind : NodeKind) : Set[KindType] =
    Set()
}

import SolvingActions.newAbsFindHostPredicate
class SolvingActions
(val rules : TransformationRules,
 val vnPolicicy : VirtualNodePolicy,
 implicit val constraints : ConstraintsMaps) {

  var newCterNumGen = 0

  def containerKind
  (g : DependencyGraph,
   toBeContainedKind: NodeKind
  ): Stream[NodeKind] =
    g.nodeKinds.toStream.filter(_.canContain(toBeContainedKind))

  def abstractionHostKind
  (g : DependencyGraph,
   toBeAbstractedKind : NodeKind
  ) : Stream[NodeKind] =
    (for {
      absK <- toBeAbstractedKind.abstractionChoices map (_._1)
      cK <- g.nodeKinds
      if cK canContain absK
    } yield cK).toSet.toStream


  def hostIntro
  (g0 : DependencyGraph,
   toBeContained: ConcreteNode
  ) : Stream[LoggedTry[(NodeId, DependencyGraph)]] =
      containerKind(g0, toBeContained.kind) map {
        hostKind =>
          newCterNumGen += 1
          val hostName = s"${toBeContained.name}_container$newCterNumGen"
          rules.intro(g0, hostName, hostKind)
      } flatMap {
        case (toBeCtedHost, g1) => {
          val s = findHost(g1.setMutability(toBeCtedHost.id, Mutable), toBeCtedHost)
          s map {
            _ flatMap {
              case (hid, g2) =>
                if (g2.contains_*(toBeContained.id, hid))
                  LoggedError("Error : introducing contains loop")
                else LoggedSuccess((toBeCtedHost.id, g2.addContains(hid, toBeCtedHost.id)))
            }
          }
        }
      }




  def findHost
  (graph : DependencyGraph,
   toBeContained: ConcreteNode) : Stream[LoggedTry[(NodeId, DependencyGraph)]] =
    chooseNode(graph,
      (dg, cn) => dg.canContain(cn, toBeContained) &&
      !constraints.isForbidden(dg, cn.id, toBeContained.id) && {
      val dg1 : DependencyGraph =
        dg.container(toBeContained.id) map (dg.removeContains(_, toBeContained.id)) getOrElse dg
      val dg2 = dg1.addContains(cn.id, toBeContained.id)
      dg2.subTree(toBeContained.id).forall(!constraints.isWronglyUsed(dg2, _)) //needed when moving violation host
    }, vnPolicicy.virtualizableKindFor(toBeContained.kind)) map (
      s"Searching host for $toBeContained\n" <++: _ )


  def logNodeChosen(n : DGNode, graph : DependencyGraph) : LoggedTry[(NodeId, DependencyGraph)] =
    LoggedSuccess(s"node chosen is $n\n", (n.id, graph))


  def chooseNode
  ( graph : DependencyGraph,
    predicate : NodePredicate, virtualizableKind : Set[KindType] = Set(NameSpace)
  ) : Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
      val initialChoices = graph.mutableNodes.toList
      //val choices = graph.mutableNodes.toList map graph.getNode filter (predicate(graph,_))
      val choices = initialChoices map graph.getNode filter (predicate(graph,_))
      ////val choices = graph.concreteNodes.filter(predicate(graph,_)).toList

      if(choices.isEmpty)
        Stream(LoggedError(s"chooseNode, choices is empty"))
      else {

        val s = partitionByKind(graph)(choices, List()).toStream

        s flatMap {
          nel =>
            if (nel.tail.isEmpty) Stream(logNodeChosen(nel.head, graph))
            else if ( virtualizableKind contains nel.head.kind.kindType ) {
              val (vn, g2) = graph.addVirtualNode(nel.map(_.id).toSet, nel.head.kind)
              Stream(logNodeChosen(vn, g2))
            }
            else {
              nel.toStream map (logNodeChosen(_, graph))
            }
        }
      }
  }

  def partitionByKind
  ( graph : DependencyGraph
  ) : (List[DGNode], List[NonEmptyList[DGNode]]) => List[NonEmptyList[DGNode]] = {
    case (Nil, acc) => acc
    case (hd :: tl, acc) =>
      val (same, diff) = tl.partition(n => n.kind == hd.kind)
      partitionByKind(graph)(diff, NonEmptyList[DGNode](hd, same:_*) +: acc)
  }



  def createVarStrategies
  ( g: DependencyGraph ): Stream[CreateVarStrategy] = {
    val tmKinds = g.nodeKindKnowledge.kindOfKindType(InstanceValue)
    (CreateParameter +: (tmKinds map CreateTypeMember.apply)).toStream
  }



  def move
  ( g0 : DependencyGraph,
    wronglyContained : ConcreteNode
  ) : Stream[LoggedTry[(NodeId, DependencyGraph)]] = {
    val lg = s"trying to move $wronglyContained, searching host\n"
    hostIntro(g0, wronglyContained) ++
      findHost(g0, wronglyContained) flatMap {
      case LoggedError(log, err) =>
        Stream(LoggedError(lg + log, err))
      case LoggedSuccess(log, (newCter, g)) =>
        val oldCter = g.container_!(wronglyContained.id)
        doMove(g, wronglyContained, oldCter, newCter) map (ltg => (lg + log) <++: ltg.map((newCter, _)))
    }
  }

  def doMove
  ( g: DependencyGraph,
    wronglyContained : ConcreteNode,
    oldCter : NodeId,
    newCter : NodeId
  ) :  Stream[LoggedTG] = {
    val stream: Stream[LoggedTG] = (wronglyContained.kind.kindType, g.styp(wronglyContained.id)) match {
      case (InstanceValue, Some(typ)) =>

        val needNewReceiver = !(typ uses newCter)

        val isPullUp = g.isa_*(oldCter, newCter)
        val isPushDown = g.isa_*(newCter, oldCter)

        (isPullUp, isPushDown) match {
          case (true, true) =>
            assert(oldCter == newCter)
            Stream(LoggedSuccess(g))
          case (true, _) =>
            Stream(rules.move.pullUp(g, List(wronglyContained.id), oldCter, newCter))
          case (_, true) =>
            Stream(rules.move.pushDown(g, List(wronglyContained.id), oldCter, newCter))
          case _ if needNewReceiver =>
            createVarStrategies(g) map {
              cvs =>
                rules.move.typeMemberBetweenUnrelatedTypeDecl(g,
                  List(wronglyContained.id), oldCter, newCter, Some(cvs))
            }
          case _ =>
            Stream(rules.move.typeMemberBetweenUnrelatedTypeDecl(g, List(wronglyContained.id), oldCter, newCter, None))
        }

      case (TypeDecl, _)
           | (NameSpace, _)
           | (StableValue, _) =>
        Stream(rules.move.staticDecl(g, wronglyContained.id, newCter))

      case (TypeConstructor, _) =>
        Stream(LoggedError("TypeConstructor cannot be moved"))
      //true for java or C++ but not necessarily for all kind of languages
      //fail in move rule ? (which would rely on a canMove language specific predicate ?)

      case (kindType, styp) => Stream(LoggedError(s"doMove $kindType typed $styp not expected"))
    }

    stream map { ltg => s"Moving $wronglyContained " +
      s"from ${(g, oldCter).shows} to ${(g, newCter).shows}\n" <++: ltg }
  }

  /*def absIntro
  ( g : DependencyGraph,
    impl : ConcreteNode
  ) :  Stream[LoggedTry[(Abstraction, DependencyGraph)]] =
      impl.kind.abstractionChoices.toStream map {
        case (absNodeKind, absPolicy) =>
          rules.abstracter.createAbstraction(g, impl, absNodeKind, absPolicy)
      } flatMap {
        case lt @ LoggedEither(_, -\/(_)) => Stream(lt)
        case lt @ LoggedEither(log, \/-((abs, g2))) =>
          val absNodeKind = abs.kind(g2)

          val g3 =  g2.setMutability(abs.nodes, Mutable)

          //fields abstractions introduced with container
          if((g3 container abs.nodes.head).nonEmpty)
              Stream(LoggedEither(log, \/-((abs, g3))))
          else
          (hostIntro(g3, g3.getConcreteNode(abs.nodes.head)) ++
            chooseNode(g3,
                        newAbsFindHostPredicate(impl,
                                                abs.policy, absNodeKind),
                                                vnPolicicy.virtualizableKindFor(abs.kind(g3)))
            ).map {
            lt =>  s"$log\nSearching host for $abs\n" <++: lt.flatMap {
              case (host, g4) => introAbsContainsAndIsa(abs, impl, g4, host)

            }
          }
      }*/

  def absIntro
  ( g : DependencyGraph,
    impl : ConcreteNode
  ) :  Stream[LoggedTry[(Abstraction, DependencyGraph)]] = {
    val choices  = impl.kind.abstractionChoices.toStream;
    val abstractions = choices.map {
      case (absNodeKind, absPolicy) =>
       rules.abstracter.createAbstraction(g, impl, absNodeKind, absPolicy)
    }

    abstractions flatMap {
      case lt@LoggedEither(_, -\/(_)) => Stream(lt)
      case lt@LoggedEither(log, \/-((abs, g2))) =>
        val absNodeKind = abs.kind(g2)

        val g3 = g2.setMutability(abs.nodes, Mutable)

        //fields abstractions introduced with container
        if ((g3 container abs.nodes.head).nonEmpty)
          Stream(LoggedEither(log, \/-((abs, g3))))
        else
          (hostIntro(g3, g3.getConcreteNode(abs.nodes.head)) ++
            chooseNode(g3,
              newAbsFindHostPredicate(impl,
                abs.policy, absNodeKind),
              vnPolicicy.virtualizableKindFor(abs.kind(g3)))
            ).map {
            lt =>
              s"$log\nSearching host for $abs\n" <++: lt.flatMap {
                case (host, g4) => introAbsContainsAndIsa(abs, impl, g4, host)

              }
          }
    }
  }

  def introAbsContainsAndIsa
  ( abs : Abstraction,
    impl : ConcreteNode,
    g : DependencyGraph,
    host : NodeId) : LoggedTry[(Abstraction, DependencyGraph)] = {
    val g2 = abs.nodes.foldLeft(g){_.addContains(host, _)}

    (g2.container(impl.id), g2.kindType(host)) match {
      case (None, _) => LoggedError("current impl has no container")
      case (Some(c), TypeDecl) =>
        if(c == host) LoggedError("node cannot be its own abstraction")
        else {
          val graph5 = g2.addAbstraction(c, AccessAbstraction(host, abs.policy))
          val graph6 =
            if (abs.policy == SupertypeAbstraction)
              graph5.addIsa(NamedType(c), NamedType(host)).addUses(c, host)
            else graph5
          LoggedSuccess((abs, graph6))
        }
      case _ => LoggedSuccess((abs, g2))
    }

  }


  def redirectTowardExistingAbstractions
  (g : DependencyGraph,  used : ConcreteNode) : Stream[LoggedTG] =
      redirectTowardExistingAbstractions(used,
        constraints.wrongUsers(g, used.id),
        g abstractions used.id)(g)





  def redirectTowardExistingAbstractions
  ( used : ConcreteNode,
    wrongUsers : List[NodeId],
    choices : Set[Abstraction]) :
  DependencyGraph => Stream[LoggedTG] =
    g =>
      if(wrongUsers.isEmpty) Stream(LoggedSuccess(g))
      else if(choices.isEmpty) Stream(LoggedError("No abstractions"))
      else {

        choices.toStream flatMap {
          abs =>
            val (remainingWus, wuToRedirect) =
              wrongUsers.partition( userId =>
                abs.nodes exists (constraints.isForbidden(g, userId, _)))


            val ltg: LoggedTG =
              wuToRedirect.foldLoggedEither(g) {
                (g, wu) =>
                  // Uses(wu, used.id) can have be deleted in a
                  // previous iteration of this foldLoggedEither loop
                  if(g uses (wu, used.id))
                    rules.redirection.redirectUsesAndPropagate(g, (wu, used.id), abs)
                  else
                    LoggedSuccess(g)

              }

            ltg.value match {
              case \/-(g2) =>
                redirectTowardExistingAbstractions(used, remainingWus, choices - abs)(g2) map ( ltg.log <++: _)
              case -\/(_) => Stream(ltg)
            }
        }
      }
}