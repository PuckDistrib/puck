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
package transformations.rules

import puck.graph.ShowDG._
import puck.graph.constraints.{ConstraintsMaps, AbstractionPolicy, DelegationAbstraction, SupertypeAbstraction}
import puck.util.LoggedEither._

import scalaz.std.list._
import scalaz.std.set._

abstract class Abstract {


  def absIntroPredicate(impl : DGNode,
                        absPolicy : AbstractionPolicy,
                        absKind : NodeKind)
                       (implicit constraints: ConstraintsMaps) : NodePredicate =


    (absKind.kindType, absPolicy) match {
      case (InstanceValueDecl, SupertypeAbstraction) =>
        (graph, potentialHost) => {
          val typeDecl = graph.container(impl.id).get
          val potentialSuperType = potentialHost.id
          val canExtends = !(graph, constraints).interloperOf(typeDecl, potentialSuperType)
          canExtends && graph.nodeKindKnowledge.canContain(graph, potentialHost, absKind)
        }
      case (_, SupertypeAbstraction) =>
        (graph, potentialHost) => !(graph, constraints).interloperOf(impl.id, potentialHost.id) &&
          graph.nodeKindKnowledge.canContain(graph, potentialHost, absKind)

      case (_, DelegationAbstraction) =>
        (graph, potentialHost) => !(graph, constraints).interloperOf(potentialHost.id, impl.id) &&
          graph.nodeKindKnowledge.canContain(graph, potentialHost, absKind)
    }



  def abstractionName
  ( g: DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    sUsesAccessKind: Option[UsesAccessKind]
    ) : String =
    impl.name + "_" + policy

  def absType
  ( g : DependencyGraph,
    impl : NodeId,
    sUsesAccessKind: Option[UsesAccessKind]
    ) : Type =
    sUsesAccessKind match {
      case None | Some(Read) => (g styp impl).get
      case Some(Write) => g.nodeKindKnowledge.writeType(g)
      case Some(RW) => sys.error("should not happen")
  }

  def createAbsNodeAndUse
  ( g : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy
    ) : (Abstraction, DependencyGraph) = {
    import g.nodeKindKnowledge.intro

    policy match {
      case DelegationAbstraction if impl.kind.isWritable =>

        val rName = abstractionName(g, impl, abskind, DelegationAbstraction, Some(Read))
        val rType = absType(g, impl.id, Some(Read))
        val wName = abstractionName(g, impl, abskind, DelegationAbstraction, Some(Write))
        val wType = absType(g, impl.id, Some(Write))

        val paramKind = g.nodeKindKnowledge.kindOfKindType(Parameter).head


        val (rNode, rDef, g1) = intro.nodeWithDef(g, rName, abskind, rType)
        val (wNode, wDef, g2) = intro.nodeWithDef(g1, wName, abskind, wType)

        val (pNode, g3) = g2.addConcreteNode(impl.name, paramKind)
        val g4 = g.styp(impl.id) map (g3.addType(pNode.id, _)) getOrElse g3

        val g5 =
          g4.addContains(wNode.id, pNode.id)
            .addUses(rDef.id, impl.id, Some(Read))
            .addUses(wDef.id, impl.id, Some(Write))

        val abs = ReadWriteAbstraction(Some(rNode.id), Some(wNode.id))
        (abs, g4.addAbstraction(impl.id, abs))

      case DelegationAbstraction =>
        val name = abstractionName(g, impl, abskind, policy, None)

        val (absNode, ndef, g1) =
          intro.nodeWithDef(g, name, abskind, g typ impl.id)
        val g2 =
          if(impl.kind.kindType == TypeConstructor) {
            val typ = g container_! impl.id
            g1.setRole(absNode.id, Some(Factory(impl.id)))
              //type returned must be a subtype of the factory type
                .addTypeUsesConstraint((impl.id, typ), Sup((absNode.id, typ)) )
          }
          else g1

        val g3 = g2.parametersOf(impl.id).foldRight(g2) {
          (paramId, g0) =>
            val param = g0.getConcreteNode(paramId)
            val (pabs, g01) = g0.addConcreteNode(param.name, param.kind)
            val g02 = (g0 styp paramId) map (g01.addType(pabs.id, _)) getOrElse g01
            g02.addEdge(ContainsParam(absNode.id, pabs.id))

        }
        val g4 = g3.addUses(ndef.id, impl.id)

        val abs = AccessAbstraction(absNode.id, policy)
        (abs, g4.addAbstraction(impl.id, abs))

      case SupertypeAbstraction =>

        val name = abstractionName(g, impl, abskind, policy, None)
        val (n, g1) = g.addConcreteNode(name, abskind)
        val g2 = (g1 styp impl.id) map (g1.addType(n.id, _)) getOrElse g1


        val g3 = g2.parametersOf(impl.id).foldRight(g2) {
          (paramId, g0) =>
            val param = g0.getConcreteNode(paramId)
            val (pabs, g01) = g0.addConcreteNode(param.name, param.kind)
            val g02 = (g0 styp paramId) map (g01.addType(pabs.id, _)) getOrElse g01

            g02.addEdge(ContainsParam(n.id, pabs.id))
//            g02.usedByExcludingTypeUse(paramId).foldLeft(g02.addContains(n.id, pabs.id)) {
//              (g00, tid) => g00.addUses(pabs.id, tid)
//            }
        }
        val abs = AccessAbstraction(n.id, policy)
        (abs, g3.addAbstraction(impl.id, abs))

    }
  }



  def insertInTypeHierarchy
  ( g : DependencyGraph,
    subTypeId : NodeId,
    newSuperTypeId : NodeId
    ) : LoggedTG =
    g.directSuperTypes(subTypeId).foldLoggedEither(g){
      (g0, oldSuperTypedId) =>

        val g1 = g0.changeSource(Isa(subTypeId, oldSuperTypedId), newSuperTypeId)

        def extractMethod(typeDeclId : NodeId) : List[(ConcreteNode, Type)] =
          g1.content(typeDeclId).toList map g1.getConcreteNode filter { n =>
            n.kind.kindType == InstanceValueDecl &&
              n.kind.abstractionNodeKinds(SupertypeAbstraction).nonEmpty
          } map (n => (n, g1.styp(n.id).get))

        val subTypeMeths = extractMethod(subTypeId)
        val newSupTypeMeths = extractMethod(newSuperTypeId)
        val oldSupTypeMeths = extractMethod(oldSuperTypedId)

        Type.findAndRegisterOverridedInList(g1, newSupTypeMeths, subTypeMeths){
          Type.ignoreOnImplemNotFound
        } flatMap (
          Type.findAndRegisterOverridedInList(_, oldSupTypeMeths, newSupTypeMeths){
            Type.ignoreOnImplemNotFound
          })

    }

  def redirectTypeUseInParameters
  ( g : DependencyGraph, meth : ConcreteNode,
    clazz : ConcreteNode, interface : ConcreteNode): LoggedTG = {

      val log = "Abstract.redirectTypeUseInParameters : " +
        s"redirecting Uses(${meth.name}, ${clazz.name}) target to $interface\n"

      val ltg = g.parametersOf(meth.id).foldLoggedEither(g){
        (g0, pid) =>
          if (g0.uses(pid, clazz.id))
            Redirection.redirectUsesAndPropagate(g0, Uses(pid, clazz.id),
              AccessAbstraction(interface.id, SupertypeAbstraction))
          else
            LoggedSuccess(g0)

      }
      log <++: ltg
  }

  def redirectTypeUseInParameters
  ( g : DependencyGraph, members : List[ConcreteNode],
    clazz : ConcreteNode, interface : ConcreteNode): LoggedTG =
    members.foldLoggedEither(
      g.comment(s"redirectTypeUseInParameters(g, $members, $clazz, $interface)")){
      (g0, child) =>
        child.kind.kindType match {
          case InstanceValueDecl if g0.parametersOf(child.id).exists(g0.uses(_, clazz.id)) =>
            redirectTypeUseInParameters(g0, child, clazz, interface)
          case _ => LoggedSuccess(g0)
        }
    }


  def canBeAbstracted
    (g : DependencyGraph,
     memberDecl : ConcreteNode,
     clazz : ConcreteNode,
     policy : AbstractionPolicy) : Boolean = {
    //the originSiblingDecl arg is needed in case of cyclic uses
    def aux(originSiblingDecl : ConcreteNode)(memberDecl: ConcreteNode): Boolean = {


      def sibling: NodeId => Boolean =
        sid => g.contains(clazz.id, sid) &&
          sid != originSiblingDecl.id &&
          sid != memberDecl.id

      def usedOnlyViaSelf(user : NodeId, used : NodeId) : Boolean =
        g.typeUsesOf(user, used) forall { case (s, t) => s == t }
      

      //TODO check if the right part of the and is valid for Delegation abstraction
      memberDecl.kind.canBeAbstractedWith(policy) && {

        val usedNodes = (memberDecl.definition(g) map g.usedByExcludingTypeUse).getOrElse(Set())

        usedNodes.isEmpty || {
          val usedSiblings = usedNodes filter sibling map g.getConcreteNode
          usedSiblings.forall {
            used0 =>
              aux(memberDecl)(used0) || usedOnlyViaSelf(memberDecl.definition_!(g), used0.id)
          }
        }
      }
    }

    aux(memberDecl)(memberDecl)
  }


  def typeMembersToPutInInterface
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    policy : AbstractionPolicy) : List[ConcreteNode] = {

    g.content(clazz.id).foldLeft(List[ConcreteNode]()){
      (acc, mid) =>
        val member = g.getConcreteNode(mid)
        if(canBeAbstracted(g, member, clazz, policy)) member +: acc
        else acc
    }
  }


  def abstractTypeDeclAndReplaceByAbstractionWherePossible
  ( g : DependencyGraph,
    clazz : ConcreteNode,
    abskind : NodeKind,
    policy : AbstractionPolicy,
    members : List[ConcreteNode]
    ) : LoggedTry[(Abstraction, DependencyGraph)] = {

    def createAbstractTypeMemberWithSuperSelfType
    ( g : DependencyGraph,
      meth : ConcreteNode,
      interface : ConcreteNode
      ) : LoggedTG ={
      createAbstraction(g, meth, meth.kind.abstractionNodeKinds(policy).head, policy) flatMap {
        case (AccessAbstraction(absMethodId, _), g0) =>

          val g1 = g0.addContains(interface.id, absMethodId)
            //is change type needed in case of delegation policy
            .changeTarget(Uses(absMethodId, clazz.id), interface.id) //change return type
            //.changeType(absMethodId, clazz.id, interface.id) //change return type
          redirectTypeUseInParameters(g1, g1.getConcreteNode(absMethodId),
            clazz, interface)

        case _ => LoggedError("unexpected type of abstraction")
      }

    }

    val (interfaceAbs, g1) = createAbsNodeAndUse(g, clazz, abskind, policy)

    val interface = interfaceAbs match {
      case AccessAbstraction(id, _) => g1.getConcreteNode(id)
      case _ => sys.error("type should not have a RW abstraction")
    }


    for{

      g2 <- members.foldLoggedEither(g1)(createAbstractTypeMemberWithSuperSelfType(_, _, interface))

      g3 <- policy match {
        case SupertypeAbstraction => insertInTypeHierarchy(g2, clazz.id, interface.id)
        case DelegationAbstraction => LoggedSuccess(g2)
      }

      g4 <- if(policy == SupertypeAbstraction)
        redirectTypeUseInParameters(g3.addIsa(clazz.id, interface.id), members,
          clazz, interface)
      else LoggedSuccess[DependencyGraph](g3)


      log = s"interface $interface created, contains : {" +
             g4.content(interface.id).map(nid => (g4, nid).shows).mkString("\n")+
             "}"
      g5 <- LoggedSuccess(log, g4)

    } yield {
      (interfaceAbs, g5)
    }
  }

//  private def createAbsNodeAndUse
//  ( g : DependencyGraph,
//    impl: ConcreteNode,
//    abskind : NodeKind ,
//    policy : AbstractionPolicy
//    ) : LoggedTry[(Abstraction, DependencyGraph)] = {
//    val (abs, g1) = createAbsNode(g, impl, abskind, policy)
//
//    LoggedSuccess((abs, abs match {
//      case AccessAbstraction(absId, SupertypeAbstraction) => g1
//        //optional isa arc is added after insertion in type hierarchy
//      case AccessAbstraction(absId, DelegationAbstraction) =>
//        val g2 = g1.addUses(absId, impl.id)
//        if(impl.kind.kindType == TypeConstructor){
//          g2.setRole(absId, Some(Factory(impl.id)))
//        }
//        else g2
//

//
//    }))
//
//  }

  def createAbstraction
  ( graph : DependencyGraph,
    impl: ConcreteNode,
    abskind : NodeKind ,
    policy : AbstractionPolicy
    ) : LoggedTry[(Abstraction, DependencyGraph)] ={
    val g = graph.comment(s"Abstract.createAbstraction(g, ${(graph,impl).shows}, $abskind, $policy)")
    (abskind.kindType, policy) match {
      case (TypeDecl, SupertypeAbstraction) =>
        val methods = typeMembersToPutInInterface(g, impl, SupertypeAbstraction)
        val log = s"Creating $abskind with abstractions of" +
          methods.mkString("\n", "\n", "\n")

        val ltg = abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
          impl,
          abskind, SupertypeAbstraction,
          methods)

        log <++: ltg
      case (TypeDecl, DelegationAbstraction) =>

        val methods = g.content(impl.id).foldLeft(List[ConcreteNode]()){
          (acc, mid) =>
            val member = g.getConcreteNode(mid)
            if(member.kind canBeAbstractedWith DelegationAbstraction) member +: acc
            else acc
        }

        abstractTypeDeclAndReplaceByAbstractionWherePossible(g,
          impl, abskind, DelegationAbstraction, methods)

      case _ => LoggedSuccess(createAbsNodeAndUse(g, impl, abskind, policy))
  }
  }



}
