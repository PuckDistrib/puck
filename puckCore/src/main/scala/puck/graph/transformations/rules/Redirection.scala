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

package puck.graph.transformations.rules

import puck.PuckError
import puck.graph._
import puck.util.LoggedEither
import LoggedEither._

import scalaz.std.list._
import scalaz.std.set._
import ShowDG._

object Redirection {

  def redirectSourceOfInitUseInFactory
  ( g: DependencyGraph,
    ctorDecl: NodeId,
    ctorDef : NodeId,
    initializer : NodeId,
    factory : NodeId) : DependencyGraph = {

    val clazz = g.container_!(ctorDecl)
    val factoryDef = g.definitionOf_!(factory)
    val selfUse = Uses(clazz,clazz)
    val g1 = g.changeSource(Uses(ctorDef, initializer), factoryDef)
      .addUses(factoryDef, clazz)
      .removeBinding(selfUse, (ctorDef, initializer))
      .addBinding((factoryDef, clazz), (factoryDef, initializer))

    val tid = Type.mainId(g1.typ(factory))

    val returnTypeConstraint = {
      val tucs =  g1.typeConstraints((factory, tid)).filter{
        case Sub(_) => true
        case _ => false
      }
      if(tucs.size > 1) error()
      tucs.head
    }
    //type constraint on factory return type
    val Sub((_, st)) = returnTypeConstraint

    val g2 =  g1.removeTypeUsesConstraint((factory, tid), returnTypeConstraint)
      .addTypeUsesConstraint((factory, tid), Sub((factoryDef, st)))
      //constraint on newly extracted local variable
      .addTypeUsesConstraint((factoryDef, st), returnTypeConstraint)

    if(g2.typeMemberUsesOf(selfUse).isEmpty)
      g2.removeEdge(selfUse)
    else g2
  }

  def cl(g: DependencyGraph, u : NodeIdP) : Set[(NodeIdP, NodeIdP)] = {
    val tus : Set[NodeIdP] =
      g.kindType(u.used) match {
        case TypeDecl => Set(u)
        case _ => g.typeUsesOf(u)
      }

    for {
      tu <-  tus
      tmu <- g.typeMemberUsesOf(tu)
    } yield (tu, tmu)

  }


  def tmAbstraction
  ( g : DependencyGraph,
    typeAbs : NodeId,
    tmImpl : NodeId
  ) : LoggedTry[Abstraction] = {
    val log = s"tmAbstraction : searching an abstraction of ${(g, tmImpl).shows} in ${(g, typeAbs).shows}\n"
    val absSet = g.abstractions(tmImpl).filter { abs =>
      abs.nodes.forall(g.contains(typeAbs,_))
    }
    if(absSet.size != 1)
      LoggedError(log + s" one abstraction required ${absSet.size} found")
    else LoggedSuccess(absSet.head)
  }

  private [rules] def redirect
  (g : DependencyGraph,
   oldUse : NodeIdP,
   newUsed : Abstraction) : LoggedTry[(DependencyGraph, Set[NodeIdP])] =
    try LoggedSuccess {
      (newUsed, g usesAccessKind oldUse ) match {
        case (AccessAbstraction(absId, _), None) =>
          (oldUse.changeTarget(g, Uses, absId), Set((oldUse.user, absId)))
        case (ReadWriteAbstraction(Some(rid), _), Some(Read)) =>
          (oldUse.changeTarget(g, Uses, rid), Set((oldUse.user, rid)))
        case (ReadWriteAbstraction(_, Some(wid)), Some(Write)) =>
          (oldUse.changeTarget(g, Uses, wid), Set((oldUse.user, wid)))
        case (ReadWriteAbstraction(Some(rid), Some(wid)), Some(RW)) =>
          oldUse.splitWithTargets(g, rid, wid)
        case _ => throw new PuckError(s"error while redirecting ${(g,oldUse).shows} toward ${(g, newUsed).shows}")
      }
    } catch {
      case e : PuckError => LoggedError(e.getMessage)
    }


  def redirectUsesAndPropagate
  (graph : DependencyGraph,
   oldUse : NodeIdP,
   newUsed : Abstraction
  ): LoggedTG = {

    val g = graph.comment(s"Redirection.redirectUsesAndPropagate(g, ${(graph,oldUse).shows}, ${(graph, newUsed).shows})")

    val oldUsed = g.getConcreteNode(oldUse.used)
    val oldKindType = oldUsed.kind.kindType
    val newKindType = newUsed.kindType(g)

    val ltg : LoggedTG = (oldKindType, newKindType) match {
      case (TypeDecl, TypeDecl) =>
        val AccessAbstraction(newUsedId, _) = newUsed
        redirectInstanceUsesAndPropagate(g, oldUse, newUsedId)

      case (InstanceValue, InstanceValue) =>
//        if(oldUsed.kind.isWritable)
//          redirectUsesOfWritableNodeAndPropagate(g, oldUse, newUsed.containerIn(g).get)
//        else
          redirectInstanceUsesAndPropagate(g, oldUse, newUsed.containerIn(g).get)

      case (TypeConstructor, InstanceValue) =>
        redirectTypeConstructorToInstanceValueDecl(g, oldUse, newUsed)()

      case (StableValue, StableValue) =>
        redirect(g, oldUse, newUsed).map(_._1)
      case (TypeConstructor, StableValue) =>
        redirect(g, oldUse, newUsed).flatMap {
          case (g1, _) =>
            updateTypeUseConstraintForTypeConstructorRedirect(g1, oldUse, newUsed)
        }

      case (kt1, kt2) =>
        LoggedError(s"redirection of type $kt1 to $kt2 unhandled")
    }

    val log = s"redirect uses and propagate from $oldKindType to $newKindType\n"
    log <++: ltg
  }

  def updateTypeUseConstraintForTypeConstructorRedirect
  (g : DependencyGraph,
   ctorUse : NodeIdP,
   newUsed : Abstraction ) : LoggedTG = {
    val (userOfCtor, ctor)= ctorUse
    val ctorTypeUse = (ctor, g container_! ctor)
    val constrainedUseToChange =
      g.typeConstraints(ctorTypeUse) filter ( _.constrainedUser == userOfCtor )
    val AccessAbstraction(absNode, _) = newUsed

    g.typ(absNode) match {
      case NamedType(tid) =>
        constrainedUseToChange.foldLoggedEither(g){
          (g0, ct) =>
            LoggedSuccess(g.removeTypeUsesConstraint(ctorTypeUse, ct)
              .addTypeUsesConstraint((absNode, tid), ct))
        }
      case _ => LoggedError("gen type factory type constraint change unhandled : TODO")
    }
  }

  def redirectTypeConstructorToInstanceValueDecl
  (g : DependencyGraph,
   ctorUse : NodeIdP,
   newUsed : Abstraction )
  ( createVarStrategy: CreateVarStrategy = CreateTypeMember(g.nodeKindKnowledge.defaultKindForNewReceiver)
  ) : LoggedTG = {
    val AccessAbstraction(absNode, _) = newUsed

    val g1 = ctorUse.changeTarget(g, Uses, absNode)

    import g.nodeKindKnowledge.intro

    val typeOfNewReveiver = g.container(absNode).get
    val (userOfCtor, _)= ctorUse

    val ltg = createVarStrategy match {
      case CreateParameter =>
        val decl = g1.getConcreteNode(g1.container_!(userOfCtor))

        intro.parameter(g1, typeOfNewReveiver, decl.id) map {
          case (pNode, g2) =>
            g2.addBinding((pNode.id, typeOfNewReveiver), (userOfCtor, absNode))
        }

      case CreateTypeMember(kind) =>
        intro.typeMember(g1, typeOfNewReveiver,
          g.hostTypeDecl(userOfCtor), kind) map {
          case (newTypeUse, g2) =>
            intro.addUsesAndSelfDependency(
              g2.addBinding(newTypeUse, (userOfCtor, absNode)),
              userOfCtor, newTypeUse.user)
        }
    }
    ltg flatMap (updateTypeUseConstraintForTypeConstructorRedirect(_, ctorUse, newUsed))


  }

  //def propagateParameterTypeConstraints

  def propagateTypeConstraints
  (g : DependencyGraph,
   oldTypeUse : NodeIdP,
   newTypeToUse : NodeId) : LoggedTG = {
    val log = s"propagateTypeConstraints old Type Use = ${(g, oldTypeUse).shows} new type to use = ${(g, newTypeToUse).shows}"
    //    import puck.util.Debug._
    //    println("propagateTypeConstraints")
    //    (g, g.edges).println
    val (_, oldTypeUsed) = oldTypeUse

    val tucs = g.typeConstraints(oldTypeUse)

    val (tucsThatNeedPropagation, tucsThatNeedUpdate) =
      if (g.isa_*(oldTypeUsed, newTypeToUse))
        tucs.partition{
          case Sup(_) | Eq(_) => true
          case Sub(_) => false
        }
      else if (g.isa_*(newTypeToUse, oldTypeUsed))
        tucs.partition{
          case Sub(_) | Eq(_) => true
          case Sup(_) => false
        }
      else
        tucs.partition {
          case Eq(_) => true
          case Sub(_) | Sup(_) => false
        }

    val tucsString = (for{ ct <- tucs.toList } yield
      (g,(oldTypeUse, ct)).shows) mkString "\n"
    val tucsThatNeedPropagationString =
      (for{ ct <- tucsThatNeedPropagation.toList } yield
        (g,(oldTypeUse, ct)).shows) mkString ("\n", "\n", "")
    val log2 = s"\nall constraints = {$tucsString}\ntucsThatNeedPropagation : {$tucsThatNeedPropagationString}\n"


    val logTabsId =
      g.abstractions(oldTypeUsed).find (abs => abs.nodes contains newTypeToUse) match {
        case Some(abs@AccessAbstraction(absId, _)) => LoggedSuccess(absId)
        case Some(ReadWriteAbstraction(_, _)) => LoggedError("!!! type is not supposed to have a r/w abs !!!")
        case None =>
          if(oldTypeUsed != newTypeToUse)
            LoggedError(s"${(g, newTypeToUse).shows} is not an abstraction of ${(g, oldTypeUsed).shows} ")
          else LoggedSuccess(newTypeToUse)
      }

    (log  + log2) <++: ( logTabsId flatMap {
      absId =>

        def update(g : DependencyGraph, tuc : TypeUseConstraint) : DependencyGraph =
          g.removeTypeUsesConstraint(oldTypeUse, tuc)
            .addTypeUsesConstraint((oldTypeUse.user, absId), tuc)

        val g1 = tucsThatNeedUpdate.foldLeft(g)(update)

        val (tucsInvolvingTypeVariables, tucsToPropagate) =
          tucsThatNeedPropagation.partition(tuc => g.kindType(tuc.constrainedType) == TypeVariableKT)

        tucsToPropagate.foldLoggedEither(g1) {
          case (g0, tuc) =>
            val (s, t) = tuc.constrainedUse
            //LoggedSuccess( update(g0, tuc) )
            val g1 = update(g0, tuc)
            log <++: (if( t == absId ) LoggedSuccess(g1)
            else redirectInstanceUsesAndPropagate(g1, (s,t), absId))
        }
    })
  }


  def propagateParameterTypeConstraints
  (g : DependencyGraph,
   typeMember : NodeId,
   abs : Abstraction) = {

    val parameters = g parametersOf typeMember
    if(parameters.isEmpty) g
    else {
      val AccessAbstraction(absId,_) = abs
      val absParameters = g parametersOf absId
      val paramsConstraints = for {
        pids <- parameters zip absParameters
        (pid, absPid) = pids
        tid <- g usedBy pid
        ct <- g.typeConstraints((pid, tid))
      } yield (pid, absPid, tid, ct)
      ()

      paramsConstraints.foldLeft(g) {
        case (g01, (pid, absPid, tid, tuc)) =>
          val g02 = g01.addTypeUsesConstraint((absPid, tid), tuc)
          if(g.usersOf(typeMember).intersect(g.usersOf(tuc.constrainedUser)).isEmpty)
            g02.removeTypeUsesConstraint((pid, tid), tuc)
          else g02
      }
    }
  }

  def redirectTypeMemberUses
  ( g : DependencyGraph,
    oldTypeUses : NodeIdP,
    brSet : Set[(NodeIdP, NodeIdP)], // \forall p in brSet, p._1 == oldTypeUses
    newTypeToUse : NodeId
  ): LoggedTG = {
    val newTypeUse = (oldTypeUses.user, newTypeToUse)
    "redirectTypeMemberUses:\n" <++:
      brSet.foldLoggedEither(g) {
        case (g00, (_, tmu)) =>

          for {
            abs <- tmAbstraction(g, newTypeToUse, tmu.used)
            gNewTmus <- redirect(g00, tmu, abs)
            (g01, nTmus) = gNewTmus
          } yield {
            val g02 = g01.removeBinding(oldTypeUses, tmu)
              .changeTypeUseForTypeMemberUseSet(oldTypeUses, newTypeUse, nTmus)
            propagateParameterTypeConstraints(g02, tmu.used, abs)
          }
      }
  }

//  def redirectUsesOfWritableNodeAndPropagate
//  (g : DependencyGraph,
//   oldUse : NodeIdP,
//   newTypeToUse : NodeId
//  ) : LoggedTG = {
//
//    val log = s"redirectUsesOfWritableNodeAndPropagate(g, oldUse = ${(g, oldUse).shows},  " +
//      s"newTypeToUse = ${(g, newTypeToUse).shows})\n"
//
//    val typeUses = g.typeUsesOf(oldUse)
//
//    val ltg : LoggedTG =
//      typeUses.foldLoggedEither(g){
//        case (g1, tu) =>
//          redirectTypeMemberUses(g1, tu, Set((tu, oldUse)), newTypeToUse)
//      } flatMap {
//        g1 =>
//          typeUses.toList.foldLoggedEither(g1) {
//            propagateTypeConstraints(_,_, newTypeToUse)
//          }
//      }
//    log <++: ltg
//  }



  def redirectInstanceUsesAndPropagate
  (g : DependencyGraph,
   oldUse : NodeIdP,
   newTypeToUse : NodeId
  ) : LoggedTG = {

    val log = s"redirectInstanceUsesAndPropagate(g, oldUse = ${(g, oldUse).shows},  " +
      s"newTypeToUse = ${(g, newTypeToUse).shows})\n"

    lazy val qualifyingSet = cl(g, oldUse)
    val log2 = qualifyingSet map (n => (g,n).shows) mkString(" type members TRset = {", "\n", "}\n")

    val ltg : LoggedTG =
      if(qualifyingSet.nonEmpty) {
        val groupedByTypeUse = qualifyingSet.groupBy(_._1)

        groupedByTypeUse.toList.foldLoggedEither(g comment log) {
          case (g0, (tu, brSet)) =>
            val g1 = tu.changeTarget(g0, Uses, newTypeToUse)
            redirectTypeMemberUses(g1, tu, brSet, newTypeToUse)
        } flatMap {
          g1 =>
            groupedByTypeUse.keys.toList.foldLoggedEither(g1) {
              propagateTypeConstraints(_,_, newTypeToUse)
            }
        }
      }
      else // on redirige un type
        g.kindType(oldUse.used) match {
          case TypeDecl =>
            propagateTypeConstraints(oldUse.changeTarget(g, Uses, newTypeToUse), oldUse, newTypeToUse)
          case _ => LoggedError(s"${(g, oldUse).shows} empty qualyfing set, type redirection is expected")
        }

    (log + log2) <++: ltg
  }


}
