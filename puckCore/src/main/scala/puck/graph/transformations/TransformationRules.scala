package puck
package graph
package transformations

import puck.graph.constraints.SupertypeAbstraction
import puck.graph.transformations.rules._
import puck.util.LoggedEither._
import scalaz.std.list._
import ShowDG._

class TransformationRules
( mergingCandidatesFinder : MergingCandidatesFinder,
  val rename : Renamer,
  val abstracter : Abstract,
  val intro : Intro) {


  def findMergingCandidate = mergingCandidatesFinder.find _
  def mergeMatcherInstances = mergingCandidatesFinder.mergeMatcherInstances

  lazy val merger = new Merge(mergingCandidatesFinder)
  def mergeInto = merger.mergeInto _
  def removeConcreteNode = merger.removeConcreteNode _

  val redirection = Redirection
  val move = Move

  def addHideFromRootException(g : DependencyGraph, node : NodeId, friend : NodeId): DependencyGraph =
    g.newGraph(constraints = g.constraints.addHideFromRootException(g, node, friend))
  /*def addHideFromRootException(node : NIdT, friend : NIdT): GraphT = {
    constraints.printConstraints(g, logger, (PuckLog.InGraph, PuckLog.Debug))
    val ng = newGraph(nConstraints = constraints.addHideFromRootException(g, node,friend))
    ng.printConstraints(ng, logger, (PuckLog.InGraph, PuckLog.Debug))
    ng
  }*/

  def makeSuperType(g: DependencyGraph, sub : NodeId, sup : NodeId) : LoggedTG = {
    val subNode = g.getConcreteNode(sub)
    val supNode = g.getConcreteNode(sup)
    if(!g.canBe(subNode, supNode))
      LoggedError(new PuckError(s"${showDG[NodeId](g).shows(sub)} cannot be ${showDG[NodeId](g).show(sup)}"))
    else {
      val subMethods = g.content(sub).toList map g.getConcreteNode
      val supMethods = g.content(sup).toList map g.getConcreteNode
      Type.findAndRegisterOverridedInList(g, supMethods, subMethods) {
        Type.errorOnImplemNotFound(showDG[NodeId](g).shows(sub))
      } map ( _.addIsa(sub, sup).
                addAbstraction(sub, AccessAbstraction(sup, SupertypeAbstraction))
        ) flatMap {
        g =>
          val overloadedMethod =
            subMethods filter {
              m => g.abstractions(m.id) exists {
                case AccessAbstraction(supMethId, SupertypeAbstraction)
                  if g.contains(sup, supMethId) => true
                case _ => false

              }
            }
          overloadedMethod.foldLoggedEither(g){
            (g0, m) =>
              abstracter.changeSelfTypeUseBySuperInTypeMember(g0, m, subNode, supNode)
          }
      }

    }
  }

}