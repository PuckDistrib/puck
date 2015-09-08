package puck.graph.transformations.rules

import puck.{graph, PuckError}
import puck.graph._

abstract class Intro {
  intro =>

  def initializer
  ( graph : DependencyGraph,
    typeInitialized : NodeId
    ) : (NodeId, DependencyGraph) = {
    val initializedContent =
      graph content typeInitialized filter {
        nid =>
          val n = graph getNode nid
          n.kind.isWritable && n.hasDefinitionIn(graph)
      }

    import graph.nodeKindKnowledge.{writeType, initializerKind}
    val returnType = writeType(graph)

    val (cn, g) = intro.typedNodeWithDef(graph, "init", initializerKind, returnType, mutable)
    val defNode = g definitionOf_! cn.id

    val g2 =
      if(initializedContent.nonEmpty &&
        ! g.uses(typeInitialized,typeInitialized) )
        g.addUses(typeInitialized,typeInitialized)
      else g

    (cn.id,
      initializedContent.foldLeft(g2.addContains(typeInitialized, cn.id)){
        (g, ic) =>
          val g1 = g.addUses(defNode, ic, Some(Write))
                    .addUsesDependency((typeInitialized,typeInitialized), (defNode, ic))

          val icDef = g definitionOf_! ic

          val g2 = g.usedBy(icDef).foldLeft(g1){
            (g0, usedByIcDef) =>
              g0.getUsesEdge_!(icDef, usedByIcDef).changeSource(g0, defNode)
          }
          val (_, g3) = g2.removeEdge(ContainsDef(ic, icDef)).removeNode(icDef)

          g3
      })

  }

  def apply
  ( graph : DependencyGraph,
    localName : String,
    kind : NodeKind,
    mutable : Mutability = true
    ) : (ConcreteNode, DependencyGraph) =
    graph.addConcreteNode(localName, kind, mutable)

  def typedNodeWithDef
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typeNode : NodeId,
   mutable : Mutability = true
    )  : (ConcreteNode, DependencyGraph) =
    typedNodeWithDef(graph, localName, kind, NamedType(typeNode), mutable)

  def typedNodeWithDef
  (graph : DependencyGraph,
   localName: String,
   kind : NodeKind,
   typ : Type,
   mutable : Mutability
    )  : (ConcreteNode, DependencyGraph)

  def parameter
  ( g : DependencyGraph,
    typeNode : NodeId,
    typeMemberDecl : NodeId
    ) : LoggedTry[(ConcreteNode, DependencyGraph)] = {

    val newTypeUsedNode = g.getConcreteNode(typeNode)
    val paramKind = g.nodeKindKnowledge.kindOfKindType(Parameter).head
    val (pNode, g1) = g.addConcreteNode(newTypeUsedNode.name.toLowerCase, paramKind)


    g1.getDefaultConstructorOfType(typeNode) match {
      case None => LoggedError(new PuckError(s"no default constructor for $typeNode"))
      case Some(cid) =>
        LoggedSuccess {
          val g2 = g1.addParam(typeMemberDecl, pNode.id)
            .setType(pNode.id, Some(NamedType(typeNode)))

          (pNode,
            g1.usersOf(typeMemberDecl).foldLeft(g2) {
              (g0, userOfUser) =>
                g0.addEdge(Uses(userOfUser, cid))
            })
        }
    }


  }

  def typeMember
  (g : DependencyGraph,
   typeNode : NodeId,
   tmContainer : NodeId,
   kind : NodeKind
    ) : LoggedTry[(DGUses, DependencyGraph)] = {
    g.getDefaultConstructorOfType(typeNode) match {
      case None => LoggedError(new PuckError(s"no default constructor for $typeNode"))
      case Some(constructorId) =>

        val delegateName = s"${g.getConcreteNode(typeNode).name.toLowerCase}_delegate"

        val (delegate, g1) = typedNodeWithDef(g, delegateName, kind, typeNode)

        val newTypeUse = Uses(delegate.id, typeNode)

        val tmContainerKind = g.getConcreteNode(tmContainer).kind
        if(tmContainerKind canContain kind)
          LoggedSuccess(
            (newTypeUse,
              g1.addContains(tmContainer, delegate.id)
                .addEdge(newTypeUse) //type field
                .addEdge(Uses(g1 definitionOf_! delegate.id, constructorId))))
        //.addEdge(Uses(tmContainer, delegate.id, Some(Write)))
        else {
          val msg =s"$tmContainerKind cannot contain $kind"
          LoggedError(new PuckError(msg), msg)
        }

    }
  }

  def addUsesAndSelfDependency
  (g : DependencyGraph,
    user : NodeId,
    used : NodeId) : DependencyGraph =
    (g.kindType(g.container_!(user)), g.kindType(used)) match {
      case (InstanceValueDecl, InstanceValueDecl)
      if g.containerOfKindType(TypeDecl, user) == g.containerOfKindType(TypeDecl, used) =>
        val cter = g.containerOfKindType(TypeDecl, user)
        val g1 =
          if (Uses(cter, cter) existsIn g) g
          else g.addUses(cter, cter)

        g1.addUses(user, used)
          .addUsesDependency((cter, cter), (user,used))
      case _ => g.addUses(user, used)

  }
}
