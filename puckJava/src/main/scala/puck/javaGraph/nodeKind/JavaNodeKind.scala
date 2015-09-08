package puck.javaGraph
package nodeKind

import puck.graph._
import puck.graph.constraints.AbstractionPolicy
import ShowDG._
import puck.graph.transformations.rules.Intro
import puck.javaGraph.transformations.JavaIntro

abstract class JavaNodeKind extends NodeKind {
  /*def packageNode : AGNode[JavaNodeKind] =
   this match {
     case Package(id) => this.node
     case _ => this.node.container.kind.packageNode
   }*/
}

case object Definition extends JavaNodeKind {
  override def canContain(k: NodeKind): Boolean = false

  override def abstractionNodeKinds(p: AbstractionPolicy): Seq[NodeKind] = Seq()

  override def kindType: KindType = ValueDef
}

case object TypeVariable extends JavaNodeKind {
  def kindType : KindType = InstanceTypeDecl
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}
case object WildCardType extends JavaNodeKind {
  def kindType : KindType = InstanceTypeDecl
  def canContain(k : NodeKind) = false
  override def abstractionPolicies = Seq()
  def abstractionNodeKinds(p : AbstractionPolicy) = Seq()
}

case object Param extends JavaNodeKind {

  def canContain(k: NodeKind): Boolean = false
  def abstractionNodeKinds(p: AbstractionPolicy): Seq[NodeKind] = Seq()
  def kindType: KindType = Parameter
}

object JavaNodeKind extends NodeKindKnowledge {

  //import AccessGraph.dummyId
  /*def packageKind = Package(dummyId)
  def interface = Interface(dummyId, None)*/
  def classKind = Class
  //fix for accessing the field in java
  def interfaceKind = Interface



  def field = Field
  def staticField = StaticField
  def constructor = Constructor
  def abstractMethod = AbstractMethod
  def method = Method
  def staticMethod = StaticMethod

  def parameter = Param

  def definition = Definition

  def primitive = Primitive
  def typeVariable = TypeVariable
  def wildcardType = WildCardType

  def noType : Option[Type] = None

  def rootKind : NodeKind = JavaRoot

  def lightKind : NodeKind = Interface


  def kindOfKindType(kindType: KindType) : Seq[NodeKind] =
    kindType match {
      case NameSpace => Seq(Package)
      case TypeConstructor => Seq(Constructor)
      case TypeDecl => Seq(Interface, Class)
      case InstanceValueDecl => Seq(Field, Method)
      case InstanceTypeDecl => Seq(Interface, Class)
      case StaticValueDecl => Seq(StaticField, StaticMethod)
      case Parameter => Seq(Param)
      case ValueDef => Seq(Definition)
      case UnknownKindType => sys.error("Unknown kind type")
    }



  val nodeKinds = Seq[NodeKind](JavaRoot, Package, Interface,
    Class, Constructor, Method, /*ConstructorMethod,*/
    Field, AbstractMethod, Literal, Primitive)

  def concreteNodeTestPred(graph : DependencyGraph, nid : NodeId)
                          (pred: ConcreteNode => Boolean): Boolean =
    graph.getNode(nid) mapConcrete (pred, false)

  override def canContain(graph : DependencyGraph)
                         (n : DGNode, other : ConcreteNode) : Boolean = {
    val id = n.id

    def noNameClash( l : Int )( cId : NodeId ) : Boolean =
      concreteNodeTestPred(graph, cId){ c =>
        (c.kind, graph.structuredType(c.id)) match {
          case (ck: MethodKind, Some(Arrow(Tuple(input), _)))=>
            c.name != other.name || input.length != l
          case (ck: MethodKind, _)=>
            throw new DGError((graph, c).shows)
          case _ => true
        }
      }

    def implementMethod
    ( absMethodName : String,
      absMethodType : Arrow)(id : NodeId) : Boolean =
      graph.content(id).exists(concreteNodeTestPred(graph, _) { c =>
        (c.kind, graph.structuredType(c.id)) match {
          case (Method, Some(mt @ Arrow(_, _))) =>
            absMethodName == c.name && absMethodType == mt
          case (Method, _) => throw new DGError()
          case _ => false
        }
      })

    super.canContain(graph)(n, other) &&
      ( (other.kind, graph.structuredType(other.id)) match {
        case (AbstractMethod, Some(absMethodType @ Arrow(Tuple(input), _))) =>
          graph.content(id).forall(noNameClash(input.length)) &&
            graph.directSubTypes(id).forall {implementMethod(other.name, absMethodType)}

        case (AbstractMethod, _) => throw new DGError(other + " does not have a MethodTypeHolder")
        /* cannot have two methods with same name and same type */
        case (Method, Some(Arrow(Tuple(input), _))) =>
          graph.content(id).forall(noNameClash(input.length))

        case (Method, st) =>
          throw new DGError(s"canContain(${(graph, id).shows}, ${(graph, other.id).shows}) $st")
        case _ => true
      })
  }

  def defaultKindForNewReceiver : NodeKind = Field

  val initializerKind : NodeKind = Method

  val intro : Intro = JavaIntro

  def getConstructorOfType(g: DependencyGraph, tid : NodeId) : Option[NodeId] = {
    g.content(tid).find {
      cid =>
        val n = g.getConcreteNode(cid)
        n.kind match {
          case Constructor => g.parameters(cid).isEmpty
          case _ => false
        }
    }
  }

  override def writeType(graph: DependencyGraph): Type = {
    val sNode = graph.concreteNodes.find(_.name == "void")
    if(sNode.isEmpty) error("void not loaded")
    else NamedType(sNode.get.id)
  }

  override def structuredType(graph : DependencyGraph, id : NodeId, params : List[NodeId]) : Option[Type] = {
    //assert node is a typed value
    if(params.nonEmpty) super.structuredType(graph, id, params)
    else graph.getNode(id).kind match {
      case Method => Some(Arrow(Tuple(), graph styp id get))
      case _ => Some(graph styp id get)
    }
  }
}