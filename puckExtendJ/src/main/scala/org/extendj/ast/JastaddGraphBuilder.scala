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

package org.extendj.ast

import puck.graph._
import puck.javaGraph.nodeKind._
import puck.javaGraph.JavaGraphBuilder

import scala.{List => SList}
import scala.collection.JavaConversions._

object JastaddGraphBuilder {

  def definitionName = DependencyGraph.definitionName


  def qualifierIsThisAccess(access : TypeMemberAccess) : Boolean =
    (!access.isQualified) &&
      access.decl().hostType() == access.hostType() ||
      access.isQualified &&
        access.qualifier.isThisAccess


  def qualifierIsSuperAccess(access : TypeMemberAccess) : Boolean =
    (!access.isQualified) &&
      access.decl().hostType() != access.hostType() ||
      access.isQualified &&
        access.qualifier.isSuperAccess


  type JASTNode = ASTNode[_ <: ASTNode[_]]


  def apply(p : Program,
            sll : Option[puck.LoadingListener]) : JastaddGraphBuilder = {
    val builder = new JastaddGraphBuilder(p)

    val nchild = p.getCompilationUnitList.getNumChild.toDouble //getNumCuFromSources();
    var d = 0d

    builder.addPrimitiveTypes(p)
    import scala.collection.JavaConversions._
    p.getCompilationUnitList foreach {
      cu =>
        cu buildDependencyGraph builder
        sll foreach {
          ll =>
            d += 1
            ll.update(d/nchild)
        }
    }
    sll foreach (_.update(1))

    builder
  }
}



class JastaddGraphBuilder(val program : Program)
  extends JavaGraphBuilder
    with TypeUsage
    with GraphBuilderVisitor
    with Registration
    with NodeFactory {
  var graph2ASTMap = Map[Int, ASTNodeLink]()

  var synthetic = Set[NodeId]()

  def addUses(user : NodeId, used : NodeId) =
    addEdge(Uses(user, used))

  def findTypeDecl(typ : String): TypeDecl ={
    val td = program findType typ
    if(td == null)
      throw new DGBuildingError(typ + " not found")
    td
  }

  def onCreate(n : DGNamedElement) : NodeId => Unit = {
    nid =>
      if(!n.fromSource) {
        fromLibrary += nid
    //    println(nid+"-"+getFullName(nid)+" is in fromLibrary")  // added by Cedric
      } else if(n.isSynthetic)
        synthetic += nid
  }

  //to handle name clash between local variables scopes
  //and keep a one level depth in methods we use a tmp localVarMap
  var localVarMap : Map[DGNamedElement, NodeId] = _
  var localVarCounter = 0
  def resetLocal() = {
    localVarMap = Map()
    localVarCounter = 0
  }
  def localVarName() = {
    val n = localVarCounter.toString
    localVarCounter += 1
    n
  }
  def getLocalVarNode(n : DGNamedElement) : NodeId =
    localVarMap get n match {
      case Some(id) => id
      case None =>
        val name = localVarName()
        val parentName = n.getParentNamedNode.dgFullName()

        val id = super.addNode (parentName + ".Definition." + name,
          name, nodeKind(n)) {
          nid => n.registerNode (this, nid)
            onCreate (n) (nid)
        }
        localVarMap += (n -> id)
        id
  }

  def getNode(n : DGNamedElement): NodeId = nodeKind(n) match {
    case LocalVariable =>
      getLocalVarNode(n)
    case Param if n.getParent.isInstanceOf[CatchClause] =>
      getLocalVarNode(n)
    case k =>
      super.addNode (n.dgFullName(), n.name (), k) {
        nid => n.registerNode (this, nid)
        onCreate (n) (nid)
      }
  }


  import JastaddGraphBuilder.definitionName
  def getDefNode(n : DGNamedElement): NodeId =
    super.addNode(n.dgFullName() + "." + definitionName, definitionName, Definition)(onCreate(n))


  def attachOrphanNodes(fromId : Int = g.rootId) : Unit = {
    val lastId = g.numNodes - 1
    if(fromId < lastId){
      for(nodeId <- Range.inclusive(fromId, lastId) ){
        //println(s"${g.container(nodeId)} contains $nodeId")
        if(g.container(nodeId).isEmpty && nodeId != g.rootId){
          //val n = g.getNode(nodeId)
          //println(s"orphan node : ${g.fullName(nodeId)}  : ${n.kind}")
          graph2ASTMap get nodeId match {
            case Some(FieldDeclHolder(d, _)) => addBodyDecl(d)
            case Some(MethodDeclHolder(d)) => addBodyDecl(d)
            case Some(EnumConstantHolder(d)) => addBodyDecl(d)
            case Some(ConstructorDeclHolder(cdecl)) =>
              nodesByName get cdecl.hostType().dgFullName() match {
                case Some(pid) => cdecl.buildDG(this, pid)
                case _ => addBodyDecl(cdecl)
              }
            case Some(TypedKindDeclHolder(decl)) =>
              addApiTypeNode(decl)
            case Some(ParameterDeclHolder(d)) =>
            //println("attaching " + d.dgFullName())
            //addBodyDecl(d.hostBodyDecl())
            case sdh =>
              println( g.fullName(nodeId) + " " + sdh + " attach orphan nodes unhandled case")
              ()
          }
        }
      }
      //addBodyDecl (case MethodDeclHolder) can add new typeNodes
      attachOrphanNodes(lastId)
    }
  }

  def buildDGType(thisId : NodeId, decl : FieldDeclarator) : Unit = {
    setType(thisId,  getType(decl.getTypeAccess))
  }

  def buildDGType(thisId : NodeId, decl : ConstructorDecl) : Unit =  {
    astParamsToDGType(thisId, decl)
    setType(thisId, NamedType(this buildNode decl.hostType()))
  }

  def buildDGType(thisId : NodeId, decl : MethodDecl) : Unit =  {
    astParamsToDGType(thisId, decl)
    setType(thisId, getType(decl.getTypeAccess))
  }

  def astParamsToDGType(thisId : NodeId, c : Callable ) : Unit =
    addParams(thisId, c.getParameterList.foldRight(SList[NodeId]()) {
        case (pdecl, params) =>

          val paramId = this buildNode pdecl

          pdecl.getTypeAccess.lock()

          setType(paramId, getType(pdecl.`type`()))
          paramId :: params
      })



  def addApiNode(nodeKind : String, typ : String, bodydeclName: String) : Unit = {
    //println("trying to add type " + typ + " " + bodydeclName+ " ... ")

    val td = findTypeDecl(typ)
    val typeNodeId = addApiTypeNode(td)

    def addBodyDecl(bd : BodyDecl) : Unit = {
      if(bd == null)
        System.err.println("Method or constructor" + bodydeclName + " not found in the program ...")
      else
        bd.buildDG(this, typeNodeId)
    }

    nodeKind match {
      case "type" => ()
      case "method" => addBodyDecl (td findMethodBySig bodydeclName)
      case "constructor" => addBodyDecl (td findConstructorBySignature ("#_" + bodydeclName))

      case _ => System.err.println("node kind unknown")
    }
  }

  //a.b.c()
  //typeMemberUse, used is c
  //b is qualifier
  //a is qualifier's qualifier


  def addApiTypeNode(td: TypeDecl): NodeId = {
    val tdNode = getNode(td)

    val n = g.getNode(tdNode)

    val cterId =
      if(td.isTopLevelType)
        addPackage(td.packageName(), fromSource = false)
      else
        this buildNode td.getParentNamedNode

    buildInheritence(td)

    if(td.isGenericType)
      buildTypeVariables(tdNode, td.asInstanceOf[GenericTypeDecl])

    if(!td.isInstanceOf[TypeVariable])
      addEdge(Contains(cterId, tdNode))
    //if td is type variable, it will be attache in the right order
    // when parent type will be build


    tdNode
  }

  def addApiTypeNodeAndRegister(td: TypeDecl): Unit =
    registerDecl(addApiTypeNode(td), td)


  def addBodyDecl(bd : BodyDecl) : Unit = {
    val typeNodeId = addApiTypeNode(bd.hostType())
    bd.buildDG(this, typeNodeId)
  }

  def getParamType(parTypeDecl : ParTypeDecl) : Type = {
    val genId = getNode(parTypeDecl.genericDecl())
    val args =
      Range.inclusive(parTypeDecl.numTypeParameter() - 1, 0, -1).foldLeft(scala.List[Type]()) {
        case (l, i) =>
          getType(parTypeDecl.getParameterization.getArg(i)) :: l
      }
    ParameterizedType(genId, args)
  }

  def getType(td : TypeDecl) : Type = td match {
    case parTypeDecl : ParTypeDecl => getParamType(parTypeDecl)
    case wst : WildcardSuperType => Contravariant(getType(wst.getAccess))
    case wet : WildcardExtendsType => Covariant(getType(wet.getAccess))
    case _ => NamedType(getNode(td))
  }

  def getType(a : Access) : Type = {
    a.lock()
    a match {
      case aa: ArrayTypeAccess =>
        ParameterizedType(arrayTypeId, scala.List(getType(aa.getAccess)))
      case bta : BoundTypeAccess => getType(bta.getTypeDecl)
      case ta: TypeAccess => NamedType(getNode(ta.decl()))
      case d: Dot => getType(d.getRight)
      case pta: ParTypeAccess => getType(pta.`type`())
      case we: AbstractWildcard => getType(we.`type`())
      case _ => throw new Error(s"getType, ${a.compilationUnit().pathName()} line ${a.location()} " +
        s"access.getClass == ${a.getClass}")
    }
  }


  def addPrimitiveTypes( p : Program) : Unit = {
    val pcu = p.getPrimitiveCompilationUnit
    SList(pcu.typeBoolean(), pcu.typeByte(), pcu.typeShort(), pcu.typeChar(),
      pcu.typeInt(), pcu.typeLong(), pcu.typeFloat(), pcu.typeDouble(),
      pcu.typeVoid(), pcu.typeNull()) foreach addApiTypeNodeAndRegister
  }


}
