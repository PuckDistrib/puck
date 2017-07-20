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


import java.io.{File, FileWriter, InputStream}
import java.util.NoSuchElementException

import org.extendj.parser
import puck.config.Config
import puck.graph.ShowDG._
import puck.graph._
import puck.graph.comparison.NodeMappingInitialState
import puck.graph.transformations.{MutabilitySet => _, _}
import puck.graph.transformations.Recording
import puck.graph.transformations.Transformation._
import org.extendj.concretize._
import puck.javaGraph.nodeKind._
import puck.util.PuckLog._
import puck.util.{PuckLog, PuckLogger}
import puck.{DG2AST, DG2ASTBuilder, Project, PuckError}
import puck.graph.MutabilitySet.MutabilitySetOps

import scala.{List => SList}

object JavaJastAddDG2AST
  extends DG2ASTBuilder {

  def buildGraph(p : Program,
                 ll : puck.LoadingListener)  : JavaJastAddDG2AST = {

    val builder = JastaddGraphBuilder(p, Option(ll))
    builder.addEdge(Contains(builder.nodesByName("@primitive"), builder.arrayTypeId))
    builder.attachOrphanNodes()
    val orphanNodes = builder.g.nodesId filter (builder.g.container(_).isEmpty) filter (_ != 0)
    if(orphanNodes.nonEmpty) {
      builder.graph2ASTMap.get(orphanNodes.head) match {
        case Some(ParameterDeclHolder(decl)) =>
          println(decl.fullLocation())
          puck.error(decl.hostBodyDecl().dgFullName() + " !!! ")
        case Some(LocalVarDeclHolder(decl)) =>
          println(decl.fullLocation())
          puck.error(decl.hostBodyDecl().dgFullName() + " !!! ")
        case Some( hn : HasNode) =>
//          (builder.g.nodesId map (id => (id, builder.g.fullName(id))) toList) sortBy (_._1) foreach println
//          println("------------------")
//          builder.nodesByName.toList.sortBy(_._1) foreach println
          puck.error("unregistred " + hn.node)
        case None =>puck.error()
      }
    }



    //builder.registerSuperTypeAbtractions()

    val (_, initialRecord) = NodeMappingInitialState.normalizeNodeTransfos(JavaNodeKind.root.kind,
      builder.g.recording, Seq())

    val g = builder.g.newGraph(recording = Recording())

    new JavaJastAddDG2AST(p, g,
      initialRecord,
      builder.fromLibrary,
      builder.synthetic,
      builder.nodesByName,
      builder.graph2ASTMap)
  }

  def fromFiles(sources: SList[String],
                sourcepaths : SList[String],
                classpaths: SList[String],
                bootclasspaths : SList[String],
                options : SList[(String, String)] = SList(),
                ll : puck.LoadingListener = null)
               (implicit logger : PuckLogger): JavaJastAddDG2AST = {
    val sProg = puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Compiling sources ...")
      compile(sources, sourcepaths, classpaths, bootclasspaths, options)
    }

    puck.util.Time.time(logger, defaultVerbosity) {
      logger.writeln("Building Access Graph ...")
      sProg match {
        case None => throw new DGBuildingError("Compilation error, no AST generated")
        case Some(p) => buildGraph(p, ll)
      }
    }
  }
//compile(sources, sourcepaths, classpaths, bootclasspaths, options)
  def compile(sources: SList[String],
              sourcepaths:SList[String],
              jars: SList[String],
              bootJars : SList[String],
              options : SList[(String, String)]): Option[Program] = {
    val arglist = createArglist(sources, sourcepaths, jars, bootJars, options)
    val f = new Frontend {
      //        protected override def processErrors(errors: java.util.Collection[Problem], unit: CompilationUnit): Unit =  {
      //          System.err.println("Errors:")
      //
      //            val it: Iterator[_] = errors.iterator
      //            while (it.hasNext) {
      //              val i = it.next()
      //              System.err.println(i)
      //            }
      //
      //        }
      protected override def processWarnings(errors: java.util.Collection[Problem], unit: CompilationUnit): Unit = {
      }
    }
    val br = new BytecodeReader() {
      def read(is: InputStream, fullName: String, p: Program) : CompilationUnit = {
        new BytecodeParser(is, fullName).parse(null, null, p)
      }
    }

    val jp = new JavaParser() {
      override def parse(is: InputStream, fileName: String) : CompilationUnit = {
        new parser.JavaParser().parse(is, fileName)
      }
    }

    if (f.run(arglist, br, jp) == 0){
      Some(f.getProgram)}
    else
      None
  }


  def compile(code : String) : Option[Program] = {
    import puck.util.FileHelper.FileOps
    val f = new File(System.getProperty("java.io.tmpdir")) \ "Tmp.java"
    val fw = new FileWriter(f)
    fw.write(code)
    fw.close()

    compile(SList(f.getAbsolutePath),SList(),SList(),SList(),SList())
  }

  def compile(cus : SList[String]) : Option[Program] = {
    import puck.util.FileHelper.FileOps
    val fs = cus.zipWithIndex map { case (code, idx) =>
      val f = new File(System.getProperty("java.io.tmpdir")) \ s"Tmp$idx.java"
      val fw = new FileWriter(f)
      fw.write(code)
      fw.close()
      f.getAbsolutePath
    }

    compile(fs, SList(),SList(),SList(),SList())
  }


  /*  try {
      val noSource = "<no source>"
      val noPath = "<no path>"
      val p = new Program()
      p initBytecodeReader Program.defaultBytecodeReader()
      val parser = Program.defaultJavaParser()
      p initJavaParser parser

      val cu0 = parser.parse(
        new ByteArrayInputStream(code.getBytes("UTF-8")), noPath)

      p addCompilationUnit cu0

      val cu = p getCompilationUnit 0
      cu.setClassSource(new FileClassSource(new SourceFolderPath(noSource), noPath))
      cu setFromSource true
      Some(p)
    } catch {
      case _ : Exception => None
    }*/




  private def createArglist(sources: SList[String],
                            sourcepaths:SList[String],
                            jars: SList[String],
                            bootClassPath : SList[String],
                            options : SList[(String, String)]): Array[String] = {

    def prepend(argName : String, argValue : SList[String], accu : SList[String]) : SList[String] =
      if(argValue.isEmpty) accu
      else argName :: argValue.mkString(File.pathSeparator) :: accu

    val args0 = prepend("-classpath", jars, sources)
    val args1 = prepend("-sourcepath", sourcepaths, args0)
    val args2 = prepend("-bootclasspath", bootClassPath, args1)
    options.foldLeft(args2){
      case (args, (name, value)) =>
        prepend(name, SList(value), args)
    }.toArray
  }


  def apply
  (p : Project,
   logger : PuckLogger,
   ll : puck.LoadingListener = null
  ) : JavaJastAddDG2AST = {

    import Config.Keys

    fromFiles(p pathList Keys.srcs,
      p pathList Keys.sourcepaths,
      p pathList Keys.classpath,
      p pathList Keys.bootclasspath)(logger)

  }

  def verbosity : PuckLog.Level => PuckLog.Verbosity = l => (PuckLog.AG2AST, l)
}

class JavaJastAddDG2AST
( val program : Program,
  val initialGraph : DependencyGraph,
  val initialRecord : Seq[Transformation],
  val fromLibrary : Set[NodeId],
  val synthetic : Set[NodeId],
  val nodesByName : Map[String, NodeId],
  val graph2ASTMap : Map[NodeId, ASTNodeLink]) extends DG2AST {

  def mutableNodes : Iterable[NodeId] =
    initialGraph.nodesId.filterNot(nid => nid == 0 ||
      (fromLibrary contains nid) ||
      (synthetic contains nid)
    )

  def initialMutability : MutabilitySet =
  MutabilitySet.allImmutable(initialGraph).setMutability(mutableNodes, Mutable)


  implicit val p = program

  implicit val defaultVerbosity = (PuckLog.AG2AST, PuckLog.Info)

  def safeGet
  ( graph : DependencyGraph,
    id2declMap : Map[NodeId, ASTNodeLink])
  ( id : NodeId ) : ASTNodeLink =
    try id2declMap(id)
    catch {
      case e : NoSuchElementException =>
        val n = graph getNode id
        if(n.kind == Package)
          PackageDeclHolder
        else NoDecl
    }

  def astNodeOf(graph : DependencyGraph, id : NodeId) : ASTNodeLink =
    safeGet(graph, graph2ASTMap)(id)

  def code(graph : DependencyGraph, id : NodeId) : String =
    astNodeOf(graph,id) match {
      case hn : HasNode => hn.node.prettyPrint()
      case n => n.toString
    }



  def apply(graph : DependencyGraph)(implicit logger : PuckLogger) : Unit = {

    logger.writeln("applying change !")
    val record = graph.recording

//    logger.writeln("before applying change : ")
//    logger.writeln(program.prettyPrint())

    record.reverse.foldLeft((graph, initialGraph, graph2ASTMap)) {
      case ((resultGraph, reenactor, g2AST), t : Transformation) =>

        logger.writeln("applying " + (reenactor, t).shows)
        val newG2AST = applyOneTransformation(g2AST, t)(logger,
          (resultGraph, reenactor))

        (resultGraph, t.redo(reenactor), newG2AST)

      case (acc, op) =>
        logger.writeln((acc._1, op).shows)
        acc
    }

//    logger.writeln("change applied : ")
//    logger.writeln(program.prettyPrint())
    logger.writeln("unlocking")
    try {
      program.eliminateLockedNamesInSubtree()
    }
    catch {
      case e : Exception =>
        e.printStackTrace()
    }
    logger.writeln("done")

//    logger.writeln("Program after unlock : ")
//    logger.writeln(program.prettyPrint())
//    logger.writeln("Program after unlock end of print ")


  }

  def printCode(dir : File)(implicit logger : PuckLogger) : Unit =
    program.printCodeInDirectory(dir)

  val discardedOp : Operation => Boolean = {
    case _ : Comment
         | _ : TypeBinding
         | _ : AbstractionOp => true
    case _ => false
  }

  def applyOneTransformation
  ( id2declMap: Map[NodeId, ASTNodeLink],
    t: Transformation)
  ( implicit logger : PuckLogger,
    resultAndReenactor : (DependencyGraph, DependencyGraph)): Map[NodeId, ASTNodeLink] = {
    val (resultGraph, reenactor) = resultAndReenactor
    implicit val mapping : NodeId => ASTNodeLink = safeGet(resultGraph, id2declMap)

    val tstr = (reenactor, t).shows
    t match {
      case Add(CNode(n)) =>
        //redo t before createDecl
        if (n.kind == Definition) id2declMap
        else {
          val newMap = id2declMap get n.id match {
            case Some(_) => id2declMap
            case None => CreateNode(program, resultGraph, id2declMap, n)
          }
          newMap
        }
      case Add(Edge(Contains(source, target)))
        if reenactor.kindType(target) == ValueDef =>
        CreateNode.addDef(program, resultGraph, id2declMap, source, target)

      case _ =>
        lazy val noApplyMsg = s"${(resultGraph, t).shows} not applied"

        t match {
          case Add(Edge(e)) => CreateEdge(e)
          case Add(Isa(sub, sup)) =>
            CreateEdge.createIsa(id2declMap, sub, sup)
          case Remove(Isa(sub, sup)) =>
              (id2declMap(Type mainId sub), id2declMap(Type mainId sup)) match {
                case (ClassDeclHolder(srcDecl), TypedKindDeclHolder(oldDecl)) =>
                  srcDecl removeSuperType oldDecl
                case (InterfaceDeclHolder(srcDecl), InterfaceDeclHolder(oldDecl)) =>
                  srcDecl removeSuperInterface oldDecl
                case _ =>
                  puck.error(s"${(resultGraph, t).shows} : application failure !")
              }

          case Add(AType(user, newType)) =>
            CreateEdge.createTypeUse(resultGraph, safeGet(resultGraph, id2declMap), user, newType)

          case ChangeSource(Contains(source, target), newSource) =>
            RedirectSource.move(source, target, newSource)

          case ChangeSource(Uses(source, target), newSource)  =>
            RedirectSource.changeUser(source, target, newSource)

          case ChangeSource(IsaEdge(source, target), newSource)  => puck.error()

          case ChangeTarget(e, newTarget) => RedirectTarget(e, newTarget)

          case ChangeType(typed, oldType, newType) =>
            RedirectTarget.changeType(typed, oldType, newType)

          // TODO see if can be performed in add node instead
          case Add(AbstractionOp(impl, AccessAbstraction(abs, SupertypeAbstraction))) =>
            (id2declMap get impl, reenactor.getConcreteNode(abs).kind) match {
              case (Some(MethodDeclHolder(decl)), AbstractMethod) =>
                decl.setVisibility(ASTNode.VIS_PUBLIC)
              case _ => ()
            }

          case Remove(CNode(n)) =>
            id2declMap get n.id foreach {
              case dh: TypedKindDeclHolder => dh.decl.puckDelete()
              case bdh: HasBodyDecl => bdh.decl.puckDelete()
              case PackageDeclHolder => ()

              case BlockHolder(_)
                   | ParameterDeclHolder(_)
                   | ExprHolder(_) => () //removed when containing decl is removed
              case NoDecl => throw new PuckError(noApplyMsg)
            }

          case Remove(Edge(ContainsParam(method, paramId))) =>
            (id2declMap get method, id2declMap get paramId) match {
              case (Some(MethodDeclHolder(m)), Some(ParameterDeclHolder(param))) =>
                val index = param.getChildIndex
                m removeParameter index
                m.getBlock removeParameterAccess param
                reenactor.usersOf(method) map id2declMap.get foreach {
                  case Some(hn : HasNode) => hn.node.removeParameter(m,index)
                  case _ => ()
                }

              case _ => logger.writeln(noApplyMsg + " : case not handled")
            }

          case Rename(nid, newName) =>
            //            implicit val nodeLinks : NodeId => ASTNodeLink =
            //              safeGet(reenactor, id2declMap) _
            ASTNodeLink.setName(newName, reenactor, nid)
            mapping(nid) match {
              case decl : HasNode =>
                reenactor.usersOf(nid) foreach {
                  mapping(_) match {
                    case user : HasNode =>
                      user.node.rename(decl.node, newName)
                    case _ => ()
                  }

                }
              case PackageDeclHolder => ()
              case NoDecl =>
                logger.writeln("tried to apply rename but no decl was found")
            }

          case ChangeTypeBinding((tUser, tUsed), tmUse @ (tmUser, tmUsed), newTuse@(ntUser, ntUsed)) =>
            if (tUser == tUsed) {
              if (ntUser != ntUsed && !reenactor.isa_*(ntUsed, ntUser) && !reenactor.isa_*(ntUser, ntUsed))
                createVarAccess(reenactor, safeGet(reenactor, id2declMap), tmUse, newTuse,
                  replaceSelfRefByVarAccess)
            }
            else if (tUser != ntUser)
              replaceMessageReceiver(reenactor, safeGet(reenactor, id2declMap), tmUser, tmUsed, tUser, ntUser)

          case Add(TypeBinding(typeUse@(tUser, tUsed), tmUse)) =>
            if (tUser != tUsed && tUser != tmUse.user &&
              resultGraph.getConcreteNode(tUser).kind != LocalVariable)
              createVarAccess(reenactor, safeGet(reenactor, id2declMap), tmUse, typeUse,
                introVarAccess)



          case Transformation(_, op) =>
            if (!discardedOp(op))
              logger.writeln(noApplyMsg)
        }
        id2declMap
    }
  }

  val introVarAccess : (ASTNode[_], MemberDecl, Access) => Unit =
    (user, decl, access) => user.introduceVarAccess(decl, access)

  val replaceSelfRefByVarAccess : (ASTNode[_], MemberDecl, Access) => Unit =
    (user, decl, access) => user.replaceThisQualifierFor(decl, access)


  def createVarAccess
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    typeMemberUse : NodeIdP,
    typeUse : NodeIdP,
    f : (ASTNode[_], MemberDecl, Access) => Unit)
  : Unit = {
    val v : Variable = id2declMap(typeUse.user) match {
      case VariableDeclHolder(decl) => decl
      case dh => error(s"expect parameter or field, $dh not handled")
    }

    val newAccess = v.createLockedAccess()

    val user : ASTNode[_] = id2declMap(typeMemberUse.user) match {
      case defh : DefHolder => defh.node
      case nodeHolder => error("create var access, expect a def " +
        s"(expr or block) as user but found  $nodeHolder")
    }

    val (usedAsVisible : Visible, usedAsMemberDecl : MemberDecl) =
      id2declMap(typeMemberUse.used) match {
        case FieldDeclHolder(fdecl,_) => (fdecl, fdecl)
        case mdh : MethodDeclHolder => (mdh.decl, mdh.decl)
        case h => error(s"self use of $h by $user case unhandled")
      }

    f(user, usedAsMemberDecl, newAccess)


    ASTNodeLink.enlargeVisibilityOfNidIfNeeded(
      reenactor, usedAsVisible,
      typeMemberUse.used)(id2declMap)

  }


  def replaceMessageReceiver
  ( reenactor : DependencyGraph,
    id2declMap: NodeId => ASTNodeLink,
    methodUser : NodeId,
    methodUsed : NodeId,
    oldMessageReceiver : NodeId,
    newMessageReceiver : NodeId
  )
  ( implicit logger : PuckLogger): Unit = {

    val mUser = id2declMap(methodUser) match {
      case dh : DefHolder => dh.node
      case nodeHolder => error("replace message receiver, expect a def " +
        s"(expr or block) as user but found $nodeHolder")
    }

    val mUsed : MemberDecl = id2declMap(methodUsed) match {
      case dh : HasMemberDecl => dh.decl
      case nodeHolder => error("replace message receiver, expect" +
        s" a field or a method as used but found $nodeHolder")
    }

    id2declMap(newMessageReceiver) match {
      case VariableDeclHolder(newReceiver) =>
        id2declMap(oldMessageReceiver) match {
          case VariableDeclHolder(oldReceiver) =>
            mUser.replaceMessageReceiver(mUsed, oldReceiver, newReceiver)
          case MethodDeclHolder(oldReceiver) => ???
          case dh : DefHolder =>
            mUser.replaceAllMessageReceiver(mUsed, newReceiver)
          //case ClassDeclHolder(_)=>
          //  logger.writeln("ignore replaceMessageReceiver with old receiver beeing a class. Suppose merge is ongoing ")
          case nodeHolder => error("replace message receiver, expect a def " +
            s"(expr or block), a field or a parameter as old receiver but found $nodeHolder")
        }
      case ClassDeclHolder(_)=>
        logger.writeln("ignore replaceMessageReceiver as new receiver beeing a class. " +
          "Suppose merge is ongoing ")

      case nodeHolder => error("replace message receiver, expect" +
        s" a field or a parameter as new receiver but found $nodeHolder")
    }



  }


}