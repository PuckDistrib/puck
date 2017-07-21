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

package puck.javaGraph

import puck.graph._
import puck.javaGraph.nodeKind._


import DependencyGraph._

trait JavaGraphBuilder extends GraphBuilder{

   g = DependencyGraph(JavaNodeKind,
     NodeIndex(JavaNodeKind.root), EdgeMap(),
     AbstractionMap(), Recording(), Set())

  nodesByName += (g.root.name -> g.rootId)

  val arrayTypeId = addNode("@primitive.[]","[]", Primitive)()
  fromLibrary += arrayTypeId

  def addPackageNode(fullName: String, localName:String, fromSource : Boolean) : NodeId =
    addNode(fullName, localName, Package)(
      pid =>
        if(!fromSource) {
          fromLibrary += pid
     //     println(pid + "-" + getFullName(pid) + " is in fromLibrary") // added by Cedric
        }
    )

  def definitionOf(nid : NodeId) =
    g.getConcreteNode(nid).definition(g)



  def addPackage(p : String, fromSource : Boolean): NodeId =
    nodesByName get p match {
      case None =>
        val fp = filterPackageName(p)
        val path = fp split "[.]"
        if (path.isEmpty) addPackageNode(fp, fp, fromSource)
        else {
          val (_, n):(StringBuilder, NodeId) = path.foldLeft((new StringBuilder(), rootId)){
              case ((sb, nodeParent), p) =>
                sb append p
                val nId = addPackageNode(sb.toString(), p, fromSource)
                addEdge(Contains(nodeParent, nId))
                sb append "."
                (sb, nId)
          }
          n
        }
      case  Some(pn) => pn
    }





  def typeEdge(typeUser : NodeId, typeUsed : NodeId) : DGEdge = {
    if(g.getConcreteNode(typeUser).kind.kindType == TypeDecl &&
      typeUser != typeUsed && g.isa_*(typeUser, typeUsed))
      IsaEdge(typeUser, typeUsed)
    else
      Uses(typeUser, typeUsed)
  }

}
