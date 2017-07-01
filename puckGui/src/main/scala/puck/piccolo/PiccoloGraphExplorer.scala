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

package puck.piccolo

import org.piccolo2d.PCanvas
import org.piccolo2d.extras.swing.PScrollPane
import puck.control._
import puck.view.{NodeKindIcons, PuckMainPanel, TreeViewHandler, ViewHandler}

import scala.swing.Publisher
import puck.view.explorer.GraphExplorer

/**
  * Created by Loïc Girault on 07/06/16.
  */
class PiccoloGraphExplorer
(control : PuckControl,
 nodeKindIcons: NodeKindIcons
) extends PScrollPane(new PCanvas()) with Publisher{

  var canvas : DGCanvas = _

  this listenTo control.Bus

  reactions += {
    case Popped(poppedGraph, newHead) =>
      canvas.popEvent(newHead, poppedGraph)
    //setModel(new MutableTreeModel(newHead))
    case Pushed(pushedGraph, previousHead) =>
      canvas.pushEvent(pushedGraph, previousHead)
    case evt : GraphChangeEvent =>
      canvas = new DGCanvas(control, nodeKindIcons)
      setViewportView(canvas)

    case GraphFocus(_, edge) =>
      canvas.focus(Set(edge.source, edge.target))

  }
}

object PiccoloViewHandler extends ViewHandler {

 // modified by cedric
 // override def toString = "Piccolo View"
 override def toString = "Piccolo View - unavailable: under construction"
  def installView(mainPanel: PuckMainPanel, treeIcons: NodeKindIcons) : Publisher = {
    new TreeViewHandler(mainPanel,
      new GraphExplorer(mainPanel.control, treeIcons))

  }
 /*
 def installView(mainPanel: PuckMainPanel,
                  nodeKindIcons: NodeKindIcons) : Publisher = {
    new TreeViewHandler(mainPanel,
      scala.swing.Component.wrap(new PiccoloGraphExplorer(mainPanel.control, nodeKindIcons)))

  } */
 
}
