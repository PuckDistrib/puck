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

package puck.view.svg

import java.io.{File, FileWriter}

import puck._
import puck.control.{ConstraintsUpdate, GraphChangeEvent, PrintingOptionsControl, PrintingOptionsUpdate}
import puck.control.actions.AddNodeAction
import puck.graph.constraints.ConstraintsMaps
import puck.graph.{DependencyGraph, GraphUtils, NameSpace}
import puck.graph.io.DotPrinter
import puck.view.svg.actions.{DefaultSwingService, SwingService}
import puck.view.{NodeKindIcons, _}

import scala.swing.SequentialContainer.Wrapper
import scala.swing._
import scala.util.{Failure, Success}

/**
  * Created by Loïc Girault on 26/01/16.
  */
object SVGViewHandler extends ViewHandler{

 // modified by cedric
 // override def toString = "Dot SVG View" 
  override def toString = "Dot SVG View - unavailable : under construction"
 
 def installView(mainPanel: PuckMainPanel, treeIcons: NodeKindIcons) : Publisher = {
    new TreeViewHandler(mainPanel,
      new GraphExplorer(mainPanel.control, treeIcons))

  }
 /*
  def installView(mainPanel: PuckMainPanel, nodeKindIcons: NodeKindIcons) : Publisher = {
    new SVGViewHandler(mainPanel)
  } */
}

class SVGViewHandler
(mainPanel : PuckMainPanel)
  extends Publisher {
  import mainPanel.control


  this listenTo mainPanel.control.Bus
  reactions += {
    case ge : GraphChangeEvent =>
      this.displayGraph(ge.graph, control.constraints)

    case PrintingOptionsUpdate =>
      this.displayGraph(control.graph, control.constraints)

    case ConstraintsUpdate(g, cm) =>
      this.displayGraph(g, Some(cm))

  }


  import control.{graphUtils, logger, printingOptionsControl}
  val swingService : SwingService = DefaultSwingService
  import swingService._

  val svgController: SVGController = new SVGController(mainPanel.control, mainPanel.console)
  val canvas = PUCKSVGCanvas(svgController, swingService, mainPanel.nodeKindIcons)
  mainPanel.upPanel.setGraphView(Component.wrap(canvas))


  def displayGraph(graph : DependencyGraph, scm : Option[ConstraintsMaps], recCall : Boolean = false): Unit =
    documentFromGraph(graph, graphUtils, scm,
      printingOptionsControl.printingOptions) {
      res =>
        val smsg : Option[String] = res match {
          case Success(0) => None
          case Success(n) =>

            if (recCall)
              Some("An error that cannot be recovered occured during the production of the SVG file by Graphviz")
            else {
              val tmpDir = System.getProperty("java.io.tmpdir")
              val f = new File(tmpDir + File.separator + "graph.dot")

              DotPrinter.genDot(graph, graphUtils.dotHelper, scm,
                printingOptionsControl.printingOptions,
                new FileWriter(f))

              Some("error during SVG production dot can be found at " + f.getAbsolutePath /*+
                "\nretry with top level package visibility only"*/)

            }

          case Failure(errMsg) =>
            Some("Image creation failure : " + errMsg)

        }
        smsg foreach (msg => swingInvokeLater(() => logger writeln msg))
    }{
      case doc =>
        swingInvokeLater(() => canvas.setDocument(doc))
    }

  displayGraph(control.graph, control.constraints)

  mainPanel.downPanel.leftComponent = new SVGMenu(svgController)

}

object SVGMenu {
  def addVisibilityCheckBoxes(c : Wrapper,
                              control: PrintingOptionsControl) : Unit  = {
    def addCheckBox(n: String, initiallySelected: Boolean)(f: Boolean => Unit) = {
      c.contents += new CheckBox(n) {
            selected = initiallySelected
            action = new Action(n) {
              def apply() = f(selected)
            }
          }
    }
    val printingOptions = control.printingOptions
    addCheckBox ("Show signatures",
      printingOptions.printSignatures) {
      b => control.signatureVisible = b
    }

    addCheckBox ("Show ids",
      printingOptions.printId) {
      b => control.idVisible = b
    }
    addCheckBox ("Show Virtual Edges",
      printingOptions.printVirtualEdges) {
      b => control.virtualEdgesVisible = b
    }
    addCheckBox ("Show Type Uses",
      printingOptions.printTypeUses) {
      b => control.typeUsesVisible = b
    }
    addCheckBox ("Concrete Uses/Virtual Edge",
      printingOptions.printConcreteUsesPerVirtualEdges) {
      b =>  control.concreteUsesPerVirtualEdges = b
    }
    ignore(addCheckBox ("Show RedOnly",
      printingOptions.redOnly) {
      b => control.redEdgesOnly = b
    })
  }
}


class SVGMenu
( private val controller: SVGController) extends BoxPanel(Orientation.Vertical) {

  implicit def graph : DependencyGraph = controller.graph
  implicit val graphUtils : GraphUtils = controller.graphUtils
  val printingOptionsControl = controller.printingOptionsControl


  SVGMenu.addVisibilityCheckBoxes(this, printingOptionsControl)


  import graphUtils.nodeKindKnowledge.kindOfKindType
  assert(kindOfKindType(NameSpace).size == 1)

  contents += new AddNodeAction(controller.genControl.Bus, graph.root, kindOfKindType(NameSpace).head)

  contents += button("Show top level packages"){
    () => printingOptionsControl.
      focusExpand(graph, graph.rootId, focus= false, expand = true)
  }

  contents += button("Hide type relationship") {
    () => printingOptionsControl.selectedEdgeForTypePrinting = None
  }


}
