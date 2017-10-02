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
package actions

import java.awt.Dimension
import javax.swing.{JTree, SwingUtilities}

import puck.control
import puck.control.{NodeClicked, PrintingOptionEvent, PrintingOptionsControl, PrintingOptionsUpdate}
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.io.VisibilitySet._
import puck.graph.io.{PrintingOptions, VisibilitySet, Visible}
import puck.view._
import puck.view.search.{SimpleElementSelector, SortedElementSelector, StateSelected}
import puck.search.{Search, SearchState}
import puck.util._
import puck.view.explorer.{DGTree, NodeInfosPanel, TreeModelAdapter}

import scala.concurrent.ExecutionContext
import scala.swing.BorderPanel.Position
import scala.swing.TabbedPane.Page
import scala.swing._
import scala.swing.event.Event
import scalaz.syntax.writer._

//this trait is needed to dispatch graphical computation on the rerserved thread in Intellij Idea plugin
trait SwingService {

  implicit val executor : ExecutionContext

  def swingInvokeLater (f : () => Unit ) : Unit
}

object DefaultSwingService extends SwingService {

  implicit val executor : ExecutionContext =
    scala.concurrent.ExecutionContext.Implicits.global

  def swingInvokeLater (f : () => Unit ) : Unit =
    SwingUtilities.invokeLater(new Runnable {
      def run(): Unit = f()
    })
}


case class Log(msg : String) extends Event


trait ResultPanel {
  def selectedResult: Logged[DependencyGraph]
}

trait GraphPanelResultPanel extends ResultPanel {
  val swingService : SwingService = DefaultSwingService
  def printingOptions : PrintingOptions
  val visibilitySet : VisibilitySet.T
  val constraints : ConstraintsMaps
  val graphUtils : GraphUtils
  val nodeKindIcons : NodeKindIcons
  val bus : Publisher

  def graphPanel
  ( g : DependencyGraph,
    visibilitySet: VisibilitySet.T) : Component =
    new ScrollPane() {
      viewportView =
//        Component.wrap(
//          //[VIEW DISPLAYED]
//          new BasicGraphCanvas(bus, nodeKindIcons) {
//            def graph: DependencyGraph = g
//        })
          new SplitPane(Orientation.Vertical) {
            self =>
            resizeWeight = 0.5

            leftComponent =
              Component.wrap (
              new JTree(TreeModelAdapter(g)) with DGTree {
              def icons: NodeKindIcons = nodeKindIcons
              addNodeClickedAction {
                (_, n) =>
                  self publish NodeClicked(n)
              }
            })

            rightComponent = new NodeInfosPanel(self, g)(nodeKindIcons)
          }


    }

  def selectedResultGraphPanel = {

    import Recording.RecordingOps
    val g = selectedResult.value

    val nodeVisibles  = {
      val involveNodes = g.recording.subRecordFromLastMilestone.involveNodes
      val nns = involveNodes flatMap g.containerPath
      visibilitySet.setVisibility(nns, Visible)
    }

    graphPanel(g, nodeVisibles)
  }
}

class AutoSolveResultPanel[S]
(val bus : Publisher,
 val constraints : ConstraintsMaps,
 val visibilitySet: VisibilitySet.T,
 printingOptionsControl: PrintingOptionsControl,
 res : Search[DecoratedGraph[S]])
( implicit val beforeGraph : DependencyGraph,
  val graphUtils : GraphUtils,
  val nodeKindIcons: NodeKindIcons)
  extends SplitPane(Orientation.Horizontal)
  with GraphPanelResultPanel {

  reactions += {
    case poe : PrintingOptionEvent => poe(printingOptionsControl)
      publish(PrintingOptionsUpdate)
  }

  def printingOptions: PrintingOptions =
    printingOptionsControl.printingOptions.copy(visibility = visibilitySet)

  def selectedResult : Logged[DependencyGraph] = activePanel.selectedResult

  val successesTab = 0
  val failuresTab = 1

  def selectorPanelOrDummy
  ( withSelector : Boolean,
    selector : => Component with Selector )  =
    if(withSelector) {
      val p = new SelectorResultPanel(bus, selector, constraints, this)
      p listenTo this
      this listenTo p
      this listenTo p.selector
      p
    }
    else new DummyResultPanel(beforeGraph)

  val successesPanel =
    selectorPanelOrDummy(res.results.nonEmpty, new SuccessSelector(res))

  val failurePanel =
    selectorPanelOrDummy(res.failures.nonEmpty, new FailureSelector(res))


  val tabs = new TabbedPane() {
    pages += new Page("Success", successesPanel) {
      mnemonic = successesTab
    }
    pages += new Page("Failures", failurePanel) {
      mnemonic = failuresTab
    }
  }


  def activePanel : ResultPanel =
  if (tabs.selection.page.mnemonic == failuresTab) failurePanel
  else successesPanel


  val console = new PuckConsolePanel()
  val upPane =  new SplitPane(Orientation.Vertical) {

    dividerSize = 3
    preferredSize = new Dimension(1024, 780)
    resizeWeight = 0.5
    leftComponent = graphPanel(beforeGraph, printingOptions.visibility)

    rightComponent = tabs

  }

  topComponent = upPane
  bottomComponent = console
    /*new SplitPane(Orientation.Vertical) {
    leftComponent = new BoxPanel(Orientation.Vertical){
      SVGMenu.addVisibilityCheckBoxes(this, printingOptionsControl)
    }
    rightComponent = console
  }*/

  reactions += {
    case Log(msg) => console.textArea.text = msg
  }

}




class DummyResultPanel
(val beforeGraph : DependencyGraph)
  extends FlowPanel with ResultPanel {

  def selectedResult : Logged[DependencyGraph] = beforeGraph.set("")


}

trait Selector extends Publisher{
  def selectedResult: Logged[DependencyGraph]
  def selectedLog : String = selectedResult.written
}

class FailureSelector[S](res : Search[DecoratedGraph[S]])
  extends SortedElementSelector[SearchState[DecoratedGraph[S]]](
    res.failuresByDepth, StateSelected.apply) with Selector{

  assert(res.failures.nonEmpty)
  def selectedResult = selectedState.prevState.get.success map (_.graph)
  override def selectedLog = selectedState.loggedResult.log
}

class SuccessSelector[S](res : Search[DecoratedGraph[S]])
  extends SimpleElementSelector[SearchState[DecoratedGraph[S]]](StateSelected.apply) with Selector{
  assert(res.results.nonEmpty)

  setStatesList(res.results)
  def selectedResult = selectedState.success map (_.graph)
}


class SelectorResultPanel[S]
(val bus : Publisher,
 val selector : Component with Selector,
 val constraints : ConstraintsMaps,
 val autosolveResultPanel : AutoSolveResultPanel[S])
(implicit val nodeKindIcons: NodeKindIcons )
  extends BorderPanel with GraphPanelResultPanel {

  def printingOptions: PrintingOptions = autosolveResultPanel.printingOptions
  val graphUtils: GraphUtils = autosolveResultPanel.graphUtils
  val visibilitySet: VisibilitySet.T = autosolveResultPanel.visibilitySet

  add(selectedResultGraphPanel, Position.Center)
  add(selector, Position.South)

  def selectedResult = selector.selectedResult

  this listenTo selector
  reactions += {
    case PrintingOptionsUpdate =>
      add(selectedResultGraphPanel, Position.Center)

    case StateSelected(state) =>
      add(selectedResultGraphPanel, Position.Center)
      publish(control.Log(selector.selectedLog))

  }

}