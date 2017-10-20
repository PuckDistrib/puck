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

package puck.view.svg.actions


import java.awt.event.WindowEvent


import puck.control.PrintingOptionsControl
import puck.control.actions.printErrOrPushGraph
import puck.graph._
import puck.graph.constraints.ConstraintsMaps
import puck.graph.constraints.search.{NoVirtualNodes, TargetedControlWithHeuristic}
import puck.graph.io.VisibilitySet
import puck.graph.transformations.MutabilitySet
import puck.view.NodeKindIcons
import puck.search._

import scala.swing.BorderPanel.Position
import scala.swing._

object ResultFrame{

  def apply[T](bus : Publisher,
               cm : ConstraintsMaps,
               printingOptionsControl: PrintingOptionsControl,
               res : Search[DecoratedGraph[T]])
              (implicit graphUtils: GraphUtils,
               nodeKindIcons: NodeKindIcons): Frame = {

    implicit val LoggedSuccess(_, (graph,_,"")) = res.initialState.loggedResult

    new Frame(){
      frame =>
      title = "Search Result"
      visible = true
      minimumSize = new Dimension(640, 480)
      preferredSize = new Dimension(1920,1084)
      override def close() : Unit =
        frame.peer.dispatchEvent(new WindowEvent(frame.peer, WindowEvent.WINDOW_CLOSING))

      contents = new BorderPanel{
        val panel = new AutoSolveResultPanel(bus, cm, VisibilitySet.allVisible(graph), printingOptionsControl, res)
        add(panel, Position.Center)
        add(new BoxPanel(Orientation.Horizontal){
          contents += Swing.HGlue
          contents += new Button("OK"){

            action = new Action("OK"){
              def apply(): Unit ={
                try printErrOrPushGraph(bus, "Solve action : "){
                  panel.selectedResult.toLoggedTry
                }
                catch{
                  case t : Throwable =>
                    println("catched "+ t.getMessage)
                    t.printStackTrace()
                }
                close()
              }
            }
          }
          contents += Button("Cancel")(close())
          contents += Swing.HGlue
        }, Position.South)
      }
    }



  }
}

class AutoSolveAction
(bus : Publisher,
 cm : ConstraintsMaps,
 printingOptionsControl: PrintingOptionsControl,
 strategy : SearchStrategy[DecoratedGraph[Any]],
 control : SearchControl[DecoratedGraph[Any]])
(implicit graphUtils: GraphUtils,
 nodeKindIcons: NodeKindIcons)
  extends Action("Solve") {

  implicit val graph = control.initialState.graph

  override def apply(): Unit = {
    val g = graph.mileStone

    val engine =
      new SearchEngine(strategy, control, Some(1) /*,
        evaluator = Some(GraphConstraintSolvingStateEvaluator)*/)

    engine.explore()

    Swing onEDT puck.ignore(ResultFrame(bus, cm, printingOptionsControl, engine))

  }
}



class TargetedAutoSolveAction
(bus : Publisher,
 cm : ConstraintsMaps,
 mutability : MutabilitySet.T,
 violationTarget : ConcreteNode,
 printingOptionsControl: PrintingOptionsControl)
(implicit graph : DependencyGraph,
 graphUtils: GraphUtils,
 nodeKindIcons: NodeKindIcons)
  extends Action("Solve [BETA - under development]") {

  override def apply(): Unit = {
    val searchControlStrategy =
      new TargetedControlWithHeuristic( graphUtils.Rules, graph, cm, NoVirtualNodes, violationTarget)

    val engine =
      new SearchEngine(
        //new BreadthFirstSearchStrategy[(DependencyGraph, Int)],
        new BreadthFirstSearchStrategy[(DependencyGraph, Int, String)],
        searchControlStrategy,
        None /*,
        evaluator = Some(GraphConstraintSolvingStateEvaluator)*/)

    engine.explore()

    Swing onEDT puck.ignore(ResultFrame(bus, cm, printingOptionsControl, engine))

  }
}
