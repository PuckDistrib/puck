package puck.gui
package explorer

import java.awt
import java.awt.{MouseInfo, Color}
import java.awt.event.{ActionEvent, MouseEvent, MouseAdapter}
import javax.swing.{Icon, AbstractAction, JPopupMenu, JTree}
import javax.swing.tree.{TreePath, DefaultTreeCellRenderer}

import puck.actions.GraphController
import puck.gui.svg.actions.ManualSolveAction
import puck.gui.{NodeMenu, NodeClicked}
import puck.{StackListener, GraphStack}
import puck.graph._


import scala.swing.{Swing, Component, ScrollPane}

trait DGTreeIcons {
  def iconOfKind(k: NodeKind ) : Icon
}

trait DGTree {
  self : JTree =>

  def graph : DependencyGraph = getModel.asInstanceOf[DGTreeModel].graph
  def icons : DGTreeIcons

  override def convertValueToText
  (value: AnyRef, selected: Boolean,
   expanded: Boolean, leaf: Boolean,
   row: Int, hasFocus: Boolean) : String =
    value match {
      case null => ""
      case node : DGNode  => node.name
      case _ => ""
    }

  this setCellRenderer DGNodeWithViolationTreeCellRenderer
}


object DGNodeWithViolationTreeCellRenderer
 extends DefaultTreeCellRenderer {

  def sourceOfViolation(graph : DependencyGraph, nodeId : NodeId) : Boolean = {

    val usedByDef =
      graph.kindType(nodeId) match {
        case TypeConstructor
          | InstanceValueDecl
          | StaticValueDecl =>
          graph.definitionOf(nodeId) map graph.usedBy getOrElse Set[NodeId]()
        case _ => Set[NodeId]()
      }
    (graph usedBy nodeId) ++ usedByDef exists (used => graph isViolation ((nodeId, used)))
  }


  def targetOfViolation(graph : DependencyGraph, nodeId : NodeId) : Boolean =
    (graph usersOf nodeId) exists (user => graph isViolation ((user, nodeId)))

  override def getTreeCellRendererComponent(tree: JTree, value: scala.Any, selected: Mutability,
                                            expanded: Mutability, leaf: Mutability, row: NodeId,
                                            hasFocus: Mutability): awt.Component = {
    val c = super.getTreeCellRendererComponent(tree, value, selected, expanded, leaf, row, hasFocus)
    tree match {
      case dgTree : DGTree =>
        val node = value.asInstanceOf[DGNode]
        if(sourceOfViolation(dgTree.graph, node.id) ||
            targetOfViolation(dgTree.graph, node.id))
          c.setForeground(Color.RED)

        setIcon(dgTree.icons.iconOfKind(node.kind))

        c
      case _ => c
    }
  }
}

class GraphExplorer(treeIcons : DGTreeIcons, var filter : Option[DGEdge] = None)
  extends ScrollPane
  with StackListener {



  def update(controller: GraphStack): Unit = {

    if (controller.graph.virtualNodes.nonEmpty) {
      import controller.graph
      val vn = graph.virtualNodes.head

      ManualSolveAction.forChoice[ConcreteNode]("Concretize node",
        "Select a concrete value for the virtual node :",
        vn.potentialMatches.toSeq map graph.getConcreteNode,
        { loggedChoice =>
          loggedChoice.value match {
            case None => ()
            case Some(cn) =>
              controller.pushGraph(graph.concretize(vn.id, cn.id))
          }

        }
      )
    }
    else {

      val model: DGTreeModel = filter match {
        case None => new FullDGTreeModel(controller.graph)
        case Some(e) => new FocusedDGTreeModel(controller.graph, e)
      }

      val tree: JTree = new JTree(model) with DGTree {
        def icons : DGTreeIcons = treeIcons
      }

      tree.addMouseListener(new MouseAdapter {

        override def mouseClicked(e: MouseEvent): Unit = {
          val path: TreePath = tree.getPathForLocation(e.getX, e.getY)

          if (path != null) {
            path.getLastPathComponent match {
              case node: DGNode =>
                if (isRightClick(e)) {
                  val menu: JPopupMenu = NodeMenu(controller.asInstanceOf[GraphController], node.id)
                  if (filter.nonEmpty)
                    menu.add(new AbstractAction("Show full graph") {
                      def actionPerformed(e: ActionEvent): Unit = {
                        filter = None
                        GraphExplorer.this.update(controller)
                      }
                    })
                  Swing.onEDT(menu.show(GraphExplorer.this.peer, e.getX, e.getY))
                }
                else GraphExplorer.this.publish(NodeClicked(node))
              case _ => ()
            }
          }
        }
      })
      contents = Component.wrap(tree)
      this.repaint()
    }
  }
}
