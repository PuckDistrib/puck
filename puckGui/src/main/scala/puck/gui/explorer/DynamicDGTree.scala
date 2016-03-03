package puck.gui.explorer

import puck.actions.Choose
import puck.graph.transformations.Recording
import puck.gui._
import java.awt.event.ActionEvent
import javax.swing.{AbstractAction, JPopupMenu, JTree}
import javax.swing.event.{TreeModelListener, TreeModelEvent}
import javax.swing.tree.TreePath

import puck.graph.transformations._
import Recording.RecordingOps

import puck.graph._


import puck.gui.{NodeClicked, Pushed, GraphUpdate}
import puck.gui.menus.NodeMenu

import scala.swing.{Swing, Reactor, Publisher}

/**
  * Created by lorilan on 29/01/16.
  */
class MutableTreeModel(var graph : DependencyGraph)
  extends TreeModelAdapter {


  def treepath(g : DependencyGraph, id : NodeId) : TreePath = {
    new TreePath((g.containerPath(id) map g.getConcreteNode).toArray[Object])
  }

  def treeModelEvent(g : DependencyGraph, id : NodeId) : Option[TreeModelEvent] =
    g.container(id) map {
      parentId =>
        val parentTreePath = treepath(g, parentId)
        val node = g.getConcreteNode(id)
        val idx = TreeModelAdapter.getChildren(g, parentId) indexOf node
        new TreeModelEvent(this, parentTreePath, Array(idx), Array[Object](node))
    }

  def parentTreeModelEvent(g : DependencyGraph, id : NodeId) : Option[TreeModelEvent] =
    g.container(id) map {
      parentId =>
        val parentTreePath = treepath(g, parentId)
        new TreeModelEvent(this, parentTreePath, null, null)
    }


  def applyRec(newGraph: DependencyGraph, oldGraph : DependencyGraph, subRec : Recording) : Unit = {
    var reenactor = oldGraph

    subRec.foreach  {
      case Transformation.Add(Edge(ContainsKind(_, cted))) =>
        graph = newGraph
        treeModelEvent(newGraph, cted) foreach fireNodesInserted

      case Transformation.Remove(CNode(cted)) =>
        graph = reenactor
        treeModelEvent(reenactor, cted.id) foreach fireNodesRemoved
        reenactor = reenactor.removeNode(cted.id)._2

      case t @ Transformation.Move((oldc, tgt), newc) =>
        graph = reenactor
        treeModelEvent(reenactor, tgt) foreach fireNodesRemoved
        reenactor = t.redo(reenactor)
        graph = reenactor
        treeModelEvent(reenactor, tgt) foreach fireNodesInserted

      case Transformation(_, Rename(id, oldName, newName)) =>
        graph = newGraph
        parentTreeModelEvent(newGraph, id) foreach fireStructureChanged

      case _ =>
    }

    graph = newGraph
  }

  def pushEvent(newGraph: DependencyGraph, oldGraph : DependencyGraph) : Unit = {
//    println("MutableTreeModel.pushEvent")
//    println(s"oldGraph = $oldGraph")
//    println(s"graph = $graph")
    assert(oldGraph eq graph)
    val subRec : Recording = newGraph.recording.subRecordFromLastMilestone.reverse
    applyRec(newGraph, oldGraph, subRec)

  }

  def popEvent(newGraph: DependencyGraph, oldGraph : DependencyGraph) : Unit = {
    assert(oldGraph eq graph)
    val subRec : Recording = oldGraph.recording.subRecordFromLastMilestone map (_.reverse)

    applyRec(newGraph, oldGraph, subRec)
  }
}

class DynamicDGTree
(model0 : MutableTreeModel,
 bus : Publisher,
 menuBuilder : NodeMenu.Builder,
 val icons: DGTreeIcons)
  extends JTree(model0) with DGTree with Reactor {
  self : JTree =>

  this listenTo bus

  def model = getModel.asInstanceOf[MutableTreeModel]

  reactions += {
    case Popped(poppedGraph, newHead) =>
      model.popEvent(newHead, poppedGraph)
      //setModel(new MutableTreeModel(newHead))

    case EmptiedButOne(graph) =>
      setModel(new MutableTreeModel(graph))

    case Pushed(pushedGraph, previousHead) =>

      if (pushedGraph.virtualNodes.isEmpty)
        model.pushEvent(pushedGraph, previousHead)
      else {
        val vn = pushedGraph.virtualNodes.head
        Choose("Concretize node",
          "Select a concrete value for the virtual node :",
          vn.potentialMatches.toSeq map pushedGraph.getConcreteNode) match {
          case None => ()
          case Some(cn) =>
            val r2 = pushedGraph.recording.subRecordFromLastMilestone.concretize(vn.id, cn.id)
            bus publish RewriteHistory(r2)
        }
      }
  }

  addNodeClickedAction {
    (e, node) =>
      if (isRightClick(e)) {
        val menu: JPopupMenu = menuBuilder(graph, node.id, selectedNodes, None)
        menu.add(new AbstractAction("Node infos") {
          def actionPerformed(e: ActionEvent): Unit =
            bus publish NodeClicked(node)
        })
        Swing.onEDT(menu.show(this, e.getX, e.getY))
      }
      else if(node.kind.kindType != NameSpace)
        bus publish NodeClicked(node)
  }

}
