package puck.gui.search

import puck.graph._
import puck.graph.io.VisibilitySet
import puck.gui.{ConstraintDisplayRequest, ApplyOnCodeRequest}
import puck.search.SearchState

import scala.swing._
import scala.swing.event.{Event, SelectionChanged}


case class StateSelected(state : SearchState[SResult]) extends Event

class SimpleElementSelector[T]
 ( evtGen : T => Event)
  extends FlowPanel {
  var searchStateComboBox : ComboBox[T] = _

  def publish() : Unit = {
    this.publish(evtGen(searchStateComboBox.selection.item))
  }

  def selectedState = searchStateComboBox.selection.item

  def setStatesList(states : Seq[T]): Unit ={
    if(searchStateComboBox != null) {
      contents.clear()
      this deafTo searchStateComboBox
    }
    searchStateComboBox = new ComboBox(states)
    this listenTo searchStateComboBox.selection
    contents += searchStateComboBox
    this.revalidate()
    publish()
  }

  reactions += {
    case SelectionChanged(cb) => publish()
  }


}

class SortedElementSelector[T]
(map : Map[Int, Seq[T]],
 evtGen : T => Event)
  extends BoxPanel(Orientation.Vertical) {
  val firstLine = new FlowPanel()
  val simpleStateSelector = new SimpleElementSelector[T](evtGen)
  val couplingValues = new ComboBox(map.keys.toSeq.sorted)

  simpleStateSelector.setStatesList(map(couplingValues.selection.item))

  firstLine.contents += couplingValues
  firstLine.contents += simpleStateSelector
  contents += firstLine

  this listenTo couplingValues.selection
  this listenTo simpleStateSelector
  this deafTo this

  reactions += {
    case SelectionChanged(cb) =>
      simpleStateSelector.setStatesList(map(couplingValues.selection.item))
    case evt  => publish(evt)
  }

  def selectedState = simpleStateSelector.selectedState

}

class StateSelector
( map : Map[Int, Seq[SearchState[SResult]]],
  printId : () => Boolean,
  printSig: () => Boolean,
  visibility : VisibilitySet.T)
  extends SortedElementSelector[SearchState[SResult]](map, StateSelected.apply) {


  val secondLine = new FlowPanel()
  /*secondLine.contents += new Button(""){
      action = new Action("Show"){
        def apply() {
          StateSelector.this publish
            GraphDisplayRequest(couplingValues.selection.item + " " + searchStateComboBox.selection.item.uuid(),
              graphOfResult(searchStateComboBox.selection.item.result), printId(), printSig())
        }
      }
    }
  */

//  secondLine.contents += new Button(""){
//    action = new Action("Show"){
//      def apply() : Unit = {
//
//        val state: SearchState[SResult] = selectedState
//        var id = -1
//
//        StateSelector.this publish SearchStateSeqPrintingRequest(state.uuid()+"history",
//          state.ancestors(includeSelf = true), Some({s => id +=1
//            id.toString}), printId(), printSig(), visibility)
//
//      }
//    }
//  }

  secondLine.contents += new Button(""){
    action = new Action("Constraint"){
      def apply() : Unit =  {
        val state: SearchState[SResult] = selectedState
        StateSelector.this publish ConstraintDisplayRequest(graphOfResult(state.loggedResult.value))
      }
    }
  }

  secondLine.contents += new Button(""){
    action = new Action("Apply"){
      def apply() : Unit = {
        StateSelector.this publish ApplyOnCodeRequest(graphOfResult(selectedState.loggedResult.value))
      }
    }
  }
  contents += secondLine

}