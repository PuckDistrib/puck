package puck.graph

import java.io.BufferedWriter

import puck.javaAG._

/**
 * Created by lorilan on 17/06/14.
 */

object PrologPrinter {

  def print[Kind <: NodeKind[Kind]](writer: BufferedWriter, graph : AccessGraph[Kind]){

    def nodeKindStr(nk : Kind) = nk match {
      case Package() => "package"
      case Interface() => "interface"
      case Class() => "class"
      case AbstractMethod() => "method"
      case Constructor() => "constructor"
      case Method() => "method"
      case Field() => "attribute"
    }


    def writeln(str : String){
      writer write str
      writer newLine()
    }

    def nodeStr(n : AGNode[Kind]) =
      "node(%d, %s, '%s', typeNotPrinted).".format(n.id, nodeKindStr(n.kind), n.name)

    def edgeStr(edge : AGEdge[_]) = {
      val kind = edge.kind match {
        case Isa() => "isa"
        case Uses() => "uses"
        case Contains() => "contains"
        case Undefined() => throw new AGError("cannot print an edge with an undefined type")
      }
      "edge(%s, %d, %d).".format(kind, edge.source.id, edge.target.id)
    }

    graph.nodes.foreach(n=> n.kind match {
      case Primitive() | JavaRoot() => ()
      case _ => writeln(nodeStr(n))
    })

    graph.nodes.foreach{n =>
      n.content.foreach(c => writeln(edgeStr(AGEdge.contains(n, c))))
    }

    graph.nodes.foreach{n =>
      n.superTypes.foreach(c => writeln(edgeStr(AGEdge.isa(n, c))))
    }

    graph.nodes.foreach{n =>
      n.users.foreach(user => writeln(edgeStr(AGEdge.uses(user, n))))
    }

    writer.close()
  }


}
