package puck.graph

/**
 * Created by lorilan on 05/05/14.
 */
abstract class Type {
  def subtypeOf(other : Type) : Boolean = this == other
}

case class NamedType[Kind <: NodeKind[Kind]](n : AGNode[Kind]) extends Type{
  override def toString = n.name
  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    (other match {
        //TODO fix cast
      case NamedType(othern) => othern.asInstanceOf[AGNode[Kind]] isSuperTypeOf n
      case _ => false
    })
}

case class Tuple[T <: Type](types: List[T]) extends Type {
  override def toString = types mkString ("(", ", ", ")")

  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    (other match {
      case Tuple(ts) => (types, ts).zipped forall( _.subtypeOf(_))
      case _ => false
    })

  def length = types.length
}

case class Arrow(input:Type, output:Type) extends Type{
  override def toString = input + " -> " + output

  override def subtypeOf(other : Type) : Boolean = super.subtypeOf(other) ||
    ( other match{
      case Arrow( i, o) => i.subtypeOf(input) && output.subtypeOf(o)
      case _ => false })

}