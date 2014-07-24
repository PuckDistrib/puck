package puck.graph

import puck.util.{HasChildren, BreadthFirstTreeIterator}

import scala.collection.mutable
import puck.graph.constraints._


class AGNodeIterator[Kind <: NodeKind[Kind]] (val root : AGNode[Kind])extends BreadthFirstTreeIterator[AGNode[Kind]]


trait AGNodeBuilder[Kind <: NodeKind[Kind]] {
  def apply(g: AccessGraph[Kind], id: Int, name:String, kind : Kind) : AGNode[Kind]
  def makeKey(fullName: String, localName:String, kind: Kind) : String
  def rootKind : Kind
  def kinds : List[Kind]
  val scopeSeparator : String = "."
}

object AGNode extends AGNodeBuilder[VanillaKind]{
  override def apply(g: AccessGraph[VanillaKind],
                     id: Int, name : String,
                     kind : VanillaKind) : AGNode[VanillaKind] = new AGNode(g, id, name, kind)

  def rootKind = VanillaRoot()

  override def makeKey(fullName: String, localName:String,
                       kind : VanillaKind) : String = fullName

  val kinds = List(VanillaNodeKind())

  def implUsesAbs(implKind : VanillaKind, absKind : VanillaKind) : Boolean =
    (implKind, absKind) match {
      case (VanillaNodeKind(), VanillaNodeKind()) => false
      case _ => throw new AGError("do not know if impl (%s) uses abs (%s)".format(implKind, absKind))
    }
}

class AGNode[Kind <: NodeKind[Kind]] (val graph: AccessGraph[Kind],
              val id: Int,
              var name: String,
              val kind: Kind) extends HasChildren[AGNode[Kind]]{

  type NodeType = AGNode[Kind]
  //extends Iterable[NodeType]{

  /*override def equals(obj: Any): Boolean = obj match {
    case that: NodeType =>
      if (this.graph eq that.graph)
       this.id == that.id
      else {

        def softEqual(n1 : NodeType, n2 :NodeType) =
          n1.name == n2.name && n1.kind == n2.kind //TODO see type equality ...
        //TODO use soft equality to compare uses arcs

        softEqual(this, that) &&
        this.name == that.name && this.kind == that.kind &&
          content.forall { c =>
            that.content.find(tc => tc == c)
          }
      }
    case _ => false
  }

  override def hashCode: Int = (this.id + kind.hashCode()) / 41*/

  def softEqual(other : NodeType) : Boolean = {
    //this.name == other.name && // NO !!
    this.kind == other.kind &&
    users.forall{n => other.users.exists(_.softEqual(n))}  &&
    content.forall{n => other.content.exists(_.softEqual(n))}
  }

  /**
   * relies on the contains tree : do not modify it while traversing
   */
  def iterator = new AGNodeIterator(this)

  override def toString: String = "(" + fullName + ", " + kind + ")"

  def nameTypeString = name + (kind match{case k : HasType[_] => " : " + k.`type`; case _ => ""})


  def fullName: String = {

    /*if (isRoot) nameTypeString
      else {*/
    val path = containerPath.map(_.nameTypeString)
    (if (path.head == AccessGraph.rootName)
      path.tail
    else
      AccessGraph.unrootedStringId :: path ).mkString(graph.scopeSeparator)
  }

  var isMutable = true

  /**
   * Arcs
   */

  private var container0 : NodeType = this

  def container = container0

  def isRoot = container == this



  def canContain(n : NodeType) : Boolean = {
    n != this &&
      (this.kind canContain n.kind) &&
      this.isMutable
  }

  private val content0 : mutable.Set[NodeType] = mutable.Set()

  def content : mutable.Iterable[NodeType] = content0

  def children = content

  def content_+=(n : NodeType, register : Boolean = true) {
    n.container0 = this
    this.content0.add(n)
    if(register)
      graph.transformations.addEdge(AGEdge.contains(this, n))
  }
  def content_-=(n : NodeType, register : Boolean = true) {
    n.container0 = n
    this.content0.remove(n)
    if(register)
      graph.transformations.removeEdge(AGEdge.contains(this, n))
  }

  /*def content_+=(n:NodeType) {
    if( n.container0 == this)
      throw new IllegalAGOperation("content += error " + n + "container is already "+ this)

    n.container0 = this

    if(!this.content0.add(n))
      throw new IllegalAGOperation("content += error "+ this +" already contains "+ n)

    graph.transformations.addEdge(AGEdge.contains(this, n))

  }

  def content_-=(n:NodeType) {

    if( n.container0 != this)
      throw new IllegalAGOperation("content -= error " +n + "container is not "+ this)

    n.container0 = n

    if(!this.content0.remove(n))
      throw new IllegalAGOperation("content -= error "+ this +" does not contains "+ n)


    graph.transformations.removeEdge(AGEdge.contains(this, n))
  }*/

  def moveTo(newContainer : NodeType) {
    AGEdge.contains(container, this).changeSource(newContainer)

    users.foreach{
      _.redirectPrimaryUses(this, this, Move())
    }
  }

  private[graph] def isContentEmpty = content0.isEmpty

  def contains(other :NodeType) = content0.contains(other)

  def contains_*(other:NodeType) : Boolean =
    other == this || !other.isRoot && this.contains_*(other.container)


  def containerPath(ancestor : NodeType) : List[NodeType] = {

    def aux(current : NodeType, acc : List[NodeType]) : List[NodeType] =
      if (ancestor == current) this :: acc
      else if (current.isRoot) throw new AGError("container path computation error :"+
        "\nfollowed path : " + acc.foldRight(List[String]()){
        case (n, acc0) => n.name :: acc0} +
        "\ncurrent node : " + current.name +
        "\nancestor " + ancestor.name + " not found !")
      else aux(current.container, current :: acc)

    aux(this, List())
  }

  def containerPath : List[NodeType] = {

    def aux(current : NodeType, acc : List[NodeType]) : List[NodeType] =
      if (current.isRoot) current :: acc
      else aux(current.container, current :: acc)

    aux(this, List())
  }

  private [this] val superTypes0 : mutable.Set[NodeType] = mutable.Set()

  def isSuperTypeOf(other : NodeType) : Boolean = {
    other.superTypes.exists(_ == this) ||
      other.superTypes.exists(_.isSuperTypeOf(this))
  }


  def superTypes : Iterable[NodeType] = superTypes0
  private val subTypes0 : mutable.Set[NodeType] = mutable.Set()
  def subTypes : Iterable[NodeType] = subTypes0

  def superTypes_+=(st:NodeType, register : Boolean = true) {
    if(superTypes0.add(st)) {
      st.subTypes0.add(this)
      if(register)
        graph.transformations.addEdge(AGEdge.isa(this, st))
      abstractions_+=(st, SupertypeAbstraction())
    }
  }

  def superTypes_-=(st:NodeType, register : Boolean = true) {

    if(superTypes0.remove(st)) {
      st.subTypes0.remove(this)
      if(register)
      graph.transformations.removeEdge(AGEdge.isa(this, st))
      abstractions_-=(st, SupertypeAbstraction())
    }
  }

  def isa(other : NodeType) : Boolean = {
    superTypes.exists( n => n == other)
  }

  //TODO think about removing uses0 & users0 and keep only the sideUses0/primaryUses0 maps
  //private var uses0 : mutable.Set[NodeType] = mutable.Set()
  def uses(other : NodeType) = other.isUsedBy(this)// uses0.contains(other)
  def uses_+=(other: NodeType, register : Boolean = true) = other.users_+=(this, register)
  def uses_-=(other: NodeType, register : Boolean = true) = other.users_-=(this, register)

  private [this] val users0 : mutable.Set[NodeType] = mutable.Set()
  def users_+=(other : NodeType, register : Boolean = true) {
    if(users0.add(other) && register)
      graph.transformations.addEdge(AGEdge.uses(other, this))
  }

  def users_-=(user : NodeType, register : Boolean = true){
    if(users0.remove(user) && register)
      graph.transformations.removeEdge(AGEdge.uses(user, this))
  }

  def users: mutable.Iterable[NodeType] = users0

  def isUsedBy(other : NodeType) = users0.contains(other)

  /* a primary use is for example the use of a class when declaring a variable or a parameter
   a side use is in this example a call to a method with the same variable/parameter
      the use of the method is a side use of the declaration
       a couple primary use/ side use is a use dependency
 */

  /*(this, key) is a primary uses and sidesUses(key) are the corresponding side uses */
  val sideUses = new UsesDependencyMap(this, Dominant())

  /*(this, key) is a side uses and primaryUses(key) is the corresponding primary uses */
  val primaryUses = new UsesDependencyMap(this, Dominated())

  def findNewPrimaryUsee(currentPrimaryUsee : NodeType,
                         newSideUsee : NodeType,
                         policy : RedirectionPolicy) : NodeType = {
    policy match {
      case Move() => newSideUsee.container
      case absPolicy : AbstractionPolicy =>
        currentPrimaryUsee.abstractions.find{
          case (node, `absPolicy`) => node.contains_*(newSideUsee)
          case _ => false
        } match {
          case Some((n, _)) => n
          case None =>
            graph.iterator.find{ node =>
              node.contains_*(newSideUsee) &&
                currentPrimaryUsee.kind.abstractKinds(absPolicy).contains(node.kind)
            } match {
              case Some(n) => n
              case None => throw new AGError("no correct primary abstraction found !")
            }
        }
    }
  }

  def redirectPrimaryUses(currentSideUsee : NodeType,
                          newSideUsee : NodeType,
                          policy : RedirectionPolicy){
    println("redirecting primary uses ... ")
    primaryUses get currentSideUsee match {
      case None =>
        println("no primary uses to redirect")
        ()
      case Some(primary_uses) =>
        println("uses to redirect:%s".format(primary_uses.mkString("\n", "\n","\nend of list")))

        assert(primary_uses.nonEmpty)

        val primary = primary_uses.head
        if(primary_uses.tail.nonEmpty) {
          println("redirecting side uses : (%s, %s) to (%s, %s)".format(this, currentSideUsee, this, newSideUsee))
          println("primary uses are ")
          primary_uses.mkString("-", "\n-", "\n")
          throw new AGError("Do not know how to unsuscribe a side use with multiple primary uses")
        }
        else {

          graph.removeUsesDependency(primary, AGEdge.uses(this, currentSideUsee))
          //TODO redirection en cascade !!!
          val newPrimary = primary.changeTarget(findNewPrimaryUsee(primary.usee, newSideUsee, policy))
          graph.addUsesDependency(newPrimary, AGEdge.uses(this, newSideUsee))

        }
    }
  }

  def redirectSideUses(currentPrimaryUsee: NodeType,
                       newPrimaryUsee : NodeType,
                       policy : RedirectionPolicy){
    println("redirecting side uses ... ")
    sideUses get currentPrimaryUsee match {
      case None =>
        println("no side uses to redirect")
        ()
      case Some( sides_uses ) =>
        println("uses to redirect:%s".format(sides_uses.mkString("\n", "\n","\nend of list")))

        sides_uses foreach {
          side =>
            side.usee.abstractions.find {
              case (abs, _) => newPrimaryUsee.contains(abs)
              case _ => false
            } match {
              case None =>
                throw new RedirectionError(("While redirecting primary uses (%s, %s) to (%s, %s)\n" +
                "no satisfying abstraction to redirect side use %s").
                format( this, currentPrimaryUsee, this, newPrimaryUsee, side))

              case Some( (new_side_usee, _) ) =>

                graph.removeUsesDependency(AGEdge.uses(this, currentPrimaryUsee), side)
                //TODO redirection en cascade !!!
                val newSide = side.changeTarget(new_side_usee)
                graph.addUsesDependency(AGEdge.uses(this, newPrimaryUsee), newSide)

            }
        }
    }
  }

  def redirectUses(oldUsee : NodeType, newUsee : NodeType,
                   policy : RedirectionPolicy){

    println("redirecting uses %s --> %s to\n%s (%s)".format(this, oldUsee, newUsee, policy))
    AGEdge.uses(this, oldUsee).changeTarget(newUsee)

    redirectPrimaryUses(oldUsee, newUsee, policy)
    redirectSideUses(oldUsee, newUsee, policy)
  }


  /**********************************************/
  /************** Constraints********************/
  /**********************************************/
  /**
   * Friends, Interlopers and Facade are scopes.
   */

  /**
   * friends bypass other constraints
   */
  val friendConstraints = new ConstraintSet[Kind, FriendConstraint[Kind]]()
  /*
   * assert owners contains this
   */
  /**
   * this scope is hidden
   */
  val scopeConstraints = new ConstraintSet[Kind, ScopeConstraint[Kind]]()

  /**
   * this element is hidden but not the elements that it contains
   */
  val elementConstraints = new ConstraintSet[Kind, ElementConstraint[Kind]]()

  /**
   * Constraints Handling
   */

  def discardConstraints() {
    friendConstraints.clear()
    scopeConstraints.clear()
    elementConstraints.clear()
  }

  def remove(ct : Constraint[Kind]) = ct match {
    case fct @ FriendConstraint(_,_) => friendConstraints -= fct
    case ect @ ElementConstraint(_,_,_) => elementConstraints -= ect
    case sct @ ScopeConstraint(_,_,_,_) => scopeConstraints -= sct
  }

  final def friendOf(other : NodeType) : Boolean = other.friendConstraints.hasFriendScopeThatContains_*( this ) ||
    !other.isRoot && friendOf(other.container)

  def violatedScopeConstraintsOf(usee0 : NodeType) : List[ScopeConstraint[Kind]] = {
    val uses = AGEdge.uses(this, usee0)

    def aux(usee : NodeType, acc : List[ScopeConstraint[Kind]]) : List[ScopeConstraint[Kind]] = {
      val acc2 = if(!usee.contains_*(this))
        usee.scopeConstraints.filter(_.violated(uses)).toList ::: acc
      else acc

      if(usee.isRoot) acc2
      else aux(usee.container, acc2)
    }
    aux(usee0,List())
  }


  def potentialScopeInterloperOf(usee0 : NodeType) : Boolean = {
    val uses = AGEdge.uses(this, usee0)

    def aux(usee: NodeType): Boolean =
      !usee.contains_*(this) &&
        usee.scopeConstraints.exists(_.violated(uses)) ||
        !usee.isRoot && aux(usee.container)

    aux(usee0)
  }

  def violatedElementConstraintOf(usee : NodeType) =
    usee.elementConstraints.filter(_.violated(AGEdge.uses(this, usee))).toList

  def potentialElementInterloperOf(usee:NodeType) =
    usee.elementConstraints.exists(_.violated(AGEdge.uses(this, usee)))

  def interloperOf(other : NodeType) =
    (potentialScopeInterloperOf(other)
      || potentialElementInterloperOf(other)) && !friendOf(other)

  def isWronglyContained : Boolean = !isRoot && (container interloperOf this)

  def wrongUsers : List[NodeType] = {
    users.foldLeft(List[NodeType]()){(acc:List[NodeType], user:NodeType) =>
      if( user interloperOf this ) user :: acc
      else acc
    }
  }

  /**
   * Solving
   */

  /*private[this] lazy val abstractions0: mutable.Set[(NodeType, AbstractionPolicy)] =
    searchExistingAbstractions()
  def searchExistingAbstractions() = mutable.Set[(NodeType, AbstractionPolicy)]()*/

  private[this] val abstractions0 = mutable.Set[(NodeType, AbstractionPolicy)]()

  def abstractions : mutable.Iterable[(NodeType, AbstractionPolicy)] = abstractions0
  def abstractions_-=(n : NodeType, p : AbstractionPolicy){
    if(abstractions0.remove( (n,p) ))
      graph.transformations.unregisterAbstraction(this, n, p)

  }
  def abstractions_+= (n : NodeType, p : AbstractionPolicy){
    if(abstractions0.add( (n,p) ))
      graph.transformations.registerAbstraction(this, n, p)
  }

  def abstractionName(abskind : Kind, policy : AbstractionPolicy) : String =
    name + "_" + policy

  def createNodeAbstraction(abskind :  Kind, policy : AbstractionPolicy) : NodeType = {
    val n = graph.addNode(abstractionName(abskind, policy), abskind)
    abstractions_+=(n, policy)
    n
  }

  def createAbstraction(abskind : Kind , policy : AbstractionPolicy) : NodeType = {
    val abs = createNodeAbstraction(abskind, policy)
    policy match {
      case SupertypeAbstraction() =>  abs users_+= this
      case DelegationAbstraction() => this users_+= abs
    }
    abs
  }

  def addHideFromRootException(friend : NodeType){
    def addExc(ct : ConstraintWithInterlopers[Kind]) {
      if (ct.interlopers.iterator.contains(graph.root)){
        ct.friends += friend
        graph.transformations.addFriend(ct, friend)
      }
    }

    scopeConstraints foreach addExc
    elementConstraints foreach addExc
  }

}



