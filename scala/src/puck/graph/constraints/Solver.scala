package puck.graph.constraints

import puck.graph._

trait Solver[Kind <: NodeKind[Kind]] {

  val graph : AccessGraph[Kind]

  val decisionMaker : DecisionMaker[Kind]

  type NodeType = AGNode[Kind]

  def redirectTowardExistingAbstractions(usee: NodeType,
                                         wrongUsers : List[NodeType]) = {
    val (absKind, absPolicy) = getAbsKindAndPolicy(usee)

    println("redirect toward existing abstractions")
    wrongUsers.foldLeft(List[NodeType]()) { (unsolved: List[NodeType], wu: NodeType) =>
      usee.abstractions find {
        case (node, `absPolicy`) if node.kind == absKind => !wu.interloperOf(node)
        case _ => false
      } match {
        case None => wu :: unsolved
        case Some((abs, _)) =>
          println(wu + " will use abstraction " + abs)

          graph.transformations.startSequence()

          try {
            wu redirectUses(usee, abs, absPolicy)
            unsolved
          }
          catch {
            case e :RedirectionError =>
              println("redirection error catched !!")
              graph.transformations.undo()
              wu :: unsolved
          }

      }
    }
  }

  def findHost(toBeContained : NodeType,
               wrongUsers: List[NodeType],
               context : => String,
               predicate : (NodeType) => Boolean = _ => true )
              (k : Option[NodeType] => Unit) {

    def loosePredicate(n : NodeType) =
      n != toBeContained &&
        n.canContain(toBeContained) &&
        predicate(n)

    // with the search engine, all solutions will be explored anyway
    decisionMaker.chooseNode(context, loosePredicate, k)


    /*def strictPredicate( n :NodeType ) = loosePredicate(n) &&
      wrongUsers.forall(!_.interloperOf(n))


    decisionMaker.chooseNode(context, strictPredicate, {
      case None => decisionMaker.chooseNode(context, loosePredicate, k)
      case sn => k(sn)
    })*/



  }

  def singleAbsIntroPredicate(impl : NodeType,
                              absPolicy : AbstractionPolicy,
                              absKind : Kind) : NodeType => Boolean = absPolicy match {
    case SupertypeAbstraction() =>
      potentialHost => !(impl interloperOf potentialHost)

    case DelegationAbstraction() =>
      potentialHost => !(potentialHost interloperOf impl)
  }




  def singleAbsIntro(impl : NodeType,
                     wrongUsers : List[NodeType],
                     degree : Int = 1)
                    (k : Option[NodeType] => Unit){
    println("\nsingle abs intro degree "+degree)

    def intro (currentImpl : NodeType,
               absKind : Kind,
               absPolicy : AbstractionPolicy,
               wrongUsers : List[NodeType])
              (predicate : (NodeType) => Boolean =
               singleAbsIntroPredicate(currentImpl, absPolicy, absKind),
               context : => String =
               "Searching host for abstraction of %s ( %s, %s )".
                 format(currentImpl, absKind, absPolicy),
               parentsThatCanBeCreated : Int = 2)
              (k : Option[(NodeType, AbstractionPolicy)] => Unit){

      graph.transformations.startSequence()
      val abs = currentImpl.createAbstraction(absKind, absPolicy)
      findHost(abs, wrongUsers, context, predicate) {
        case None =>
          graph.transformations.undo()
          if(parentsThatCanBeCreated > 0)
            hostIntro(currentImpl, absKind, absPolicy, wrongUsers,
              parentsThatCanBeCreated - 1, k)
          else
            k(None)
        case Some(host) =>
          host.content_+=(abs)
          k(Some(abs, absPolicy))
      }

    }

    def hostIntro (toBeContained : NodeType,
                   absKind : Kind,
                   absPolicy : AbstractionPolicy,
                   wrongUsers : List[NodeType],
                   parentsThatCanBeCreated : Int,
                   k : Option[(NodeType, AbstractionPolicy)] => Unit){
      println("\nhostIntro")
      toBeContained.container.kind.abstractKinds(absPolicy).find(_.canContain(absKind)) match {
        case None => throw new AGError("container abstraction creation error")
        case Some(cterAbsKind) =>
          intro(toBeContained.container,
            cterAbsKind, absPolicy, wrongUsers)({ n: NodeType => true},
          "Searching host for abstraction of %s's container:\n%s ( %s, %s )\n".
            format(toBeContained, toBeContained.container, cterAbsKind, absPolicy),
          parentsThatCanBeCreated){
            case None => k(None)
            case Some((cterAbs, _ )) => toBeContained.abstractions.find {
              case (existingAbs, `absPolicy`) => cterAbs.contains(existingAbs)
              case _ => false
            } match {
              case None =>
                val newAbs = toBeContained.createAbstraction(absKind, absPolicy)
                cterAbs content_+= newAbs
                k(Some(newAbs, absPolicy))
              case existingAbsAndPolicy => k(existingAbsAndPolicy)
            }
          }
      }
    }

    def aux(deg : Int, currentImpl : NodeType)
           (k :Option[(NodeType, AbstractionPolicy)] => Unit) {
      println("*** abs intro degree %d/%d ***".format(deg,degree))

      val (absKind, absPolicy) = getAbsKindAndPolicy(currentImpl)

      def doIntro(currentWrongUsers : List[NodeType],
                  k : Option[(NodeType, AbstractionPolicy)] => Unit) =
        intro (currentImpl, absKind, absPolicy, currentWrongUsers)()(k)

      if(deg == degree)
        doIntro(wrongUsers, k)
      else
        doIntro(List(), {
          case None => throw new AGError("Single abs intro degree %d/%d error (currentImpl = %s)".
            format(deg, degree, currentImpl))
          case Some((abs, _)) =>
            aux(deg + 1, abs)(k)
        })
    }


    graph.transformations.startSequence()
    aux(1, impl) {
      case None => k(None)
      case Some((abs, absPolicy)) =>

        println("redirecting wrong users !!")
        wrongUsers.foreach(_.redirectUses(impl, abs, absPolicy))
        /*if(impl.wrongUsers.nonEmpty){
          println("abs intro failure, remaing wrongusers :")
          print(impl.wrongUsers.mkString("-","\n-", "\n"))
          graph.transformations.undo()
          None
        }
        else{*/
        k(Some(abs))
      //}
    }

  }

  def getAbsKindAndPolicy(impl : NodeType) : (Kind, AbstractionPolicy) = {
    if(impl.kind.abstractionPolicies.isEmpty){
      throw new AGError(impl + " has no abstraction policy !")
    }
    val policies = impl.kind.abstractionPolicies
    if(policies.tail.isEmpty
      && impl.kind.abstractKinds(policies.head).tail.isEmpty){
      (impl.kind.abstractKinds(policies.head).head, policies.head)
    }
    else
      decisionMaker.abstractionKindAndPolicy(impl)
  }

  def solveUsesToward(impl : NodeType, k : () => Unit) {
    println("###################################################")
    println("##### Solving uses violations toward %s ######".format(impl))
    val wrongUsers = redirectTowardExistingAbstractions(impl, impl.wrongUsers)

    if (wrongUsers.nonEmpty){
      singleAbsIntro(impl, wrongUsers){
        case None =>
          singleAbsIntro(impl, wrongUsers, 2) {
            case None =>
              decisionMaker.modifyConstraints(LiteralNodeSet(wrongUsers), impl)
              /*if(impl.wrongUsers.nonEmpty)
                throw new AGError ("cannot solve uses toward " + impl)*/
              k ()
            case _ => k ()
          }
        case _ => k ()
      }
    }
  }

  var newCterNumGen = 0
  def solveContains(wronglyContained : NodeType, k : () => Unit) {
    graph.transformations.startSequence()
    // detach for host searching : do not want to consider parent constraints
    val oldCter = wronglyContained.container

    if(wronglyContained.container != wronglyContained)
      oldCter.content_-=(wronglyContained, register = false)

    /*
      snode is either the wrongly contained node
      either another container introduced to contains the wrongly contained node
     */
    def moveToNewCter (snode : Option[NodeType]) {
      snode match {
        case Some(newCter) =>
          //re-attach before moving
          if(oldCter != wronglyContained)
          oldCter content_+= (wronglyContained, register = false)
          wronglyContained.moveTo(newCter)
        case None =>
          graph.transformations.undo()
          decisionMaker.modifyConstraints(LiteralNodeSet(wronglyContained.container), wronglyContained)
          if(wronglyContained.isWronglyContained)
            throw new SolvingError("Cannot solve %s contains violation".
              format(AGEdge.contains(wronglyContained.container, wronglyContained)))
      }
      k ()
    }

    findHost(wronglyContained, List(),
      "Moving " + wronglyContained + "\nSearching a host for it",
      (potentialHost: NodeType) => !(potentialHost interloperOf wronglyContained)){
      case None =>
        val newCter = graph.nodeKinds.find(_.canContain(wronglyContained.kind)) match {
          case None =>
            throw new AGError("do not know how to create a valid parent for " + wronglyContained.kind)
          case Some(parentKind) =>
            newCterNumGen += 1
            graph.addNode("%s_container%d".format(wronglyContained.name, newCterNumGen), parentKind)
        }

        wronglyContained.addHideFromRootException(newCter)

        findHost(newCter, List(),
          "Moving " + wronglyContained + "\n" +
            newCter + " created to contain it.\n" +
            "Searching a host for " + newCter, n => n != newCter) {
          case None => moveToNewCter(None)

          case Some(h) =>
            h.content_+=(newCter)
            moveToNewCter(Some(newCter))
        }
      case somehost => moveToNewCter(somehost)
    }
  }

  def solveViolationsToward(target : NodeType) (k: () => Unit){
    println("solving violation toward "+ target)

    def end() =
      if(target.wrongUsers.nonEmpty)
        solveUsesToward(target, k)

    if(target.isWronglyContained)
      solveContains(target, end)
    else
      end()
  }

  def solve(step : () => Unit) {

    def aux(){
      decisionMaker.violationTarget {
        case None => ()
        case Some(target) =>
          solveViolationsToward(target){ () =>
            step()
            aux()
          }
      }
    }

    aux()
  }


}