package puck.graph

/**
 * Created by lorilan on 15/05/14.
 */
class AccessGraphSpec extends UnitSpec{
  val ag : AccessGraph[VanillaKind] = new AccessGraph(AGNode)

  var a0 : AGNode[VanillaKind] = _
  var aa : AGNode[VanillaKind] = _
  var ab : AGNode[VanillaKind] = _
  var ac : AGNode[VanillaKind] = _


  "An Access Graph" should "have a root node" in {
    ag.root should not be (null)
    ag.iterator.hasNext should be (true)
    ag.iterator.next() should be (ag.root)
    ag.size should be (1)
  }

  it should "have nodes with a unique full name" in {

    a0 = ag.addNode("a", "a", VanillaNodeKind())
    val a2 = ag.addNode("a", "a", VanillaNodeKind())
    aa = ag.addNode("a.a", "a", VanillaNodeKind())
    ab = ag.addNode("a.b", "b", VanillaNodeKind())
    ac = ag.addNode("a.c", "c", VanillaNodeKind())


    a0 content_+= aa
    a0 content_+= ab
    a0 content_+= ac

    a0 should equal (a2)
    a0 should not equal (aa)

  }

  it should "contains nodes that are added to it" in {
    ag.nodes.toStream should contain (a0)
    ag.nodes.toStream should contain (aa)
    ag.nodes.toStream should contain (ab)
    ag.nodes.toStream should contain (ac)
  }


 /* "An Access Graph Node" should "contains its content" in {
    a0.contains(aa) should be (true)
    a0.contains(ab) should be (true)
    a0.contains(ac) should be (true)

    aa.container should be (a0)
    ab.container should be (a0)
    ac.container should be (a0)
  }*/
}
