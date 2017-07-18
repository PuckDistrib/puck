package puck.piccolo2.LayoutStack;

import puck.piccolo2.node.PiccoloCustomNode;

/**
 * Created by Vincent Hudry on 26/06/2017.
 */
public class NodeState {

    private int nodeId;
    private boolean areChildrenIHidden;

    public NodeState(PiccoloCustomNode PCN){
        nodeId=PCN.getidNode();
        areChildrenIHidden=PCN.getChildren().size()==0;
    }

    public int getNodeId() {
        return nodeId;
    }

    public boolean AreChildrenIHidden() {
        return areChildrenIHidden;
    }
}
