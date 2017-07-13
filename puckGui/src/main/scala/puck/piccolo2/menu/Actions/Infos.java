package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.control.NodeClicked;
import puck.control.PuckControl;
import puck.graph.DGNode;
import puck.piccolo2.node.PiccoloCustomNode;

/**
 * Created by Vincent Hudry on 23/06/2017.
 */
public class Infos extends MenuItemEventHandler {
    private PuckControl control;
    private DGNode node;

    public Infos(PuckControl control) {
        this.control = control;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        node=control.graph().getNode(nodeId);

        control.historyHandler().bus().publish(new NodeClicked(node));
    }
}
