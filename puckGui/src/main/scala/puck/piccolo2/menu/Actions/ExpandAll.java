package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.piccolo2.Parrows.Parrow;
import puck.piccolo2.node.PiccoloCustomNode;

/**
 * Created by Vincent Hudry on 16/06/2017.
 */
public class ExpandAll extends MenuItemEventHandler{

    private PiccoloCustomNode root;
    private ArrowNodesHolder ANH;

    public ExpandAll(PiccoloCustomNode root,ArrowNodesHolder ANH){
        this.root=root;
        this.ANH=ANH;
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        ((PiccoloCustomNode)(target.getParent().getParent())).expandAll();
        root.setLayout();
        root.updateContentBoundingBoxes(false,null);
        for(Parrow p:ANH.getVisibleArrows())
            ANH.updatePosition(p);
    }
}
