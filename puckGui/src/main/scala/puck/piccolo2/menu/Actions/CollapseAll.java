package puck.piccolo2.menu.Actions;

import org.piccolo2d.PCanvas;
import org.piccolo2d.event.PInputEvent;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.piccolo2.Parrows.Parrow;
import puck.piccolo2.Parrows.ParrowDottedFat;
import puck.piccolo2.node.PiccoloCustomNode;

/**
 * Created by Vincent Hudry on 16/06/2017.
 */
public class CollapseAll extends MenuItemEventHandler{

    private PiccoloCustomNode root;
    private ArrowNodesHolder ANH;

    public CollapseAll(PiccoloCustomNode root, ArrowNodesHolder ANH){
        this.root=root;
        this.ANH=ANH;
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        ((PiccoloCustomNode)(target.getParent().getParent())).collapseAll();
        root.setLayout();
        root.updateContentBoundingBoxes(false,null);
        for(Parrow p:ANH.getVisibleArrows())
        ANH.updatePosition(p);
        ANH.clearCounters();
        for(Parrow ar:ANH.getVisibleArrows())
            if(ar instanceof ParrowDottedFat)
                ANH.updateCount((ParrowDottedFat) ar);
    }
}
