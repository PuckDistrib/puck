package puck.piccolo2.node;

import org.piccolo2d.PCanvas;
import org.piccolo2d.PNode;
import org.piccolo2d.event.PBasicInputEventHandler;
import org.piccolo2d.event.PInputEvent;
import org.piccolo2d.event.PInputEventFilter;
import org.piccolo2d.nodes.PText;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.piccolo2.Parrows.Parrow;
import puck.piccolo2.Parrows.ParrowDottedFat;
import puck.piccolo2.menu.DisplayUsesMenu;

import java.awt.event.InputEvent;
import java.util.Collection;

public class PCustomInputEventHandler extends PBasicInputEventHandler {
    private PiccoloCustomNode node;
    private PiccoloCustomNode tree;

    private static DisplayUsesMenu menu;

    private PCanvas canvas;

    private ArrowNodesHolder ANH;

    public PCustomInputEventHandler(PiccoloCustomNode node,PiccoloCustomNode tree, DisplayUsesMenu menu,PCanvas canvas, ArrowNodesHolder ANH){
        setEventFilter(new PInputEventFilter(InputEvent.BUTTON1_MASK & InputEvent.BUTTON2_MASK));
        this.node=node;
        this.tree=tree;

        PCustomInputEventHandler.menu=menu;

        this.canvas=canvas;

        this.ANH=ANH;
    }

    @Override
    public void mouseClicked(PInputEvent e){
        if(e.isLeftMouseButton()) {
            node.toggleChildren();
            tree.setLayout();
            tree.updateContentBoundingBoxes(false,canvas);
            //optional
            menu.clear();

            //arrows
            //you can use the ArrowNodeHolder ANH

            for(Parrow arrow:ANH.getVisibleArrows())
                ANH.updatePosition(arrow);
            ANH.clearCounters();
            for(Parrow ar:ANH.getVisibleArrows())
                if(ar instanceof ParrowDottedFat)
                    ANH.updateCount((ParrowDottedFat) ar);
            ANH.hide_show_arrows(node);

        }
        if(e.isRightMouseButton()){
            if(e.getPickedNode() instanceof PText)
            menu.draw(e.getPickedNode());
        }
    }
}
