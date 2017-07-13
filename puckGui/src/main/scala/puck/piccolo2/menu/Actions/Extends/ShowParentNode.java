package puck.piccolo2.menu.Actions.Extends;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.piccolo2.menu.Actions.ShowNodeUsersOf;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.*;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.util.HashMap;

/**
 * Created by Vincent Hudry on 08/06/2017.
 */
public class ShowParentNode extends ShowNodeUsersOf {
    public ShowParentNode(PuckControl control, DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object, PiccoloCustomNode> idNodeMap) {
        super(control,DG, arrowNodesHolder, idNodeMap);
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> parent=DG.superTypes(nodeId);
        for(Iterator<Object> iterator = parent.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            PNode from=target.getContent();
            PNode to = idNodeMap.get(O).getContent();
            if((to.getParent() instanceof PiccoloCustomNode)&&(!((PiccoloCustomNode) to.getParent()).isHidden()))
            arrowNodesHolder.addArrow(new ParrowExtends(from,to));

        }
    }
}
