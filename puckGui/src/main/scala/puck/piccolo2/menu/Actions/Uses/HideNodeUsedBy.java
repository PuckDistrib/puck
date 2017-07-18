package puck.piccolo2.menu.Actions.Uses;

import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.piccolo2.menu.Actions.ShowNodeUsersOf;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.piccolo2.Parrows.Parrow;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.util.Collection;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 08/06/2017.
 */
public class HideNodeUsedBy extends ShowNodeUsersOf {
    public HideNodeUsedBy(PuckControl control, DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object, PiccoloCustomNode> idNodeMap) {
        super(control,DG, arrowNodesHolder, idNodeMap);
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> usersof=DG.usedBy(nodeId);
        Collection<Parrow> quiver=arrowNodesHolder.getVisibleArrows();
        for(Parrow A:quiver){
            PiccoloCustomNode from= (PiccoloCustomNode) A.getFrom().getParent();
            PiccoloCustomNode to= (PiccoloCustomNode) A.getTo().getParent();
            for(Iterator<Object> iterator = usersof.iterator(); iterator.hasNext();)
                if(from.getidNode()==(int)iterator.next()){
                    arrowNodesHolder.removeArrow(A);
                }
        }

    }

}
