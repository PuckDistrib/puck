package puck.piccolo2.menu.Actions.Uses;

import org.piccolo2d.PNode;
import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.piccolo2.menu.Actions.ShowNodeUsersOf;
import puck.piccolo2.node.NodeContent;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.*;
import scala.collection.Iterator;
import scala.collection.immutable.Set;

import java.awt.*;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 08/06/2017.
 */
public class ShowNodeUsedBy extends ShowNodeUsersOf {

    public ShowNodeUsedBy(PuckControl control, DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object, PiccoloCustomNode> idNodeMap) {
        super(control,DG, arrowNodesHolder, idNodeMap);
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        int nodeId=target.getidNode();

        Set<Object> usersof=DG.usedBy(nodeId);
        for(Iterator<Object> iterator = usersof.toIterator(); iterator.hasNext();) {
            Object O=iterator.next();
            PNode from = idNodeMap.get(O).getContent();
            PNode to=target.getContent();
            //if((from.getParent().getParent() instanceof  PiccoloCustomNode)&&!((PiccoloCustomNode) from.getParent().getParent()).isHidden()){
            if(from.getParent() instanceof PiccoloCustomNode &&! ((PiccoloCustomNode)from.getParent()).isHidden()){

                int user = target.getidNode();
                int used = (int)O;

                boolean forbidden = control.constraints().get().isForbidden(DG,user,used);

                if(forbidden)
                    arrowNodesHolder.addArrow(new ParrowFat(from,to,5, Color.RED));
                else
                    arrowNodesHolder.addArrow(new ParrowUses(from, to, 10));
            }
            else
                if(from.getParent() instanceof PiccoloCustomNode){
                int user=target.getidNode();
                int used = (int)O;

                boolean forbidden=control.constraints().get().isForbidden(DG,user,used);

                if(forbidden){
                    NodeContent virtualFrom=((PiccoloCustomNode)from.getParent()).getHigherParent().getContent();
                    ParrowDottedFat arrow=new ParrowDottedFat(virtualFrom,to,10,5,Color.RED,from,to, ParrowDottedFat.Type.Uses);
                    arrowNodesHolder.addArrow(arrow);
                }

                }
        }
    }
}
