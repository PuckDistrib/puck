package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.graph.Type;
import puck.piccolo2.Parrows.*;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.*;
import java.util.Collection;
import java.util.HashMap;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class ShowViolations extends MenuItemEventHandler {

    private DependencyGraph DG;
    private ArrowNodesHolder arrowNodesHolder;
    private HashMap<Object,PiccoloCustomNode> idNodeMap;

    private PuckControl control;

    private PiccoloCustomNode root;
    
    
    public ShowViolations(PuckControl control, DependencyGraph DG, ArrowNodesHolder arrowNodesHolder, HashMap<Object,PiccoloCustomNode> idNodeMap){
        this.DG=DG;
        this.arrowNodesHolder=arrowNodesHolder;
        this.idNodeMap=idNodeMap;
        this.control=control;
        this.root=idNodeMap.get(0);
    }

    private boolean fullSearch=true;

    public void setFullSearch(boolean b){
        fullSearch=b;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        PiccoloCustomNode target=(PiccoloCustomNode) this.target.getParent().getParent();

        //region for hierarchy nodes

        Collection<PiccoloCustomNode> hierarchy1;
        Collection<PiccoloCustomNode> hierarchy2;
                if(fullSearch) {
                    hierarchy1 = root.getHierarchy();
                    hierarchy2 = root.getHierarchy();
                }
                else{
                    hierarchy1=target.getHierarchy();
                    hierarchy2=target.getHierarchy();
                }
        for(PiccoloCustomNode PCN1:hierarchy1)
            for(PiccoloCustomNode PCN2:hierarchy2){
                int nodeId1=PCN1.getidNode();
                int nodeId2=PCN2.getidNode();
                if(DG.usersOf(nodeId1).contains(nodeId2)){
                    if(control.constraints().get().isForbidden(DG,nodeId2,nodeId1)){
                        if(!PCN1.isHidden()&&!PCN2.isHidden())
                            arrowNodesHolder.addArrow(new ParrowFat(PCN1.getContent(),PCN2.getContent(),5,Color.RED));
                        if(PCN1.isHidden()||PCN2.isHidden())
                        arrowNodesHolder.addArrow(new ParrowDottedFat(PCN1.getHigherParent().getContent(),PCN2.getHigherParent().getContent(),10,5,Color.RED,PCN1.getContent(),PCN2.getContent(), ParrowDottedFat.Type.Uses));
                    }
                }
                else if(DG.contains(nodeId1,nodeId2)){
                    if(control.constraints().get().isForbidden(DG,nodeId1,nodeId2)){
                        if(!PCN1.isHidden()&&!PCN2.isHidden())
                        arrowNodesHolder.addArrow(new ParrowContainsViolations(PCN1.getContent(),PCN2.getContent()));
                        else
                            arrowNodesHolder.addArrow(new ParrowDottedFat(PCN1.getHigherParent().getContent(),PCN2.getHigherParent().getContent(),10,5,Color.RED,PCN1.getContent(),PCN2.getContent(), ParrowDottedFat.Type.Contains));
                    }
                }
            }
        //endregion

        //region count arrows
        arrowNodesHolder.clearCounters();
        for(Parrow parrow:arrowNodesHolder.getVisibleArrows())
            if(parrow instanceof ParrowDottedFat)
                arrowNodesHolder.updateCount((ParrowDottedFat) parrow);
        //endregion
    }
}
