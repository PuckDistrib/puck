package puck.piccolo2.menu.Actions.Refactorings;

import org.piccolo2d.event.PInputEvent;
import puck.control.PuckControl;
import puck.graph.DependencyGraph;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;
import puck.piccolo2.node.PiccoloCustomNode;

import javax.swing.*;
import java.util.HashMap;


/**
 * Created by Vincent Hudry on 09/06/2017.
 */
public class Rename extends MenuItemEventHandler{

    private DependencyGraph DG;
    private TransformationRules TR;
    private PuckControl control;
    private int nodeId;

    HashMap<Object,PiccoloCustomNode> nodeByID;

    public Rename(PuckControl control,HashMap<Object,PiccoloCustomNode> nodeByID){
        DG=control.graph();
        TR=control.graphUtils().Rules();
        this.control=control;
        this.nodeByID=nodeByID;
    }

    @Override
    public void mouseClicked(PInputEvent e) {

        //TODO : breaks if renaming a created node (already broken)

        String dialout = JOptionPane.showInputDialog("Rename","");
        if(dialout==null||dialout.equals(""))
            return;
        if(target.getParent()!=null)
        nodeId=((PiccoloCustomNode)(target.getParent().getParent())).getidNode();
        DependencyGraph DG2 = TR.rename().apply(DG,nodeId,dialout);
        control.historyHandler().pushGraph(DG2);
        DG=control.graph();
        nodeByID.get(nodeId).getContent().setText(nodeId+"-"+dialout);
        nodeByID.get(0).setLayout();
    }
}
