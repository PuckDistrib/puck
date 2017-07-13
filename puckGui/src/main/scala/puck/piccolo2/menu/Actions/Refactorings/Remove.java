package puck.piccolo2.menu.Actions.Refactorings;

import org.piccolo2d.event.PInputEvent;
import puck.control.PrintErrOrPushGraph;
import puck.control.PuckControl;
import puck.graph.ConcreteNode;
import puck.graph.DependencyGraph;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.MenuItemEventHandler;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.util.LoggedEither;

/**
 * Created by Vincent Hudry on 26/06/2017.
 */
public class Remove extends MenuItemEventHandler {

    private PuckControl control;

    private DependencyGraph DG;
    private TransformationRules TR;
    private int nodeId;
    private ConcreteNode CN;

    public Remove(PuckControl control){
        this.control=control;
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        PiccoloCustomNode PCN= (PiccoloCustomNode) target.getParent().getParent();
        TR=control.graphUtils().Rules();
        DG=control.graph();
        nodeId=PCN.getidNode();
        CN=DG.getConcreteNode(nodeId);
        LoggedEither le=TR.remove().concreteNode(DG.mileStone(),CN);
        control.historyHandler().bus().publish(new PrintErrOrPushGraph("Remove Node Action Failure",le));

    }
}
