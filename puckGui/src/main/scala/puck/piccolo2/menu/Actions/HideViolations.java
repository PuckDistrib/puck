package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import puck.piccolo2.Parrows.Parrow;
import puck.piccolo2.Parrows.ParrowDottedFat;
import puck.piccolo2.Parrows.ParrowFat;

/**
 * Created by Vincent Hudry on 07/07/2017.
 */
public class HideViolations extends MenuItemEventHandler {
    private ArrowNodesHolder ANH;

    public HideViolations(ArrowNodesHolder ANH){
        this.ANH=ANH;
    }

    @Override
    public void mouseClicked(PInputEvent e) {
        for(Parrow parrow:ANH.getVisibleArrows()) {
            if (parrow instanceof ParrowFat || parrow instanceof ParrowDottedFat) {
                ANH.removeArrow(parrow);
            }
        }
        ANH.clearCounters();
    }
}
