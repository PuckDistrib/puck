package puck.piccolo2.menu.Actions;

import org.piccolo2d.event.PInputEvent;
import puck.piccolo2.menu.DisplayUsesMenu;
import puck.piccolo2.node.PiccoloCustomNode;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class CloseMenu extends MenuItemEventHandler {
    private DisplayUsesMenu menu;
    public CloseMenu(DisplayUsesMenu menu){
        this.menu=menu;
    }
    @Override
    public void mouseClicked(PInputEvent e){
        menu.clear();
    }
}
