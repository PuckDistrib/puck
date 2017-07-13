package puck.piccolo2.menu;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;

import java.awt.geom.AffineTransform;
import java.util.ArrayList;
import java.util.List;

/**
 * Created by Vincent Hudry on 01/06/2017.
 */
public class Menu extends PNode {

    private List<MenuItem> items;

    public Menu(){
        items=new ArrayList<>();
    }

    public void add(MenuItem item){
        items.add(item);
    }

    public void remove(MenuItem item){
        items.remove(item);
        removeChild(item);
    }

    public void draw(PNode target){

        double x= target.getGlobalTranslation().getX()+target.getBounds().getCenterX();
        double y = target.getGlobalTranslation().getY()+target.getBounds().getCenterY();

        for(MenuItem m:items) {
            m.draw(target);

            m.setTransform(AffineTransform.getTranslateInstance(0,0));

            m.translate(x,y);
            addChild(m);
            y+=m.getHeight();
        }
    }

}
