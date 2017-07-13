package puck.piccolo2.node;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PImage;
import org.piccolo2d.nodes.PText;
import org.piccolo2d.util.PBounds;

import java.awt.geom.Point2D;

/**
 * Created by Vincent Hudry on 07/06/2017.
 */
public class NodeContent extends PNode {
    private PText text;
    private PImage icon;
    private int margin=10;

    //region getters/setters
    public int getMargin() {
        return margin;
    }

    public void setMargin(int margin) {
        this.margin = margin;
    }

    public PText getText(){
        return text;
    }

    public void setText(String text){
        removeChild(this.text);
        this.text = new PText(text);
        addChild(this.text);
        this.text.setBounds(icon.getBounds().getWidth()+margin,0,this.text.getWidth(),this.text.getHeight());
        PBounds bounds=this.getUnionOfChildrenBounds(null);
        this.setBounds(bounds.getX(),bounds.getY(),bounds.getWidth(),bounds.getHeight());
    }

    public PImage getIcon(){
        return icon;
    }

    //endregion
    
    public NodeContent(PText text, PImage Icon){
        this.text=text;
        this.icon=Icon;
        addChild(this.text);
        addChild(Icon);
        text.setBounds(Icon.getBounds().getWidth()+margin,0,text.getWidth(),text.getHeight());
        PBounds bounds=this.getUnionOfChildrenBounds(null);
        this.setBounds(bounds.getX(),bounds.getY(),bounds.getWidth(),bounds.getHeight());
    }

    public String toString(){
        return text.getText();
    }
}
