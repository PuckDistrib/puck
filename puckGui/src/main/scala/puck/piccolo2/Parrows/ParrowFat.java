package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import puck.piccolo2.Util;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.*;
import java.awt.geom.Point2D;

public class ParrowFat extends Parrow{

    private float width;
    private Color color;

    public ParrowFat(Point2D from, Point2D to, float width, Color color){
        super(from,to);
        Triangle head=new Triangle(color);
        double dist= Util.distance(from.getX(),from.getY(),to.getX(),to.getY());
        PPath rect=PPath.createRectangle(0,0,width,dist-head.getBounds().height/2);
        rect.setPaint(color);
        double theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90+180);
        rect.translate(from.getX(),from.getY());
        rect.rotate(theta);
        rect.translate(-width/2,0);
        addChild(rect);

        theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90);
        head.translate(to.getX(),to.getY());
        head.rotate(theta);
        addChild(head);

        this.width=width;
        this.color=color;

    }

    public ParrowFat(PNode from, PNode to, float width, Color color){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D(),width,color);
        this.from=from;
        this.to=to;
    }

    @Override
    public Parrow redraw() {
        removeAllChildren();
        PiccoloCustomNode vpfrom=(PiccoloCustomNode) from.getParent();
        PiccoloCustomNode vpto = (PiccoloCustomNode) to.getParent();
        PNode vphf=vpfrom.getHigherParent().getContent();
        PNode vpht=vpto.getHigherParent().getContent();
        if(vphf==vpfrom.getContent()&&vpht==vpto.getContent())
            return new ParrowFat(vphf,vpht,width,color);
        else
            return new ParrowDottedFat(vphf,vpht,10,width,color,vpfrom.getContent(),vpto.getContent(), ParrowDottedFat.Type.Uses);


        //ParrowFat arrow=new ParrowFat(from,to,width,color);
        //return arrow;
    }
}
