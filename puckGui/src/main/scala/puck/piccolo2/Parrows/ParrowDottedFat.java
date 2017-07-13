package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import puck.piccolo2.Util;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.*;
import java.awt.geom.Point2D;

/**
 * Created by Vincent Hudry on 03/07/2017.
 */
public class ParrowDottedFat extends Parrow {

    private double spacing;
    private float width;
    private  Color color;

    private PNode virtualFrom;
    private PNode virtualTo;

    public enum Type {Uses,Contains}
    private Type type;

    public ParrowDottedFat(Point2D from,Point2D to, double spacing, float width,Color color){
        super(from,to);
        this.spacing=spacing;
        this.width=width;
        this.color=color;

        Triangle head=new Triangle(color);
        double dist= Util.distance(from.getX(),from.getY(),to.getX(),to.getY());
        double num=dist/spacing;

        PNode lines=new PNode();

        double fromx=from.getX();
        double fromy=from.getY();
        double tox=Util.lerp(0,num,fromx,to.getX(),1);
        double toy=Util.lerp(0,num,fromy,to.getY(),1);
        double dstx=tox-fromx;
        double dsty=toy-fromy;

        double rLen=Util.distance(fromx,fromy,tox,toy);

        double theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90);

        int j=1;
        for(double i=0;i<num-1;i++){
            j++;
            if(j%2==1){
                PPath rect=PPath.createRectangle(0,0,width,rLen);
                rect.setPaint(color);
                rect.translate(fromx,fromy);
                rect.rotate(theta);
                rect.translate(-width/2,0);
                lines.addChild(rect);
            }
            fromx+=dstx;
            fromy+=dsty;
        }

        head.translate(to.getX(),to.getY());
        head.rotate(theta);
        addChild(head);
        addChild(lines);

    }

    public ParrowDottedFat(PNode from,PNode to,double spacing,float width,Color color,PNode virtualFrom,PNode virtualTo,Type type){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D(),spacing,width,color);
        this.from=from;
        this.to=to;

        this.virtualFrom=virtualFrom;
        this.virtualTo=virtualTo;

        this.type=type;
    }

    @Override
    public Parrow redraw() {
        removeAllChildren();
            PiccoloCustomNode vpfrom=(PiccoloCustomNode) this.getVirtualFrom().getParent();
            PiccoloCustomNode vpto = (PiccoloCustomNode) this.getVirtualTo().getParent();
            PNode vphf=vpfrom.getHigherParent().getContent();
            PNode vpht=vpto.getHigherParent().getContent();
            if(vphf==vpfrom.getContent()&&vpht==vpto.getContent()) {
                if (type == Type.Uses)
                    return (new ParrowFat(vphf, vpht, width, color));
                else if (type==Type.Contains)
                    return (new ParrowContainsViolations(vphf,vpht));
                else return null;
            }
                else
            return (new ParrowDottedFat(vphf,vpht,spacing,width, color,vpfrom.getContent(),vpto.getContent(),type));
    }

    public PNode getVirtualFrom() {
        return virtualFrom;
    }

    public PNode getVirtualTo() {
        return virtualTo;
    }

    @Override
    public boolean equals(Object arrow){
        if(arrow instanceof ParrowDottedFat){
            if(getFrom()==((ParrowDottedFat) arrow).getFrom()
                    &&getTo()==((ParrowDottedFat) arrow).getTo()
                    &&getVirtualFrom()==((ParrowDottedFat) arrow).getVirtualFrom()
                    &&getVirtualTo()==((ParrowDottedFat) arrow).getVirtualTo())
                return true;
            else
                return false;
        }
        else
            return false;
    }
}
