package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import puck.piccolo2.Util;

import java.awt.geom.Point2D;

public class ParrowUses extends Parrow{

    private double spacing;

    public ParrowUses(Point2D from, Point2D to, double spacing){
        super(from,to);
        TriangleHollow head=new TriangleHollow();

        double dist= Util.distance(from.getX(),from.getY(),to.getX(),to.getY());

        double num=dist/spacing;

        PNode lines=new PNode();

        double fromx=from.getX();
        double fromy=from.getY();
        double tox=Util.lerp(0,num,fromx,to.getX(),1);
        double toy=Util.lerp(0,num,fromy,to.getY(),1);
        double dstx=tox-fromx;
        double dsty=toy-fromy;

        int j=1;
        for(double i=0;i<num-1;i++){
            PPath line= PPath.createLine(fromx,fromy,tox,toy);
            j++;
            if(j%2==0){
                lines.addChild(line);
            }

            fromx+=dstx;
            tox+=dstx;
            fromy+=dsty;
            toy+=dsty;

        }

        double theta=Math.atan2(to.getY()-from.getY(),to.getX()-from.getX())+Math.toRadians(90);
        head.translate(to.getX(),to.getY());
        head.rotate(theta);

        addChild(head);
        addChild(lines);

        this.spacing=spacing;

    }

    public ParrowUses(PNode from, PNode to,double spacing){
        this(from.getBounds().getCenter2D(),to.getBounds().getCenter2D(),spacing);
        this.from=from;
        this.to=to;
    }

    @Override
    public Parrow redraw() {
        removeAllChildren();
        return new ParrowUses(from,to,spacing);
    }
}
