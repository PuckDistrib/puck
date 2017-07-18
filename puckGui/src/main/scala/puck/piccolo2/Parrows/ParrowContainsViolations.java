package puck.piccolo2.Parrows;

import org.piccolo2d.PNode;
import org.piccolo2d.nodes.PPath;
import org.piccolo2d.util.PBounds;
import puck.piccolo2.node.PiccoloCustomNode;

import java.awt.*;

/**
 * Created by Vincent Hudry on 12/07/2017.
 */
public class ParrowContainsViolations extends  ParrowFat{
    public ParrowContainsViolations(PNode from, PNode to) {
        super(from,to,0,null);
        removeAllChildren();

        PiccoloCustomNode dst=(PiccoloCustomNode)to.getParent();
        if(dst.getContent().getText().getTextPaint()!=Color.RED) {
            dst.getContent().getText().setTextPaint(Color.RED);
            PBounds bounds = dst.getContent().getUnionOfChildrenBounds(null);
            PPath line = PPath.createLine(0, bounds.getHeight(), bounds.getWidth(), bounds.getHeight());
            line.setPaint(Color.RED);
            dst.getContent().addChild(line);
        }
    }
    @Override
    public Parrow redraw(){
        PiccoloCustomNode vpfrom=(PiccoloCustomNode) from.getParent();
        PiccoloCustomNode vpto = (PiccoloCustomNode) to.getParent();
        PNode vphf=vpfrom.getHigherParent().getContent();
        PNode vpht=vpto.getHigherParent().getContent();
        if(vphf==vpfrom.getContent()&&vpht==vpto.getContent())
            return new ParrowContainsViolations(vphf,vpht);
        else
            return new ParrowDottedFat(vphf,vpht,10,5,Color.RED,vpfrom.getContent(),vpto.getContent(), ParrowDottedFat.Type.Contains);
    }
}
