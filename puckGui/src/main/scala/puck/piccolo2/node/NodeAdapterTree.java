package puck.piccolo2.node;

import org.piccolo2d.nodes.PImage;
import org.piccolo2d.nodes.PText;
import puck.control.PuckControl;
import puck.graph.DGNode;
import puck.graph.DependencyGraph;
import puck.view.NodeKindIcons;
import scala.Tuple2;
import scala.collection.Iterator;
import scala.collection.immutable.Map;
import scala.collection.immutable.Set;

import java.util.ArrayList;
import java.util.Collection;
import java.util.stream.Collectors;

/**
 * Created by Vincent Hudry on 31/05/2017.
 */
public class NodeAdapterTree implements Tree {

    private int nodeId;

    public int getNodeId() {
        return nodeId;
    }

    private Collection<NodeAdapterTree> children;

    private NodeKindIcons icons;
    private DependencyGraph DG;

    public NodeAdapterTree(DependencyGraph DG, int nodeId,NodeKindIcons icons){
        this.children=new ArrayList<>();
        this.DG=DG;
        this.nodeId=nodeId;

        this.icons=icons;

        Set<Object> childrenObjects = DG.content(nodeId);
        for (Iterator<Object> iterator=childrenObjects.toIterator();iterator.hasNext();){
            NodeAdapterTree NTA=new NodeAdapterTree(DG,(int)iterator.next(),icons);
            this.children.add(NTA);
        }
    }

    @Override
    public Collection<Tree> getChildren() {
        return children.stream().map(x->(Tree)x).collect(Collectors.toList());
    }

    @Override
    public NodeContent getContent() {
        return new NodeContent(new PText(toString()),new PImage(icons.iconOfKind(DG.getNode(nodeId).kind()).getImage()));

    }

    @Override
    public String toString(){
        DGNode DGN=DG.getNode(nodeId);
        String name=DGN.name();
        StringBuilder sb=new StringBuilder();
        sb.append(nodeId);
        sb.append("-");
        sb.append(name);
        return sb.toString();

    }


}
