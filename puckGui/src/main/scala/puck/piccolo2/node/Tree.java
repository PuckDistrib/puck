package puck.piccolo2.node;

import java.util.Collection;

public interface Tree {
    Collection<Tree> getChildren();
    NodeContent getContent();
    int getNodeId();
}
