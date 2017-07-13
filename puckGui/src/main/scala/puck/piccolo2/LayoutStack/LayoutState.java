package puck.piccolo2.LayoutStack;

import puck.piccolo2.node.PiccoloCustomNode;

import java.util.Collection;
import java.util.HashMap;
import java.util.HashSet;

/**
 * Created by Vincent Hudry on 26/06/2017.
 */
public class LayoutState {

   private Collection<NodeState> states;

   public LayoutState(PiccoloCustomNode PCN){
       states=new HashSet<>();
       getLayout(PCN);
   }

   public void getLayout(PiccoloCustomNode PCN){
      NodeState nodeState=new NodeState(PCN);
      states.add(nodeState);
      for(PiccoloCustomNode PCNchild:PCN.getAllChildren()){
         getLayout(PCNchild);
      }
   }

    public void setLayout(HashMap<Object,PiccoloCustomNode> idNodeMap) {
        for(NodeState NS:states){
            PiccoloCustomNode PCN=idNodeMap.get(NS.getNodeId());
            if(NS.AreChildrenIHidden())
                PCN.hideChildren();
            else
                PCN.showChildren();
        }
        idNodeMap.get(0).setLayout();
    }
}
