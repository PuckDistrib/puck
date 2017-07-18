package puck.piccolo2.menu;

import org.piccolo2d.PNode;
import puck.control.PuckControl;
import puck.graph.*;
import puck.graph.transformations.TransformationRules;
import puck.piccolo2.menu.Actions.*;
import puck.piccolo2.menu.Actions.Refactorings.AddChildKind;
import puck.piccolo2.menu.Actions.Refactorings.Remove;
import puck.piccolo2.menu.Actions.Refactorings.Rename;
import puck.piccolo2.menu.Actions.Extends.HideChildrenNode;
import puck.piccolo2.menu.Actions.Extends.HideParentNode;
import puck.piccolo2.menu.Actions.Extends.ShowChildrenNode;
import puck.piccolo2.menu.Actions.Extends.ShowParentNode;
import puck.piccolo2.menu.Actions.Uses.HideNodeUsedBy;
import puck.piccolo2.menu.Actions.Uses.HideNodeUsersOf;
import puck.piccolo2.menu.Actions.Uses.ShowNodeUsedBy;
import puck.piccolo2.menu.Actions.ShowNodeUsersOf;
import puck.piccolo2.node.PiccoloCustomNode;
import puck.piccolo2.Parrows.ArrowNodesHolder;
import scala.Tuple2;
import scala.collection.*;

import java.awt.geom.AffineTransform;
import java.util.*;
import java.util.List;

/**
 * Created by Vincent Hudry on 02/06/2017.
 */
public class DisplayUsesMenu extends PNode {

    private Menu menu;

    private PuckControl control;

    private HashMap<Object,PiccoloCustomNode> idNodeMap;

    public DisplayUsesMenu(PuckControl control, ArrowNodesHolder ANH, HashMap<Object,PiccoloCustomNode> idNodeMap){
        menu=new Menu();

        MenuItem closes=new MenuItem("Close Menu",new CloseMenu(this));
        menu.add(closes);

        MenuItem infos=new MenuItem("Infos",new Infos(control));
        menu.add(infos);

        DependencyGraph DG=control.graph();
        TransformationRules TR=control.graphUtils().Rules();

        //region for Violations

        ShowViolations SV2=new ShowViolations(control,control.graph(),ANH,idNodeMap);
        SV2.setFullSearch(false);
        MenuItem sv2mi=new MenuItem("Show violations in hierarchy",SV2);
        menu.add(sv2mi);

        ShowViolations SV=new ShowViolations(control,control.graph(),ANH,idNodeMap);
        SV.setFullSearch(true);
        MenuItem showviolations=new MenuItem("Show All violations",SV);
        menu.add(showviolations);

        HideViolations HV=new HideViolations(ANH);
        MenuItem hideviolations=new MenuItem("Hide violations",HV);
        menu.add(hideviolations);

        //endregion

        //region for showuses

        ShowNodeUsersOf SNUO =new ShowNodeUsersOf(control,DG,ANH,idNodeMap);
        MenuItem showusersof=new MenuItem("Show users of", SNUO);
        menu.add(showusersof);

        HideNodeUsersOf HNUO=new HideNodeUsersOf(control,DG,ANH,idNodeMap);
        MenuItem hideusersof=new MenuItem("Hide users of",HNUO);
        menu.add(hideusersof);

        ShowNodeUsedBy SNOB = new ShowNodeUsedBy(control,DG,ANH,idNodeMap);
        MenuItem shownodeusedby=new MenuItem("Show used by",SNOB);
        menu.add(shownodeusedby);

        HideNodeUsedBy HNUB=new HideNodeUsedBy(control,DG,ANH,idNodeMap);
        MenuItem hidenodeusedby=new MenuItem("Hide used by",HNUB);
        menu.add(hidenodeusedby);

        //endregion

        //region for Uses
        ShowParentNode SPN=new ShowParentNode(control,DG,ANH,idNodeMap);
        MenuItem showparentnode=new MenuItem("Show parents",SPN);
        menu.add(showparentnode);

        HideParentNode HPN=new HideParentNode(control,DG,ANH,idNodeMap);
        MenuItem hideparentnode=new MenuItem("Hide parents",HPN);
        menu.add(hideparentnode);

        ShowChildrenNode SCN=new ShowChildrenNode(control,DG,ANH,idNodeMap);
        MenuItem showchildrennode=new MenuItem("Show children",SCN);
        menu.add(showchildrennode);

        HideChildrenNode HCN=new HideChildrenNode(control,DG,ANH,idNodeMap);
        MenuItem hidechildrennode=new MenuItem("Hide children",HCN);
        menu.add(hidechildrennode);

        //endregion

        //region rename

        Rename rename=new Rename(control,idNodeMap);
        MenuItem renam=new MenuItem("Rename",rename);
        menu.add(renam);

        //endregion

        //region AddChildKind

        this.control=control;
        dynamicItems=new LinkedList<>();

        this.idNodeMap=idNodeMap;

        //endregion

        ExpandAll expandall=new ExpandAll(idNodeMap.get(0),ANH);
        MenuItem expandal=new MenuItem("Expand All",expandall);
        menu.add(expandal);

        CollapseAll collapseall=new CollapseAll(idNodeMap.get(0),ANH);
        MenuItem collapseal=new MenuItem("Collapse All",collapseall);
        menu.add(collapseal);

        Remove remove=new Remove(control);
        MenuItem remov=new MenuItem("Remove node and children",remove);
        menu.add(remov);

    }

    private List<MenuItem> dynamicItems;

    public void draw(PNode target){
        for(MenuItem item:dynamicItems){
            menu.remove(item);
        }

        dynamicItems=new LinkedList<>();

        clear();
        addChild(menu);

        //region AddChildKind
        PiccoloCustomNode PCN= (PiccoloCustomNode) target.getParent().getParent();
        DependencyGraph DG = control.graph();
        for(scala.collection.Iterator<NodeKind> iterator = DG.nodeKinds().iterator(); iterator.hasNext();){
            NodeKind nk=iterator.next();
            if(DG.getNode(PCN.getidNode()).kind().canContain(nk)){
                AddChildKind ACK=new AddChildKind(control,nk,idNodeMap.get(0));

                MenuItem menuItem=new MenuItem("Add "+ nk.toString(),ACK);
                    dynamicItems.add(menuItem);
                    menu.add(menuItem);
                }
        }

        //endregion

        //region abstraction

        ConcreteNode node= DG.getConcreteNode(PCN.getidNode());

        Seq<Tuple2<NodeKind,AbstractionPolicy>> abstractionChoices = node.kind().abstractionChoices();

        for(scala.collection.Iterator<Tuple2<NodeKind,AbstractionPolicy>> iterator=abstractionChoices.iterator();iterator.hasNext();){
            Tuple2<NodeKind,AbstractionPolicy> tuple = iterator.next();


            AbstractionAction abstraction = new AbstractionAction(control.historyHandler().bus(),node,tuple._2(),tuple._1(),control.graph(),control.graphUtils(),control.nodeKindIcons());
            MenuItem menuItem = new MenuItem("Abstract "+node.name()+" as "+tuple._1().toString()+" "+tuple._2().toString(),abstraction);
            menu.add(menuItem);
        }

        //endregion

        menu.draw(target);
    }
    public void clear(){
        menu.setTransform(AffineTransform.getTranslateInstance(0,0));

        removeAllChildren();
    }
}
