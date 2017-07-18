package puck.piccolo2.LayoutStack;

import java.util.Deque;
import java.util.LinkedList;

/**
 * Created by Vincent Hudry on 26/06/2017.
 */
public class LayoutStack {

    private Deque<LayoutState> states;

    public LayoutStack(){
        states=new LinkedList<>();
    }

    public void push(LayoutState layoutState){
        states.push(layoutState);
    }

    public LayoutState peek(){
        return states.peek();
    }

    public LayoutState pop(LayoutState layoutState){
        return states.pop();
    }

}
