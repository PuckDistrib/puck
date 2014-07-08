:-module(javaRules, 
	 [can_contain_kind/2,
	  abstract/4,
	  abstract_kind/2,
	  violations_node_type_priority/1,
	  type_name_to_id/3,
	  node_named_type_to_id/3,
	  struct_type/3,
      redirect_to_abstraction/5,
	  %%%% printing predicates
	  subgraph/1,
	  hook/1,
	  hooked/1,
	  node_kind_to_fill_color/2,
      real_kind_use_abs_kind/1]).

:-use_module(graph).
:-use_module(typing).

can_contain_kind(package, package).
can_contain_kind(package, class).
can_contain_kind(package, interface).
can_contain_kind(class, class).
%can_contain_kind(class, interface).
can_contain_kind(class, method).
can_contain_kind(class, constructor).
can_contain_kind(class, attribute).
can_contain_kind(interface, method). 
%can_contain_kind(method, param).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

abstract_kind(package, package). %% is it ?
abstract_kind(interface,interface).
abstract_kind(class, interface).
abstract_kind(attribute, method).
abstract_kind(method, method). %% methodImpl methodSig ??
abstract_kind(constructor, method).

abstract_method(InterfaceId, NodeId, GraphIn, GraphOut):-
    get_node(NodeId, GraphIn, Node),
    (kind_of_node(method, Node)-> abstract_node(Node, GraphIn, Abs, G2),
    id_of_node(AbsId, Abs),
    put_contains(InterfaceId, AbsId, G2, GraphOut);
    GraphOut=GraphIn).
    
%%%%%%%%%%%%%
%% (+Node, +Graph, +AbsAssoc
%% -Abs, -UseeId, -NewGraph, - NewAbsAssoc
abstract(Node, GraphIn, Abs, G3):-
    kind_of_node(Type, Node), id_of_node(NodeId, Node),
    \+abstract_kind(Type, interface),
    abstract_node(Node, GraphIn, Abs, G2),
    id_of_node(AbsId, Abs), put_uses(AbsId, NodeId, G2, G3).

abstract(Node, GraphIn, Abs2, G4):-
    kind_of_node(Type, Node), id_of_node(NodeId, Node),
    abstract_kind(Type, interface),
	    
    abstract_node(Node, GraphIn, Abs, G2),
    
    id_of_node(AbsId, Abs),  put_uses(NodeId, AbsId, G2, G3),
    
    containees_of_node(Cees, Node),
    
    foldl(call(abstract_method(AbsId)), Cees, G3, G4),
    get_node(AbsId, G4, Abs2).

%%%%%%%

real_kind_use_abs_kind(Real):-
    (kind_of_node(class, Real); 
        kind_of_node(interface, Real)).


%%%%%%% typing


type_name_to_id(_, '@primitive.void', -1).

type_name_to_id(_, '@primitive.boolean', -2).
type_name_to_id(_, '@primitive.byte', -3).
type_name_to_id(_, '@primitive.char', -4).
type_name_to_id(_, '@primitive.double', -5).
type_name_to_id(_, '@primitive.float', -6).
type_name_to_id(_, '@primitive.int', -7).
type_name_to_id(_, '@primitive.long', -8).
type_name_to_id(_, '@primitive.short', -9).

type_name_to_id(_, 'java.lang.String', -10).

% see later how to handle array and paramterized type
% String[] is added to handle main method
type_name_to_id(_, 'String[]', -11). 
type_name_to_id(_, 'int[]', -12). 

type_name_to_id(G, tuple(Names), tuple(Ids)):- maplist(call(type_name_to_id(G)), Names, Ids).
type_name_to_id(G, arrow(N1, N2), arrow(Id1, Id2)):- 
    type_name_to_id(G, N1, Id1), type_name_to_id(G, N2, Id2).
type_name_to_id(G, CoI, Id):- full_name_to_id(G, CoI, Id).


instance_type_aux(G, NodeId, Sigs, Sigs):- 
    get_node(NodeId, G, N), 
    (kind_of_node(constructor, N); kind_of_node(class, N); kind_of_node(interface, N)),!.

instance_type_aux(G, NodeId, Sigs, NewSigs):-
    get_node(NodeId, G, N), (kind_of_node(method, N); kind_of_node(attribute, N)),
    type_of_node(T,N), name_of_node(Name, N), put_assoc(Name, Sigs, (Name,T), NewSigs).


instance_template_id_to_type_struct(NodeId, G, object(Struct)):-
    get_node(NodeId, G, N), (kind_of_node(class,N); kind_of_node(interface,N)),
    containees_of_node(Cees, N), empty_assoc(Struct0),
    foldl(call(instance_type_aux(G)), Cees, Struct0, Struct).

%%%%    
struct_type(Id, Type, G):-
    gen_node(Id, G, N), kind_of_node(Kind, N),
    (((Kind=method; Kind=attribute; Kind=constructor), type_of_node(Type, N)); 
    ((Kind=class; Kind=interface), instance_template_id_to_type_struct(Id, G, Type))).

%%
to_self(X,X,self_type).
to_self(X,Y,Y):-X\=Y.    

to_self(X, tuple(Ts), tuple(Ts2)):-maplist(call(to_self(X)), Ts, Ts2).
to_self(X, arrow(Y,Z),arrow(Y2,Z2)):- to_self(X, Y, Y2), to_self(X, Z, Z2).

%%%%
%%% translate the signature using named type to signature using access graph nodes id
%%%
node_named_type_to_id(G, N, (Id, (Kind, Name, TypeId), Edges, Cts)):- 
    N=(Id, (Kind, Name, TypeName), Edges, Cts),
    (Kind=method; Kind=constructor; Kind=attribute),
    container_of_node(Cer,N),
    type_name_to_id(G, TypeName, TypeId0),
    to_self(Cer, TypeId0, TypeId),!.

node_named_type_to_id(_, N, N).

%%%%%

%%TODO : répercute le changement de type des signatures !!!
%% version sans utilisation des "sides uses"
%% redirect_to_type_abs_aux_methods(AbsId, UserId, UseeId, GraphIn, GraphOut):-
%%     (get_node(UseeId, GraphIn, Usee),
%%         kind_of_node(method, Usee)) ->
%%         contains(AbsId, AbsCeeId, GraphIn), 
%%         gen_abstraction(UseeId, GraphIn, AbsCeeId),
%%         redirect_uses(UserId, UseeId, AbsCeeId, GraphIn, GraphOut)
%%     ;GraphOut=GraphIn. %% flou -> quid des attibuts et des constructeurs ?

%% redirect_to_type_abs_aux(RealId, TypeAbsId, UserId, GraphIn, GraphOut):-
%%     get_node(RealId, GraphIn, Real),
%%     containees_of_node(RAttributesId, Real),
%%     findall(RAttributeId, 
%%         (member(RAttributeId, RAttributesId),
%%             uses(UserId, RAttributeId, GraphIn)), Usees), %% UseesList),
%%     %% list_to_set(UseesList, Usees), %%shouldn't be necessary
%%     foldl(call(redirect_to_type_abs_aux_methods(TypeAbsId, UserId)), Usees, GraphIn, G1),
%%     %%via recursive call, User may use only a containee of Real and not real itself
%%     (uses(UserId, RealId, G1) ->
%%         redirect_uses(UserId, RealId, TypeAbsId, G1, G2);G1=G2),
    
%%     get_node(UserId, G2, User), 
%%     ((kind_of_node(class, User); kind_of_node(interface, User))->
%%     %% if we redirect from a class, we also have to redirect for all the methods:
%%     %% if use in an extends context, replace super by call to delegate
%%     %% if it forbidden for a class to use another, it also forbidden for its method:
%%     %% we have to replace the optionnal class declaration
%%         containees_of_node(AttributesIds, User),
%%         redirect_to_type_abs(AttributesIds, RealId, TypeAbsId, G2, GraphOut);
%%         GraphOut=G2).

redirect_side_use(PUse, TypeAbs, (SUserId, SUseeId), GraphIn, GraphOut):-
    containees_of_node(TypeAbsCeesId, TypeAbs),
    member(TAMethodId, TypeAbsCeesId), 
    gen_abstraction(SUseeId, GraphIn, TAMethodId),
    
    get_primary_uses((SUserId, SUseeId), GraphIn, PUses),
    (PUses=[]-> fail;
        
        get_node(SUseeId, GraphIn, SUsee),
        (select(PUse, PUses, [])-> 
            del_primary_uses(SUsee, SUserId, NSUsee),
            redirect_uses(SUserId, SUseeId, TAMethodId, GraphIn, G2);

            select(PUse, PUses, NPUses),
            set_primary_uses(SUsee, NPUses, NSUsee),
            put_uses(SUserId, TAMethodId, GraphIn, G2)),

            put_node(NSUsee, G2, G3),
            get_node(TAMethodId, G3, TAMethod),
            add_primary_uses(TAMethod, PUse, SUserId, NTAMethod),
            put_node(NTAMethod, G3, GraphOut)).


redirect_to_type_abs_aux(RealId, TypeAbsId, UserId, GraphIn, GraphOut):-
    get_side_uses((UserId, RealId), GraphIn, SideUses),
    get_node(TypeAbsId, GraphIn, TypeAbs),
    foldl(call(redirect_side_use((UserId, RealId), TypeAbs)), SideUses, GraphIn, G1),
    %%via recursive call, User may use only a containee of Real and not real itself
    redirect_uses(UserId, RealId, TypeAbsId, G1, G2),
    get_node(RealId, G2, Real),
    del_side_uses(Real, UserId, NReal),
    put_node(NReal, G2, GraphOut).

redirect_to_type_abs(UserIds, RealId, AbsId, GraphIn, GraphOut):-
    foldl(call(redirect_to_type_abs_aux(RealId, AbsId)),UserIds, GraphIn, GraphOut). %%G1),

%%
redirect_to_method_abs_aux(RealId, AbsId, UserId, GraphIn, GraphOut):-
        redirect_uses(UserId, RealId, AbsId, GraphIn, GraphOut).

redirect_to_method_abs(UserIds, RealId, AbsId, GraphIn, GraphOut):-
        %% au moins ajoute uses vers container de Abs ! (éventuellement redirect)
        foldl(call(redirect_to_method_abs_aux(RealId, AbsId)), UserIds, GraphIn, GraphOut).

%%
redirect_to_abstraction(UserIds, RealId, AbsId, GraphIn, GraphOut):-
    get_node(AbsId, GraphIn, Abs),
    ((kind_of_node(class, Abs); kind_of_node(interface, Abs)), 
        redirect_to_type_abs(UserIds, RealId, AbsId, GraphIn, GraphOut);
     kind_of_node(method, Abs),
        redirect_to_method_abs(UserIds, RealId, AbsId, GraphIn, GraphOut)).

    
%%%%% solver strategy

violations_node_type_priority([attribute, class, interface]).

%%%% printing predicates

subgraph(package).

hook(interface).
hook(class).

hooked(method).
hooked(attribute).
hooked(constructor).

node_kind_to_fill_color(virtualScope,'#33FF33').%Green
node_kind_to_fill_color(package,'#FF9933').%Orange
node_kind_to_fill_color(interface,'#FFFF99').%Light yellow
node_kind_to_fill_color(class,'#FFFF33').%Yellow

node_kind_to_fill_color(constructor,'#FFFF33').%yellow
node_kind_to_fill_color(method,'#FFFFFF').%White
node_kind_to_fill_color(attribute,'#FFFFFF').%White
node_kind_to_fill_color(stringLiteral,'#CCFFCC').%Very light green


