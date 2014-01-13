node('root', 'root').

node('root.scope_a', 'scope_a').
node('root.scope_b', 'scope_b').

node('root.scope_a.sub_scope_a0', 'sub_scope_a0').
node('root.scope_a.sub_scope_a1', 'sub_scope_a1').

node('root.scope_b.interloper', 'interloper').
node('root.scope_b.friend', 'friend').


contains('root', 'root.scope_a').
contains('root.scope_a', 'root.scope_a.sub_scope_a0').
contains('root.scope_a', 'root.scope_a.sub_scope_a1').

contains('root', 'root.scope_b').
contains('root.scope_b', 'root.scope_b.interloper').
contains('root.scope_b', 'root.scope_b.friend').

uses('root.scope_b.interloper', 'root.scope_a.sub_scope_a0').
uses('root.scope_b.friend', 'root.scope_a.sub_scope_a0').

%% constraints

hideFrom('root.scope_a', 'root.scope_b').

%hideFrom('root.scope_a.sub_scope_a0', 'root.scope_b').
%hideFrom('root.scope_a.sub_scope_a0', 'root.scope_b.interloper').

isFriendOf('root.scope_b.friend', 'root.scope_a').

