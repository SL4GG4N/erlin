

%% The database is an ordered unbalanced tree of
%% {Key,Value,Left,Right} | empty.

-module(db).
-author("Scalman").
-export([new/0,destroy/1,write/3,match/2,delete/2,keys/1,read/2]).
-record (node, {k1 = empty,v1 = empty,left = empty,right = empty}).
%% Create a new database.


%% Create new DB
new() -> 
    empty.

%% Destroy the database.

destroy(_Db) -> ok.

write(Key, Val, Node) when Key < Node#node.k1 ->
	Node#node{left=write(Key, Val, Node#node.left)};
write(Key, Val, Node) when Key > Node#node.k1 ->
	Node#node{right=write(Key, Val, Node#node.right)};
write(Key,Val,#node{k1 = Key} = Node) ->
	#node{k1 = Key,v1 = Val,left = Node#node.left, right = Node#node.right};
write(Key,Val,empty)->
	#node{k1 = Key,v1 = Val,left = empty,right = empty}.


match(_,empty)->
	[];
match(Val,#node{v1 = Val} = Node) ->
	[Node#node.k1] ++ match(Val,Node#node.left) 
	++ match(Val,Node#node.right);
match(Val,Node) ->
	match(Val,Node#node.left)
	++ match(Val,Node#node.right).


delete(_,empty)->
	empty;
delete(Key,Node) when Key < Node#node.k1 ->
	Node#node{left = delete(Key,Node#node.left)};
delete(Key,Node) when Key > Node#node.k1 ->
	Node#node{right = delete(Key,Node#node.right)};
delete(_,Node)->
	merge(Node#node.left,Node#node.right).


merge(empty, Right) -> Right;
merge(Left, empty) -> Left;
merge(Left, Right) ->
     Left#node{right = merge(Left#node.right, Right)}.


keys(empty)->
	[];
keys(Node) ->
	[Node#node.k1] ++ keys(Node#node.left) ++ keys(Node#node.right).

	
read(Key,Node) when Node#node.k1 > Key ->
	read(Key,Node#node.left);
read(Key,Node) when Node#node.k1 < Key ->
	read(Key,Node#node.right);
read(_,empty)->
	{error,instance};
read(_,Node)->
	{ok,Node#node.v1}.


