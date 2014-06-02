/*
    The contents of this file are subject to the Mozilla Public License
    Version  1.1  (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at:

    http://www.mozilla.org/MPL/

    Software  distributed  under  the License is distributed on an "AS
    IS"  basis,  WITHOUT  WARRANTY  OF  ANY  KIND,  either  express or
    implied.  See  the  License  for  the  specific language governing
    rights and limitations under the License.
    The Original Code is the contents of this file.
    The  Initial  Developer  of  the  Original  Code is SICS, Swedish
    Institute of Computer Science AB (SICS).
    Portions  created  by the Initial Developer are Copyright (C) 2007
    of the Initial Developer. All Rights Reserved.
    Contributor(s):
    _____Mats Carlsson <matsc@sics.se>
    _____Nicolas Beldiceanu <Nicolas.Beldiceanu@emn.fr>

    Alternatively, if the contents of this file is included as a part of
    SICStus Prolog distribution by SICS, it may be used under the terms of
    an appropriate SICStus Prolog License Agreement (the "SICStus Prolog
    License"), in which case the provisions of the SICStus Prolog License
    are applicable instead of those above.
*/
:-module(schema_dot, [top/0, top/1]).

:- use_module(library(lists)).
:- use_module(library(system3), [system/1]).
:- use_module(library(xml)).

top:-
	top('ctrs/schema.xsd').

top(File):-
	xml_parse_file(File,Doc),
	process(Doc).



/*
interface to xml_parse/2 when using strings
*/
xml_parse_file(File, Document) :-
	open(File, read, Stream),
	read_lines(Stream, Lines, []),
	close(Stream),
	append(Lines, Chars),
	xml_parse(Chars, Document).

read_lines(Stream) -->
	{read_line(Stream, L)},
	read_lines(Stream, L).

read_lines(_, end_of_file) --> !.
read_lines(Stream, L) --> [L],
	read_lines(Stream).

/*
process an xml parse tree and create a list of constraints
process(++,-)
*/
process(xml(_Attr,[Child])):- % top-level of parse tree
	!,
	process(Child).
process(namespace(_Http,_Ab,Element)):- % top-level of parse tree
	!,
	process(Element).
process(element(schema,_Attr,[Child|_])):- 
	!,
	process(Child).
process(element(element,Attr,Children)):- 
	memberchk(name=Codes,Attr),
	atom_codes(String,Codes),
	!,
	(String = 'constraints' ->
		Children = [element(_,_,[element(_,_,L)])],
	        foreach(L, constraint)
	;
		process_children(Children)
	).
process(element(_X,_Attr,Children)):- 
	!,
	process_children(Children).
process(X):-
	portray_clause(X).

process_children(Children):-
	foreach(Children, process).	

constraint(element(element,Attr,Def)):-
	memberchk(name=Codes,Attr),
	atom_codes(String,Codes),
	tree_list(Def,List,2,_),
	Tree = tree(node(cons,1,String,range(1,1)),List),
	Dir = 'images',
	concat_string([Dir,'/',String,'.dot'],File),
	open(File,write,SS),
	preamble(SS),
	nodes(SS,Tree),
	edges(SS,Tree),
	postfix(SS),
	close(SS),
	concat_string(['dot -Tpng -o ',Dir,'/',String,'.png ','-Tps -o ',Dir,'/',String,'.eps ',Dir,'/',String,'.dot'],Cmd),
	portray_clause(Cmd),
	system(Cmd),
	fail.
constraint(_).


preamble(SS):-
	format(SS,'digraph G {\nrankdir=LR;\n',[]).

postfix(SS):-
	format(SS,'}',[]).

tree_list([], [], N, N).
tree_list([X|L], [Y|K], N, Nend) :-
	tree(X, Y, N, N1),
	tree_list(L, K, N1, Nend).

tree(element(Builtin,Attr,Def),tree(node(Type,N,Type,Range),List),N,Nend):-
	match_builtin(Builtin,Type),
	!,
	range(Attr,Range),
	N1 is N+1,
	tree_list(Def,List,N1,Nend).
tree(element(Builtin,Attr,Def),tree(node(attr,N,Label,Range),List),N,Nend):-
	memberchk(Builtin,[attribute]),
	!,
	attr_range(Attr,Range),
	memberchk(name=Codes,Attr),
	atom_codes(String,Codes),
	memberchk(type=Codes1,Attr),
	atom_codes(String1,Codes1),
	concat_string([String,': ',String1],Label),
	N1 is N+1,
	tree_list(Def,List,N1,Nend).
tree(element(element,Attr,Def),tree(node(Type,N,Atom,Range),List),N,Nend):-
	range(Attr,Range),
	memberchk(name=Codes,Attr),
	atom_codes(Atom,Codes),
	match_builtin(Atom,Type),
	!,
	N1 is N+1,
	tree_list(Def,List,N1,Nend).
tree(element(element,Attr,Def),tree(node(ele,N,Atom,Range),List),N,Nend):-
	!,
	range(Attr,Range),
	memberchk(name=Codes,Attr),
	atom_codes(Atom,Codes),
	N1 is N+1,
	tree_list(Def,List,N1,Nend).
tree(Other,x,N,N):-
	raise_exception(other(Other)).

attr_range(Attr,Range):-
	(memberchk(use=Codes,Attr) ->
		atom_codes(Range,Codes)
	;
		Range = 'required'
	).

range(Attr,range(Min,Max)):-
	(memberchk(minOccurs=Codes,Attr) ->
		my_number_codes(Min,Codes)
	;
		Min = 1
	),
	(memberchk(maxOccurs=Codes1,Attr) ->
		(my_number_codes(Max,Codes1) -> true ; Max = unbounded)
	;
		Max = 1
	).

my_number_codes(unbounded, "unbounded") :- !.
my_number_codes(N, C) :-
	number_codes(N, C).

range_style(range(1,1),Styles,Styles):-
	!.
range_style('required',Styles,Styles):-
	!.
range_style('optional',Styles,[color=lightgray|Styles]):-
	!.
range_style(range(0,1),Styles,[color=lightgray|Styles]):-
	!.
range_style(range(0,unbounded),Styles,[color=lightgray,fontsize=24|Styles]):-
	!.
range_style(range(1,unbounded),Styles,[fontsize=24|Styles]):-
	!.
range_style(range(Min,Max),Styles,Styles):-
	portray_clause(range(Min,Max)).

nodes(SS,tree(node(Type,N,Label,Range),Children)):-
	type_shape(Type,Styles),
	range_style(Range,Styles,AllStyles),
	format(SS,'n~d [label=\"~w\" ',[N,Label]),
	foreach(AllStyles, nodes1(SS)),
	format(SS,'];\n',[]),
	foreach(Children, nodes(SS)).

nodes1(SS, A=Value) :-
	format(SS,'~w=\"~w\" ',[A,Value]).

type_shape(collection,[shape=box,style=filled,fillcolor=cornflowerblue]).
type_shape(item,[shape=box,style=filled,fillcolor=skyblue]).
type_shape(variableref,[shape=box,style=filled,fillcolor=lightskyblue]).
type_shape(integer,[shape=box,style=filled,fillcolor=lightblue]).
type_shape(atom,[shape=box,style=filled,fillcolor=lightblue]).
type_shape(integerset,[shape=box,style=filled,fillcolor=lightslateblue]).
type_shape(ele,[shape=box,style=filled,fillcolor=ivory]).
type_shape(attr,[shape=ellipse,style=filled,fillcolor=palegreen]).
type_shape(seq,[shape=hexagon]).
type_shape(choice,[shape=diamond]).
type_shape(cT,[shape=circle]).
type_shape(cons,[shape=octagon,style=filled,fillcolor=wheat]).

match_builtin(sequence,seq).
match_builtin(choice,choice).
match_builtin(complexType,cT).
match_builtin(collection,collection).
match_builtin(item,item).
match_builtin(variableref,variableref).
match_builtin(integer,integer).
match_builtin(atom,atom).
match_builtin(integerset,integerset).

edges(SS,tree(node(_,N,_,_),Children)):-
	foreach(Children, edges1(SS,N)),
	foreach(Children, edges(SS)).

edges1(SS, N, tree(node(_,N1,_,_),_)) :-
	format(SS,'n~d -> n~d;\n',[N,N1]).

% begin Eclipse/SP3 support

concat_string(Atoms, Atom) :-
	foreach(Atoms, Codess, atom_codes),
	append(Codess, Codes),
	atom_codes(Atom, Codes).

foreach([], _).
foreach([X|L], Goal) :-
	call(Goal, X),
	foreach(L, Goal).

foreach([], [], _).
foreach([X|L], [Y|M], Goal) :-
	call(Goal, X, Y),
	foreach(L, M, Goal).

	
% end Eclipse/SP3 support
