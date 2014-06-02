%%% -*- Mode: Prolog; Module: schema; -*-
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

:-module(schema,
	 [% called from pl2latex
	  get_arguments/1,
	  schema_file/4,
	  % callable by the user
	  all/0,
	  schema/0,
	  top/0,
	  top/2,
	  top/3,
	  xml_translate/0,
	  xml_translate/2,
	  xml_translate/3,
	  xml_translate_file/0,
	  xml_translate_file/2,
	  xml_translate_file/3]).

% duplicated when called from pl2latex
:- op(1200, xfx, --->).
:- op(760,  yfx, #<=>).
:- op(750,  xfy, #=>).
:- op(750,  yfx, #<=).
:- op(740,  yfx, #\/).
:- op(730,  yfx, #\).
:- op(720,  yfx, #/\).
:- op(710,   fy, #\).
:- op(700,  xfx, [in,in_set]).
:- op(700,  xfx, [#=,#\=,#<,#=<,#>,#>=]).
:- op(550,  xfx, ..). %% higher than +,-,*...
% :- op(200,  xfy, ^). % predef
:- op(180,  xfy, @).

:- dynamic(ctr_arguments/2).
:- dynamic(simple_type/3).

/* "Eclipse compatibility code" */

:- use_module(library(file_systems)).
:- use_module(library(terms), [
	term_variables/2
	]).
:- use_module(library(lists), [
	append/2,
	keys_and_values/3
	]).
:- use_module(library(codesio), [
	with_output_to_codes/4
	]).

getval(K,S) :- bb_get(K,S).

setval(K,S) :- bb_put(K,S).

incval(K)   :- bb_get(K,V1), V2 is V1+1, bb_put(K,V2).

writeln(X) :- write(X), nl.

writeln(S,X) :- write(S,X), nl(S).

read_directory(Dir, Pat, _, Relatives) :-
	file_members_of_directory(Dir, Pat, Pairs),
	keys_and_values(Pairs, Relatives, _).
	
indent(S,N) :-
	format(S, '~*c', [N,0' ]).

concat_string(List,String) :-
	atomics_codes(List, Codess),
	append(Codess, Codes),
	atom_codes(String, Codes).

atomics_codes([], []).
atomics_codes([X|Xs], [S|Ss]) :-
	number(X), !, 
	number_codes(X, S),
	atomics_codes(Xs, Ss).
atomics_codes([X|Xs], [S|Ss]) :-
	atom(X),
	atom_codes(X, S),
	atomics_codes(Xs, Ss).

foreach([], _).
foreach([X|Xs], Goal) :-
        call(Goal, X),
	foreach(Xs, Goal).

foreacharg(Term, Goal) :-
	functor(Term, _, N),
	foreacharg(0, N, Term, Goal).

foreacharg(N, N, _, _) :- !.
foreacharg(I, N, Term, Goal) :-
	J is I+1,
	arg(J, Term, X),
	call(Goal, X),
	foreacharg(J, N, Term, Goal).

foreach_trace([], _, _).
foreach_trace([X|Xs], I, Goal) :-
	J is I+1,
        format(user, '~w-~w\n', [J,X]),
        call(Goal, X),
	foreach_trace(Xs, J, Goal).

/*

top level setup

*/

schema:-
        SrcDir='src',
        OutFile='ctrs/schema.xsd',
        Options = [schema,not(constraint)],
        schema(SrcDir,OutFile,Options).

        
schema(SrcDir,OutFile,Options):-
	valid_option(schema,Options),
        !,
	get_arguments('src'),
        reset_counters,
        read_directory(SrcDir,'*.pl',_L,K),
        sort(K,Sorted),
        open(OutFile,write,SS),
        schema_header(SS,Options),
	foreach_trace(Sorted, 0, schema_pattern(SS,SrcDir,Options)),
        schema_postfix(SS),
        close(SS).
schema(_SrcDir,_OutFile,_Options).

schema_pattern(SS,SrcDir,Options,Name):-
	read_prolog_file(SrcDir,Name,L),
        treat_schema(SS,Name,L,9,Options).

xml_translate_file:-
        xml_translate_file('queen.out','queen.xml'),
        xml_translate_file('square.out','square.xml').

xml_translate_file(InFile,OutFile):-
	xml_translate_file(InFile,OutFile,[schema,not(constraint)]).

xml_translate_file(InFile,OutFile,Options):-
        open(InFile,read,S),
        read(S,Clause),
        close(S),
        xml_translate(Clause,OutFile,Options).
	
xml_translate:-
        Clause = (query([A,B,C,D,E,F,G,H]) :-
		     in_interval(A, 1, 8),
		     in_interval(B, 1, 8),
		     in_interval(C, 1, 8),
		     in_interval(D, 1, 8),
		     in_interval(E, 1, 8),
		     in_interval(F, 1, 8),
		     in_interval(G, 1, 8),
		     in_interval(H, 1, 8),
		     alldifferent([[var-A],[var-B],[var-C],[var-D],
				   [var-E],[var-F],[var-G],[var-H]]),
		     alldifferent_cst([[var-A,cst-0],[var-B,cst-1],
				       [var-C,cst-2],[var-D,cst-3],
				       [var-E,cst-4],[var-F,cst-5],
				       [var-G,cst-6],[var-H,cst-7]]),
		     alldifferent_cst([[var-A,cst-7],[var-B,cst-6],
				       [var-C,cst-5],[var-D,cst-4],
				       [var-E,cst-3],[var-F,cst-2],
				       [var-G,cst-1],[var-H,cst-0]])),
        xml_translate(Clause,'queen.xml').

xml_translate(Clause,OutFile):-
	xml_translate(Clause,OutFile,[schema,not(constraint)]).

xml_translate(Clause,OutFile,Options):-
	get_arguments('src'),
        reset_counters,
        Clause = (Head :- Body),
        open(OutFile,write,SS),
        header(SS,Options),
        arg(1,Head,Externals),
        term_variables(Clause,Vars),
        variables(SS,Vars,Externals,0,Options),
        format(SS,'<constraints>\n',[]),
        treat_example(SS,Body,1,Options),
        format(SS,'</constraints>\n',[]),
        postfix(SS),
        close(SS).
        
all:-
        SrcDir='src',
        OutFile='all.xml',
        Options = [schema,not(constraint)],
        all(SrcDir,OutFile,Options).

        
all(SrcDir,OutFile,Options):-
	get_arguments('src'),
        reset_counters,
        read_directory(SrcDir,'*.pl',_L,K),
        sort(K,Sorted),
        open(OutFile,write,SS),
        header(SS,Options),
        format(SS,'<constraints>\n',[]),
	foreach_trace(Sorted, 0, schema_constraint(SS,SrcDir,Options)),
        format(SS,'</constraints>\n',[]),
        postfix(SS),
        close(SS).

schema_constraint(SS,SrcDir,Options,Name):-
        concat_string([SrcDir,'/',Name],File),
        open(File,read,S),
        read(S,X),
        read_lp(S,X,L),
        treat_ctr(SS,Name,L,1,Options),
        close(S).

top:-
        SrcDir='src',
        OutDir='ctrs',
        top(SrcDir,OutDir).
        
top(SrcDir,OutDir):-
	top(SrcDir,OutDir,[schema,not(constraint)]).

top(SrcDir,OutDir,Options):-
	get_arguments('src'),
        read_directory(SrcDir,'*.pl',_L,K),
        sort(K,Sorted),
	foreach_trace(Sorted, 0, schema_file(SrcDir,OutDir,Options)).

schema_file(SrcDir,OutDir,Options,Name):-
        reset_counters,
        concat_string([SrcDir,'/',Name],File),
        open(File,read,S),
        read(S,X),
        read_lp(S,X,L),
	atom_concat(Root, '.pl', Name),
        concat_string([OutDir,'/',Root,'.xml'],OutFile),
        open(OutFile,write,SS),
        header(SS,Options),
        format(SS,'<constraints>\n',[]),
        treat_ctr(SS,Name,L,1,Options),
        format(SS,'</constraints>\n',[]),
        postfix(SS),
        close(SS),
        close(S).

/*

processing

*/

/*

schema processing

*/

treat_schema(SS,_Name,L,Indent,Options):-
        try_ctr(ctr_arguments(Type,Args),L),
        try_ctr(ctr_restrictions(Type,Restrictions),L),
        try_ctr(ctr_types(Type,Types),L),
        treat_constraint(SS,Type,Args,Types,Restrictions,Indent,Options).


treat_constraint(SS,Type,Args,Types,Restrictions,Indent,Options):-
        Indent3 is Indent+3,
	Indent2 is Indent+2,
        schema_head(SS,Type,Indent,Options),
        treat_args(SS,Args,Types,Restrictions,Indent3),
	sequence(SS,Indent2,'/'),
	schema_attribute(SS,Indent2,'id','xs:ID','required'),
	schema_attribute(SS,Indent2,'name','xs:string','optional'),
	schema_attribute(SS,Indent2,'desc','xs:string','optional'),
        schema_tail(SS,Type,Indent,Options).

treat_args(_,[],_,_,_).
treat_args(SS,[H|T],Types,Restrictions,Indent):-
        treat_arg(SS,H,Types,Restrictions,Indent),
        treat_args(SS,T,Types,Restrictions,Indent).

treat_arg(SS,Name-Type,Types,Restrictions,Indent):-
        Indent1 is Indent+1,
	Indent2 is Indent+2,
	element(SS,Indent,Name),
	complex_type(SS,Indent1,''),
        treat_type(SS,-,Name,Type,Types,Restrictions,Indent2),
	complex_type(SS,Indent1,'/'),
	end_element(SS,Indent).

treat_attribute(SS,Collection,Types,Restrictions,Indent,Name-Type):-
        Indent1 is Indent+1,
	Indent2 is Indent+2,
        required_attr(Collection,Name,Restrictions,Required),
	element(SS,Indent,Name,Required,1),
	complex_type(SS,Indent1,''),
        treat_type(SS,Collection,Name,Type,Types,Restrictions,Indent2),
	complex_type(SS,Indent1,'/'),
	end_element(SS,Indent).

treat_type(SS,Collection,Name,Basic,_,Restrictions,Indent):-
        memberchk(Basic,[int]),
        !,
	sequence(SS,Indent,''),
        integer(SS,Indent,Collection,Name,Basic,Restrictions),
	sequence(SS,Indent,'/').
treat_type(SS,Collection,Name,Basic,_,Restrictions,Indent):-
        memberchk(Basic,[atom]),
        !,
	sequence(SS,Indent,''),
        atom(SS,Indent,Collection,Name,Basic,Restrictions),
	sequence(SS,Indent,'/').
treat_type(SS,_Collection,_Name,Basic,_,_Restrictions,Indent):-
        memberchk(Basic,[sint]),
        !,
	sequence(SS,Indent,''),
        integer_set(SS,Indent),
	sequence(SS,Indent,'/').
treat_type(SS,Collection,Name,Basic,_,Restrictions,Indent):-
        memberchk(Basic,[dvar]),
        !,
	choice(SS,Indent,''),
        integer(SS,Indent,Collection,Name,Basic,Restrictions),
        variable_ref(SS,Indent),
	choice(SS,Indent,'/').
treat_type(SS,_Collection,_Name,Basic,_,_Restrictions,Indent):-
        memberchk(Basic,[svar]),
        !,
	choice(SS,Indent,''),
        integer_set(SS,Indent),
        variable_ref(SS,Indent),
	choice(SS,Indent,'/').
treat_type(SS,_,Name,Term,Types,Restrictions,Indent):-
        compound(Term),
        functor(Term,collection,_N),
        !,
	Indent1 is Indent+1,
	Indent2 is Indent+2,
	Indent3 is Indent+3,
	Indent4 is Indent+4,
	Indent5 is Indent+5,
	Indent6 is Indent+6,
	Indent7 is Indent+7,
	sequence(SS,Indent,''),
	element(SS,Indent1,collection),
	complex_type(SS,Indent2,''),
	sequence(SS,Indent3,''),
	element(SS,Indent4,item,0,unbounded),
	complex_type(SS,Indent5,''),
	sequence(SS,Indent6,''),
	foreacharg(Term, treat_attribute(SS,Name,Types,Restrictions,Indent7)),
	sequence(SS,Indent6,'/'),
	complex_type(SS,Indent5,'/'),
	end_element(SS,Indent4),	
	sequence(SS,Indent3,'/'),
	complex_type(SS,Indent2,'/'),
	end_element(SS,Indent1),
	sequence(SS,Indent,'/').
treat_type(SS,_,Name,Type,Types,Restrictions,Indent):-
        memberchk(Type-Def,Types),
        !,
        treat_type(SS,Name,Type,Def,Types,Restrictions,Indent).
treat_type(_,Coll,Name,X,_,_,_):-
        raise_exception(unknown_treat(Coll,Name,X)).

/*

example data processing

*/


treat_ctr(SS,_Name,L,Indent,Options):-
        try_ctr(ctr_example(_Type,Example),L),
        treat_example(SS,Example,Indent,Options).

treat_example(_SS,[],_Indent,_):-
        !.
treat_example(SS,[H|T],Indent,Options):-
        !,
        treat_example(SS,H,Indent,Options),
        treat_example(SS,T,Indent,Options).
treat_example(SS,','(H,T),Indent,Options):-
        !,
	functor(H,Constraint,_),
	writeln(constraint(Constraint)),
        treat_example(SS,H,Indent,Options),
	writeln(treated),
        treat_example(SS,T,Indent,Options).
treat_example(SS,Term,Indent,Options):-
        compound(Term),
        functor(Term,Type,_),
	once(ctr_arguments(Type,Args)),
        new_id(Id),
        new_name(Name),
        constraint_head(SS,Id,Name,Type,'',Indent,Options),
        Indent1 is Indent+1,
	treat_example_args(Args, 0, Term, SS, Indent1),
        constraint_tail(SS,Type,Indent,Options).

treat_example_args([], _, _, _, _).
treat_example_args([Name-_|Args], I, Term, SS, Indent1) :-
	J is I+1,
	arg(J, Term, X),
	indent(SS,Indent1),
	format(SS,'<~w>\n',[Name]),
	Indent2 is Indent1+1,
	treat_values(SS,X,Indent2),
	indent(SS,Indent1),
	format(SS,'</~w>\n',[Name]),
	treat_example_args(Args, J, Term, SS, Indent1).

treat_values(_SS,X,_Indent):-
        var(X),
        !,
        raise_exception(unexpected_var(X)),
        abort.
treat_values(SS,'$VAR'(N),Indent):-
        !,
        concat_string(['V',N],Ref),
        indent(SS,Indent),
        format(SS,'<variableref id=\'~w\'/>\n',[Ref]).
treat_values(SS,X,Indent):-
        integer(X),
        !,
        indent(SS,Indent),
        format(SS,'<integer value=\'~w\'/>\n',[X]).
treat_values(SS,[H|T],Indent):-
        !,
        indent(SS,Indent),
        format(SS,'<collection>\n',[]),
        Indent1 is Indent+1,
        treat_items(SS,[H|T],Indent1),
        indent(SS,Indent),
        format(SS,'</collection>\n',[]).
treat_values(SS,[],Indent):-
        !,
        indent(SS,Indent),
        format(SS,'<collection/>\n',[]).
treat_values(SS,{},Indent):-
        !,
        indent(SS,Indent),
        format(SS,'<integerset/>\n',[]).
treat_values(SS,X,Indent):-
        atom(X),
        !,
        indent(SS,Indent),
        map_atom(X,Y),
        format(SS,'<atom value=\'~w\'/>\n',[Y]).
treat_values(SS,{A},Indent):-
        !,
        indent(SS,Indent),
        format(SS,'<integerset>\n',[]),
        Indent1 is Indent+1,
        treat_integer_list(SS,A,Indent1),
        indent(SS,Indent),
        format(SS,'</integerset>\n',[]).
treat_values(_SS,X,_Indent):-
        raise_exception(unknown_values(X)).
        
treat_items(_SS,[],_Indent) :- !.
treat_items(SS,[H|T],Indent):-
        treat_item(SS,H,Indent),
        treat_items(SS,T,Indent).

treat_item(SS,[H|T],Indent):-
        indent(SS,Indent),
        format(SS,'<item>\n',[]),
        Indent1 is Indent+1,
        treat_attr_values(SS,[H|T],Indent1),
        indent(SS,Indent),
        format(SS,'</item>\n',[]).

treat_attr_values(_SS,[],_Indent) :- !.
treat_attr_values(SS,[H|T],Indent):-
        treat_attr_value(SS,H,Indent),
        treat_attr_values(SS,T,Indent).

treat_attr_value(SS,Name-Value,Indent):-
        !,
        indent(SS,Indent),
        format(SS,'<~w>\n',[Name]),
        Indent1 is Indent+1,
        treat_values(SS,Value,Indent1),
        indent(SS,Indent),
        format(SS,'</~w>\n',[Name]).

treat_integer_list(SS,A,Indent):-
        integer(A),
        !,
        treat_values(SS,A,Indent).
treat_integer_list(SS,(A,B),Indent):-
        treat_integer_list(SS,A,Indent),
        treat_integer_list(SS,B,Indent).


/*

variables

*/
variables(SS,Vars,Externals,Indent,Options):-
        numbervars(Vars,1,_),
        indent(SS,Indent),
        format(SS,'<variables>\n',[]),
        Indent1 is Indent+1,
	foreach(Vars, variable(SS,Externals,Indent1,Options)),
        indent(SS,Indent),
        format(SS,'</variables>\n',[]).

variable(SS,Externals,Indent,_Options,'$VAR'(N)):-
        concat_string(['V',N],Id),
        concat_string([var,N],Name),
        (memberchk('$VAR'(N),Externals) ->
            External = yes
        ;
            External = no
        ),
        indent(SS,Indent),
        format(SS,'<variable id=\'~w\' name=\'~w\' external=\'~w\'/>\n',
               [Id,Name,External]).


/*

utility

*/

try_ctr(X,L):-
        memberchk(X,L),
        !.
try_ctr(Term,_):-
        arg(2,Term,[]).

new_id(String):-
        incval(id),
        getval(id,N),
        concat_string([c,N],String).

new_name(String):-
        incval(name),
        getval(name,N),
        concat_string([name,N],String).

        

map_atom('=','EQ'):-
        !.
map_atom('<','LT'):-
        !.
map_atom('>','GT'):-
        !.
map_atom('=\\=','NEQ'):-
        !.
map_atom('>=','GE'):-
        !.
map_atom('=<','LE'):-
        !.
map_atom(X,unknown):-
        raise_exception(unknown_atom(X)).


valid_option(X,L):-
        memberchk(X,L).

header(SS,Options):-
        valid_option(dtd,Options),
        !,
        format(SS,'<?xml version=\'1.0\' encoding=\'UTF-8\' ?>\n',[]),
        format(SS,'<!DOCTYPE model SYSTEM \'model.dtd\'>\n',[]),
        format(SS,'<model>\n',[]).
header(SS,Options):-
        valid_option(schema,Options),
        !,
        format(SS,'<?xml version=\'1.0\' encoding=\'UTF-8\' ?>\n',[]),
        format(SS,'<model xsi:noNamespaceSchemaLocation=\'schema.xsd\' xmlns:xsi=\'http://www.w3.org/2001/XMLSchema-instance\'>\n',[]).

postfix(SS):-
        format(SS,'</model>\n',[]).


schema_header(SS,_Options):-
	Indent is 0,
	Indent1 is Indent+1, 
	Indent2 is Indent+2, 
	Indent3 is Indent+3, 
	Indent4 is Indent+4, 
	Indent5 is Indent+5, 
	Indent6 is Indent+6, 
	Indent7 is Indent+7, 
	Indent8 is Indent+8, 
        format(SS,'<?xml version=\'1.0\' encoding=\'UTF-8\' ?>\n',[]),
	format(SS,'<xs:schema xmlns:xs=\'http://www.w3.org/2001/XMLSchema\'>\n',[]),
	element(SS,Indent,model),
	complex_type(SS,Indent1,''),
	sequence(SS,Indent2,''),
	element(SS,Indent3,variables,0,1),
	complex_type(SS,Indent4,''),
	sequence(SS,Indent5,''),
	element(SS,Indent6,variable,1,unbounded),
	complex_type(SS,Indent7,''),
	schema_attribute(SS,Indent8,'id','xs:ID',required),
	schema_attribute(SS,Indent8,'name','xs:string',optional),
	schema_attribute(SS,Indent8,'external','xs:string',required),
	complex_type(SS,Indent7,'/'),
	end_element(SS,Indent6),
	sequence(SS,Indent5,'/'),
	complex_type(SS,Indent4,'/'),
	end_element(SS,Indent3),
	element(SS,Indent3,constraints,1,1),
	complex_type(SS,Indent4,''),
	choice(SS,Indent5,'',1,unbounded).

	

schema_postfix(SS):-
	Indent is 0,
	Indent1 is Indent+1, 
	Indent2 is Indent+2, 
	Indent3 is Indent+3, 
	Indent4 is Indent+4, 
	Indent5 is Indent+5, 
	choice(SS,Indent5,'/'),
	complex_type(SS,Indent4,'/'),
	end_element(SS,Indent3),
	sequence(SS,Indent2,'/'),
	complex_type(SS,Indent1,'/'),
	end_element(SS,Indent),
	schema_types(SS),
        format(SS,'</xs:schema>\n',[]).

schema_types(SS):-
	findall(String,simple_type(_,_,String),L),
	foreach(L, writeln(SS)).

get_arguments(Dir):-
	retractall(ctr_arguments(_,_)),
	retractall(simple_type(_,_,_)),
	read_directory(Dir,'*.pl',_,K),
	foreach(K, get_arguments1(Dir)).

get_arguments1(Dir, X) :-
	   read_prolog_file(Dir,X,L),
           memberchk(ctr_arguments(A,B),L),
	   asserta(ctr_arguments(A,B)).

read_prolog_file(SrcDir,Name,L):-
        concat_string([SrcDir,'/',Name],File),
        open(File,read,S),
        read(S,X),
        read_lp(S,X,L),
	close(S).

read_lp(_S,end_of_file,[]):-
        !.
read_lp(S,X,[X|L]):-
        read(S,Y),
        read_lp(S,Y,L).

constraint_head(SS,Id,Name,Type,Desc,Indent,Options):-
        valid_option(constraint,Options),
        !,
        indent(SS,Indent),
        format(SS,'<constraint id=\'~w\' name=\'~w\' type=\'~w\' desc=\'~w\'>\n',
               [Id,Name,Type,Desc]).
constraint_head(SS,Id,Name,Type,Desc,Indent,Options):-
        valid_option(not(constraint),Options),
        !,
        indent(SS,Indent),
        format(SS,'<~w id=\'~w\' name=\'~w\' desc=\'~w\'>\n',
               [Type,Id,Name,Desc]).

constraint_tail(SS,_Type,Indent,Options):-
        valid_option(constraint,Options),
        !,
        indent(SS,Indent),
        format(SS,'</constraint>\n',[]).
constraint_tail(SS,Type,Indent,Options):-
        valid_option(not(constraint),Options),
        !,
        indent(SS,Indent),
        format(SS,'</~w>\n',[Type]).

reset_counters:-
        setval(id,0),
        setval(name,0).

schema_head(SS,Type,Indent,_Options):-
	Indent1 is Indent+1,
	Indent2 is Indent+2,
	element(SS,Indent,Type),
	complex_type(SS,Indent1,''),
	sequence(SS,Indent2,'').

schema_tail(SS,_Type,Indent,_Options):-
	Indent1 is Indent+1,
	complex_type(SS,Indent1,'/'),
	end_element(SS,Indent).


complex_type(SS,Indent,Slash):-
	indent(SS,Indent),
	format(SS,'<~wxs:complexType>\n',[Slash]).

sequence(SS,Indent,Slash):-
	indent(SS,Indent),
	format(SS,'<~wxs:sequence>\n',[Slash]).

choice(SS,Indent,Slash):-
	indent(SS,Indent),
	format(SS,'<~wxs:choice>\n',[Slash]).

choice(SS,Indent,Slash,Min,Max):-
	indent(SS,Indent),
	format(SS,'<~wxs:choice minOccurs=\'~w\' maxOccurs=\'~w\'>\n',
                  [Slash,Min,Max]).

element(SS,Indent,Name):-
	indent(SS,Indent),
	format(SS,'<xs:element name=\'~w\'>\n',[Name]).

element(SS,Indent,Name,Min,Max):-
	indent(SS,Indent),
	format(SS,'<xs:element name=\'~w\' minOccurs=\'~w\' maxOccurs=\'~w\'>\n',[Name,Min,Max]).

end_element(SS,Indent):-
	indent(SS,Indent),
        format(SS,'</xs:element>\n',[]).

schema_attribute(SS,Indent,Name,Type,Use):-
	indent(SS,Indent),
	format(SS,'<xs:attribute name=\'~w\' type=\'~w\' use=\'~w\'/>\n',[
		Name,Type,Use]).

restrict_integer(Collection,Name,Type,Restrictions,RestrictedType):-
        memberchk(Type,[dvar,int]),
        lower_restriction(Collection,Name,Restrictions,Low),
        upper_restriction(Collection,Name,Restrictions,High),
        decide_type(Low,High,RestrictedType),
        !.

restrict_atom(_Coll,Name,Type,Restrictions,RestrictedString):-
        memberchk(Type,[atom]),
        memberchk(in_list(Name,Values),Restrictions),
	map_atoms(Values, Mapped),
        build_restriction(Mapped,RestrictedString),
        !.
restrict_atom(Coll,Name,Basic,_Restrictions,'xs:string'):-
        writeln(basic(Coll,Name,Basic)).

map_atoms([], []).
map_atoms([X|Xs], [Y|Ys]) :-
	map_atom(X, Y),
	map_atoms(Xs, Ys).

required_attr(Collection,Name,Restrictions,1):-
        memberchk(required(Collection,Name),Restrictions),
        !.
required_attr(Collection,Name,Restrictions,1):-
        member(required(Collection,List),Restrictions),
        memberchk(Name,List),
        !.
required_attr(_Collection,_Name,_Restrictions,0).

lower_restriction(-,Name,Restrictions,N1):-
        memberchk((Name > N),Restrictions),
        integer(N),
        !,
        N1 is N+1.
lower_restriction(-,Name,Restrictions,N):-
        memberchk((Name >= N),Restrictions),
        integer(N),
        !.
lower_restriction(Coll,Name,Restrictions,N1):-
        memberchk((Coll^Name > N),Restrictions),
        integer(N),
        !,
        N1 is N+1.
lower_restriction(Coll,Name,Restrictions,N):-
        memberchk((Coll^Name >= N),Restrictions),
        integer(N),
        !.
lower_restriction(_,_,_,none).

upper_restriction(-,Name,Restrictions,N1):-
        memberchk((Name < N),Restrictions),
        integer(N),
        !,
        N1 is N-1.
upper_restriction(-,Name,Restrictions,N):-
        memberchk((Name =< N),Restrictions),
        integer(N),
        !.
upper_restriction(Coll,Name,Restrictions,N1):-
        memberchk((Coll^Name < N),Restrictions),
        integer(N),
        !,
        N1 is N-1.
upper_restriction(Coll,Name,Restrictions,N):-
        memberchk((Coll^Name =< N),Restrictions),
        integer(N),
        !.
upper_restriction(_,_,_,none).

decide_type(0,none,'xs:nonNegativeInteger'):-
        !.
decide_type(1,none,'xs:positiveInteger'):-
        !.
decide_type(none,none,'xs:integer'):-
        !.
decide_type(Low,High,String):-
        build_low_high_restriction(Low,High,String).

% find out which restrictions are not known 
restriction_types(L) :-
	restriction_types(L, Rem, []),
	writeln(Rem).

restriction_types([]) --> [].
restriction_types([X|L]) -->
	handled(X),
	restriction_types(L).
	

handled(X) -->
        {handle_table(X)}, !.
handled(X) --> [X].

handle_table(required(_,_)).
handle_table(in_list(_,_)).
handle_table((_ >= N)):-
        integer(N).
handle_table((_ > N)):-
        integer(N).
handle_table((_ < N)):-
        integer(N).
handle_table((_ =< N)):-
        integer(N).
% not handled
handle_table(require_at_least(_,_,_)).
handle_table((_ = _)).
handle_table((_ =\= _)).
handle_table((_ >= _)).
handle_table((_ =< _)).
handle_table((_ > _)).
handle_table((_ < _)).
handle_table(same_size(_,_)).
handle_table(distinct(_,_)).
handle_table(in_attr(_,_,_,_)).
handle_table(orth_link_ori_siz_end(_)).
handle_table(alldifferent(_)).
handle_table(increasing_seq(_,_)).
handle_table(diffn(_)).
handle_table(change(_,_,_)).
handle_table(lex_lesseq(_,_)).
handle_table(non_increasing_size(_,_)).
handle_table(strictly_increasing(_)).
handle_table(in_relation(_,_)).

% produce a restricted, enumerated string type as a simple type
build_restriction(Mapped,Name):-
	simple_type(Mapped,Name,_),
	!.
build_restriction(Mapped,Name):-
	concat_string([type|Mapped],Name),
	with_output_to_codes(build_restriction1(Stream,Mapped,Name), Stream, Codes, []),
	atom_codes(String, Codes),
	assert(simple_type(Mapped,Name,String)).

build_restriction1(S, Mapped, Name) :-
        format(S,'<xs:simpleType name=\'~w\'>\n',[Name]),
        format(S,' <xs:restriction base=\'xs:string\'>\n',[]),
	foreach(Mapped, fmt_enum_value(S)),
        format(S,' </xs:restriction>\n',[]),
        format(S,'</xs:simpleType>\n',[]).

fmt_enum_value(S, X) :-
	format(S,'  <xs:enumeration value=\'~w\'/>\n',[X]).

% produce a restricted integer type with bounds
build_low_high_restriction(Low,High,Name):-
	simple_type(int(Low,High),Name,_),
	!.
build_low_high_restriction(Low,High,Name):-
	concat_string([type,'_',Low,'_',High],Name),
	with_output_to_codes(build_low_high_restriction1(S,Low,High,Name), S, Codes, []),
	atom_codes(String, Codes),
	assert(simple_type(int(Low,High),Name,String)).

build_low_high_restriction1(S, Low, High, Name) :-
        format(S,'<xs:simpleType name=\'~w\'>\n',[Name]),
        format(S,' <xs:restriction base=\'xs:integer\'>\n',[]),
        (Low \= none ->
            format(S,'  <xs:minInclusive value=\'~w\'/>\n',[Low])
        ;
            true
        ),
        (High \= none ->
            format(S,'  <xs:maxInclusive value=\'~w\'/>\n',[High])
        ;
            true
        ),
        format(S,' </xs:restriction>\n',[]),
        format(S,'</xs:simpleType>\n',[]).

% produce the schema for an integer set
integer_set(SS,Indent):-
	Indent1 is Indent+1,
	Indent2 is Indent+2,
	Indent3 is Indent+3,
	Indent4 is Indent+4,
	Indent5 is Indent+5,
	Indent6 is Indent+6,
	element(SS,Indent1,integerset),
	complex_type(SS,Indent2,''),
        sequence(SS,Indent3,''),
        element(SS,Indent4,integer,0,unbounded),
	complex_type(SS,Indent5,''),
	schema_attribute(SS,Indent6,'value','xs:integer','required'),
	complex_type(SS,Indent5,'/'),
	end_element(SS,Indent4),
        sequence(SS,Indent3,'/'),
	complex_type(SS,Indent2,'/'),
	end_element(SS,Indent1).

% produce the schema for an integer
integer(SS,Indent,Collection,Name,Basic,Restrictions):-
	Indent1 is Indent+1,
	Indent2 is Indent+2,
	Indent3 is Indent+3,
	element(SS,Indent1,integer),
	complex_type(SS,Indent2,''),
        restrict_integer(Collection,Name,Basic,Restrictions,ValueType),
	schema_attribute(SS,Indent3,'value',ValueType,'required'),
	complex_type(SS,Indent2,'/'),
	end_element(SS,Indent1).

% produce the schema for an atom
atom(SS,Indent,Collection,Name,Basic,Restrictions):-
	Indent1 is Indent+1,
	Indent2 is Indent+2,
	Indent3 is Indent+3,
	element(SS,Indent1,atom),
	complex_type(SS,Indent2,''),
        restrict_atom(Collection,Name,Basic,Restrictions,ValueType),
	schema_attribute(SS,Indent3,'value',ValueType,'required'),
	complex_type(SS,Indent2,'/'),
	end_element(SS,Indent1).

% produce the schema for a variable reference
variable_ref(SS,Indent):-
	Indent1 is Indent+1,
	Indent2 is Indent+2,
	Indent3 is Indent+3,
	element(SS,Indent1,variableref),
	complex_type(SS,Indent2,''),
	schema_attribute(SS,Indent3,'id','xs:IDREF','required'),
	complex_type(SS,Indent2,'/'),
	end_element(SS,Indent1).
