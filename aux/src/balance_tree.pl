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

:- multifile
    ctr_predefined/1,
    ctr_date/2,
    ctr_persons/2,
    ctr_origin/3,
    ctr_usual_name/2,
    ctr_synonyms/2,
    ctr_types/2,
    ctr_arguments/2,
    ctr_exchangeable/2,
    ctr_restrictions/2,
    ctr_typical/2,
    ctr_pure_functional_dependency/2,
    ctr_functional_dependency/3,
    ctr_contractible/4,
    ctr_extensible/4,
    ctr_aggregate/3,
    ctr_example/2,
    ctr_draw_example/9,
    ctr_cond_imply/5,
    ctr_see_also/2,
    ctr_key_words/2,
    ctr_derived_collections/2,
    ctr_graph/7,
    ctr_graph/9,
    ctr_eval/2,
    ctr_sol/6,
    ctr_logic/3,
    ctr_application/2.

ctr_date(balance_tree,['20111226']).

ctr_origin(balance_tree, 'derived from %c and %c', [balance,tree]).

ctr_arguments(balance_tree,
              ['BALANCE'-dvar                          ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(balance_tree,
                 [items('NODES',all)]).

ctr_restrictions(balance_tree,
                 ['BALANCE'     >= 0                     ,
                  'BALANCE'     =< max(0,size('NODES')-2),
                  required('NODES',[index,succ])         ,
                  'NODES'^index >= 1                     ,
                  'NODES'^index =< size('NODES')         ,
                  distinct('NODES',index)                ,
                  'NODES'^succ  >= 1                     ,
                  'NODES'^succ  =< size('NODES')         ]).
		  
ctr_typical(balance_tree,
            [size('NODES') > 2]).

ctr_functional_dependency(balance_tree, 1, [2]).

ctr_graph(balance_tree,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['MAX_NSCC'  =< 1       ,
           'RANGE_NCC' = 'BALANCE'],
          []).

ctr_example(balance_tree,
            [balance_tree(4,[[index-1, succ-1],[index-2, succ-5],[index-3, succ-5],[index-4, succ-7],
			     [index-5, succ-1],[index-6, succ-1],[index-7, succ-7],[index-8, succ-5]]),
	     balance_tree(2,[[index-1, succ-1],[index-2, succ-1],[index-3, succ-1],
			     [index-4, succ-2],[index-5, succ-6],[index-6, succ-6]])]).

ctr_draw_example(balance_tree,
                 ['NODES'],
                 [[[index-1, succ-1],
		   [index-2, succ-5],
		   [index-3, succ-5],
		   [index-4, succ-7],
		   [index-5, succ-1],
		   [index-6, succ-1],
		   [index-7, succ-7],
		   [index-8, succ-5]]],
                 ['CLIQUE'],
                 [1-1,2-5,3-5,4-7,5-1,6-1,7-7,8-5],
                 ['RANGE_NCC'([4,7],[2,3,8,5,6,1])],
                 '','MAX_NSCC=1, RANGE_NCC=6-2=4',
                 [2.145,2.145,2.145,2.145]).

ctr_cond_imply(balance_tree, ordered_atleast_nvector, ['BALANCE' > 0, 'BALANCE' =< size('NODES')], [], [same('BALANCE'),same('NODES')]).

ctr_see_also(balance_tree,
 [link('implied by', balance_path, '',                                                                                                         []),
  link('related',    balance,      'equivalence classes correspond to vertices in same tree rather than variables assigned to the same value', []),
  link('related',    tree,         'do not care how many trees but how balanced the trees are',                                                [])]).

ctr_key_words(balance_tree,['graph constraint'             ,
			    'graph partitioning constraint',
			    'connected component'          ,
			    'tree'                         ,
			    'one\\_succ'                   ,
			    'functional dependency'        ,
			    'DFS-bottleneck'               ]).

ctr_application(balance_tree, [2]).

ctr_eval(balance_tree, [checker(balance_tree_c)]).

ctr_sol(balance_tree,2,0,2,3,[0-3]).
ctr_sol(balance_tree,3,0,3,16,[0-10,1-6]).
ctr_sol(balance_tree,4,0,4,125,[0-77,1-12,2-36]).
ctr_sol(balance_tree,5,0,5,1296,[0-626,1-260,2-90,3-320]).
ctr_sol(balance_tree,6,0,6,16807,[0-8707,1-210,2-3180,3-960,4-3750]).
ctr_sol(balance_tree,7,0,7,262144,[0-117650,1-25242,2-9765,3-41930,4-13125,5-54432]).
ctr_sol(balance_tree,8,0,8,4782969,[0-2242193,1-49616,2-432264,3-219520,4-680456,5-217728,6-941192]).

balance_tree_c(BALANCE, NODES):-
        length(NODES, N),	                        % parameters checking
	N2 is max(N-2,0),
        check_type(dvar(0,N2), BALANCE),
        collection(NODES, [int(1,N),dvar(1,N)]),
        get_attr1(NODES, INDEXES),
        get_attr2(NODES, SUCCS),
	sort(INDEXES, SIND),	                        % indexes are distinct
	length(SIND, N),
	length(RANKS, N),	                        % no cycle
	domain(RANKS, 1, N),
	balance_tree1(INDEXES, SUCCS, RANKS),
        (   % for(J,1,N),		                % compute BALANCE
	    foreach(X,SUCCS),
	    foreach(Free,Term),
	    foreach(Free-1,KeyTerm),
	    % foreach(J,Js),
	    param(Term,N)
	do  nth1(X, Term, Free)
        ),
	keysort(KeyTerm, KeySorted),
	keyclumped(KeySorted, KeyClumped),
	(   foreach(_-Ones,KeyClumped),
	    foreach(Count,Counts)
	do  length(Ones, Count)
	),
	min_member(Min, Counts),
	max_member(Max, Counts),
	BALANCE is Max-Min.

balance_tree1([], [], _) :- !.
balance_tree1([I|RI], [I|RS], RANKS) :-
	!,
	balance_tree1(RI, RS, RANKS).
balance_tree1([I|RI], [S|RS], RANKS) :-
	nth1(I, RANKS, Ri),
	nth1(S, RANKS, Rs),
	Ri #< Rs,	
	balance_tree1(RI, RS, RANKS).
