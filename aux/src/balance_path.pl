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

ctr_date(balance_path,['20111226']).

ctr_origin(balance_path, 'derived from %c and %c', [balance,path]).

ctr_arguments(balance_path,
              ['BALANCE'-dvar                          ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(balance_path,
                 [items('NODES',all)]).

ctr_restrictions(balance_path,
                 ['BALANCE'     >= 0                     ,
                  'BALANCE'     =< max(0,size('NODES')-2),
                  required('NODES',[index,succ])         ,
                  'NODES'^index >= 1                     ,
                  'NODES'^index =< size('NODES')         ,
                  distinct('NODES',index)                ,
                  'NODES'^succ  >= 1                     ,
                  'NODES'^succ  =< size('NODES')         ]).

ctr_typical(balance_path,
            [size('NODES') > 2]).

ctr_functional_dependency(balance_path, 1, [2]).

ctr_graph(balance_path,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['MAX_NSCC'  =< 1       ,
	   'MAX_ID'    =< 1       ,
           'RANGE_NCC' = 'BALANCE'],
          ['ONE_SUCC']).

ctr_example(balance_path,
            [balance_path(3,[[index-1, succ-1],[index-2, succ-3],[index-3, succ-5],[index-4, succ-4],
			     [index-5, succ-1],[index-6, succ-6],[index-7, succ-7],[index-8, succ-6]]),
	     balance_path(0,[[index-1, succ-2],[index-2, succ-3],[index-3, succ-4],[index-4, succ-4],
			     [index-5, succ-6],[index-6, succ-7],[index-7, succ-8],[index-8, succ-8]]),
	     balance_path(6,[[index-1, succ-2],[index-2, succ-3],[index-3, succ-4],[index-4, succ-5],
			     [index-5, succ-6],[index-6, succ-7],[index-7, succ-7],[index-8, succ-8]])]).

ctr_draw_example(balance_path,
                 ['NODES'],
                 [[[index-1, succ-1],
		   [index-2, succ-3],
		   [index-3, succ-5],
		   [index-4, succ-4],
		   [index-5, succ-1],
		   [index-6, succ-6],
		   [index-7, succ-7],
		   [index-8, succ-6]]],
                 ['CLIQUE'],
                 [1-1,2-3,3-5,4-4,5-1,6-6,7-7,8-6],
                 ['RANGE_NCC'([4],[1,5,3,2])],
                 '','MAX_NSCC=1, MAX_ID=1\\nRANGE_NCC=4-1=3',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(balance_path,
 [link('implies', balance_tree, '',                                                                                                         []),
  link('related', balance,      'equivalence classes correspond to vertices in same path rather than variables assigned to the same value', []),
  link('related', path,         'do not care how many paths but how balanced the paths are',                                                [])]).

ctr_key_words(balance_path,['graph constraint'             ,
                            'graph partitioning constraint',
                            'connected component'          ,
                            'path'                         ,
                            'tree'                         ,
                            'one\\_succ'                   ,
                            'DFS-bottleneck'               ,
                            'functional dependency'        ]).

ctr_application(balance_path, [2]).

ctr_eval(balance_path, [checker(balance_path_c)]).

ctr_sol(balance_path,2,0,2,3,[0-3]).
ctr_sol(balance_path,3,0,3,13,[0-7,1-6]).
ctr_sol(balance_path,4,0,4,73,[0-37,1-12,2-24]).
ctr_sol(balance_path,5,0,5,501,[0-121,1-200,2-60,3-120]).
ctr_sol(balance_path,6,0,6,4051,[0-1201,1-210,2-1560,3-360,4-720]).
ctr_sol(balance_path,7,0,7,37633,[0-5041,1-8862,2-5250,3-10920,4-2520,5-5040]).
ctr_sol(balance_path,8,0,8,394353,[0-62161,1-24416,2-97776,3-62160,4-87360,5-20160,6-40320]).

balance_path_c(BALANCE, NODES):-
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
	balance_path1(INDEXES, SUCCS, RANKS, SUCC_WITHOUT_LOOPS),
	sort(SUCC_WITHOUT_LOOPS, SSUCC_WITHOUT_LOOPS),  % no two nodes having the same father
	length(SUCC_WITHOUT_LOOPS, NSL),
	length(SSUCC_WITHOUT_LOOPS, NSL),
        (   % for(J,1,N),		                        % compute BALANCE
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

balance_path1([], [], _, []) :- !.
balance_path1([I|RI], [I|RS], RANKS, SUCC) :-
	!,
	balance_path1(RI, RS, RANKS, SUCC).
balance_path1([I|RI], [S|RS], RANKS, [S|SUCC]) :-
	nth1(I, RANKS, Ri),
	nth1(S, RANKS, Rs),
	Ri #< Rs,	
	balance_path1(RI, RS, RANKS, SUCC).
