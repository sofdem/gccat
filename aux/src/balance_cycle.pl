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

ctr_date(balance_cycle,['20111218']).

ctr_origin(balance_cycle, 'derived from %c and %c', [balance,cycle]).

ctr_arguments(balance_cycle,
              ['BALANCE'-dvar                          ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(balance_cycle,
                 [items('NODES',all)]).

ctr_restrictions(balance_cycle,
                 ['BALANCE'     >= 0                     ,
                  'BALANCE'     =< max(0,size('NODES')-2),
                  required('NODES',[index,succ])         ,
                  'NODES'^index >= 1                     ,
                  'NODES'^index =< size('NODES')         ,
                  distinct('NODES',index)                ,
                  'NODES'^succ  >= 1                     ,
                  'NODES'^succ  =< size('NODES')         ]).

ctr_typical(balance_cycle,
            [size('NODES') > 2]).

ctr_functional_dependency(balance_cycle, 1, [2]).

ctr_graph(balance_cycle,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['NTREE'     = 0        ,
           'RANGE_NCC' = 'BALANCE'],
          ['ONE_SUCC']).

ctr_example(balance_cycle,
            [balance_cycle(1,[[index-1, succ-2],[index-2, succ-1],[index-3, succ-5],[index-4, succ-3],[index-5, succ-4]]),
	     balance_cycle(0,[[index-1, succ-2],[index-2, succ-3],[index-3, succ-1],[index-4, succ-5],[index-5, succ-6],[index-6, succ-4]]),
	     balance_cycle(4,[[index-1, succ-2],[index-2, succ-3],[index-3, succ-4],[index-4, succ-5],[index-5, succ-1],[index-6, succ-6]])]).

ctr_draw_example(balance_cycle,
                 ['NODES'],
                 [[[index-1, succ-2],
                   [index-2, succ-1],
                   [index-3, succ-5],
                   [index-4, succ-3],
                   [index-5, succ-4]]],
                 ['CLIQUE'],
                 [1-2,2-1,3-5,4-3,5-4],
                 ['RANGE_NCC'([1,2],[3,5,4])],
                 '','NTREE=0, RANGE_NCC=3-2=1',
                 [2.145,2.145,2.145,2.145]).

ctr_cond_imply(balance_cycle, all_differ_from_at_least_k_pos, ['BALANCE' > 0, 'BALANCE' =< 2], [], [same('BALANCE'),same('NODES')]).
ctr_cond_imply(balance_cycle, permutation,                    [],                              [], [index_to_col('NODES')]).

ctr_see_also(balance_cycle,
 [link('related', balance, 'equivalence classes correspond to vertices in same cycle rather than variables assigned to the same value', []),
  link('related', cycle, 'do not care how many cycles but how balanced the cycles are', [])]).

ctr_key_words(balance_cycle,['graph constraint'             ,
                             'circuit'                      ,
                             'cycle'                        ,
                             'permutation'                  ,
                             'graph partitioning constraint',
                             'connected component'          ,
                             'strongly connected component' ,
                             'one\\_succ'                   ,
                             'DFS-bottleneck'               ,
                             'functional dependency'        ]).

ctr_application(balance_cycle, [2]).

ctr_eval(balance_cycle, [checker(balance_cycle_c)]).

ctr_sol(balance_cycle,2,0,2,2,[0-2]).
ctr_sol(balance_cycle,3,0,3,6,[0-3,1-3]).
ctr_sol(balance_cycle,4,0,4,24,[0-10,1-6,2-8]).
ctr_sol(balance_cycle,5,0,5,120,[0-25,1-45,2-20,3-30]).
ctr_sol(balance_cycle,6,0,6,720,[0-176,1-60,2-250,3-90,4-144]).
ctr_sol(balance_cycle,7,0,7,5040,[0-721,1-861,2-770,3-1344,4-504,5-840]).
ctr_sol(balance_cycle,8,0,8,40320,[0-6406,1-1778,2-7980,3-6300,4-8736,5-3360,6-5760]).
ctr_sol(balance_cycle,9,0,9,362880,[0-42561,1-23283,2-38808,3-75348,4-45360,5-66240,6-25920,7-45360]).
ctr_sol(balance_cycle,10,0,10,3628800,[0-436402,1-84150,2-363680,3-456120,4-708048,5-378000,6-572400,7-226800,8-403200]).

balance_cycle_c(BALANCE, NODES):-
        length(NODES, N),
	N2 is max(N-2,0),
        check_type(dvar(0,N2), BALANCE),
        collection(NODES, [int(1,N),dvar(1,N)]),
        get_attr1(NODES, INDEXES),
        get_attr2(NODES, SUCCS),
	sort(INDEXES, Js),
	sort(SUCCS, Js),
        length(Js,N),
        (   for(J,1,N),
	    foreach(X,SUCCS),
	    foreach(Free,Term),
	    foreach(Free-1,KeyTerm),
	    foreach(J,Js),
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
