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

ctr_date(proper_circuit,['20120429']).

ctr_origin(proper_circuit, 'Derived from %c', [circuit]).

ctr_arguments(proper_circuit,
              ['NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(proper_circuit,
                 [items('NODES',all)]).

ctr_synonyms(proper_circuit,[circuit]).

ctr_restrictions(proper_circuit,
                 [size('NODES') >  1            ,
		  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ >= 1             ,
                  'NODES'^succ =< size('NODES') ]).

ctr_typical(proper_circuit,
            [size('NODES') > 2]).

ctr_predefined(proper_circuit).

ctr_example(proper_circuit,
            proper_circuit([[index-1, succ-2],
                            [index-2, succ-3],
                            [index-3, succ-1],
                            [index-4, succ-4]])).

ctr_see_also(proper_circuit,
 [link('implies',                       permutation,        '',       []),
  link('implies',                       twin,               '',       []),
  link('implies (items to collection)', lex_alldifferent,   '',       []),
  link('implied by',                    circuit,            '',       []),
  link('common keyword',                path,               '%k, %k', ['graph partitioning constraint', 'one\\_succ']),
  link('common keyword',                alldifferent,       '%k',     ['permutation']),
  link('common keyword',                circuit,            '%k, %k', ['permutation', 'one\\_succ'])]).

ctr_key_words(proper_circuit,['graph constraint'             ,
                              'graph partitioning constraint',
                              'circuit'                      ,
                              'permutation'                  ,
                              'one\\_succ'                   ,
                              'DFS-bottleneck'               ]).

ctr_application(proper_circuit, [1]).

ctr_eval(proper_circuit, [checker(proper_circuit_c),
			  reformulation(proper_circuit_r)]).

ctr_sol(proper_circuit,2,0,2,1,-).
ctr_sol(proper_circuit,3,0,3,5,-).
ctr_sol(proper_circuit,4,0,4,20,-).
ctr_sol(proper_circuit,5,0,5,84,-).
ctr_sol(proper_circuit,6,0,6,409,-).
ctr_sol(proper_circuit,7,0,7,2365,-).
ctr_sol(proper_circuit,8,0,8,16064,-).
ctr_sol(proper_circuit,9,0,9,125664,-).
ctr_sol(proper_circuit,10,0,10,1112073,-).

proper_circuit_c([[_,succ-V],[_,succ-V]|_]) :-
    !,
    fail.
proper_circuit_c(NODES) :-
    length(NODES, N),
    N > 1,
    collection(NODES, [int(1,N),int(1,N)]),
    sort_collection(NODES, index, SORTED_NODES),
    get_attr1(SORTED_NODES, INDEXES),
    get_attr2(SORTED_NODES, SUCCS),
    (   for(J,1,N),
	foreach(X,SUCCS),
	foreach(Free-1,KeyTerm),
	foreach(J,Js),
	param(KeyTerm)
    do  nth1(X, KeyTerm, Free-1)
    ),
    sort(INDEXES, Js),
    sort(SUCCS, Js),
    keysort(KeyTerm, KeySorted),
    keyclumped(KeySorted, KeyClumped),
    (   foreach(_-Ones,KeyClumped),
	foreach(Count,Counts)	% Counts = list of lengths of cycles
    do  length(Ones, Count)
    ),
    max_member(Max, Counts),
    Max > 1,
    length(Counts, M),
    M+Max =:= N+1.

proper_circuit_r(NODES):-
    length(NODES, N),
    collection(NODES, [int(1,N),dvar(1,N)]),
    get_attr1(NODES, IND),
    sort(IND, SIND),
    length(SIND, N),
    get_attr12(NODES, IND_SUCC),
    keysort(IND_SUCC, SIND_SUCC),
    remove_key_from_collection(SIND_SUCC, Succ),
    all_distinct(Succ),
    (for(I,1,N),
	foreach(Min,Mins),
	param(Succ,N)
    do  length([I|Ss], N),
	minimum(Min, [I|Ss]),
	(foreach(S2,Ss),
	    fromto(I,S1,S2,_),
	    param(Succ)
	do  element(S1, Succ, S2)
	)
    ),
    (for(J,1,N),
	foreach(J-C,ICs),
	foreach(C,Cs)
    do  true
    ),
    global_cardinality(Mins, ICs),
    length(Ps, N),
    length(Vs, N),
    Max1 in 2..N,
    Max2 in 0..1,
    nth1(N, Vs, Max1),
    N1 is N-1,
    nth1(N1, Vs, Max2),
    sorting(Cs, Ps, Vs).
