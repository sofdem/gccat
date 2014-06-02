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

ctr_date(balance,['20000128','20030820','20060804','20110713']).

ctr_origin(balance, 'N.~Beldiceanu', []).

ctr_arguments(balance,
              ['BALANCE'-dvar                  ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(balance,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(balance,
                 ['BALANCE' >= 0                         ,
                  'BALANCE' =< max(0,size('VARIABLES')-2),
                  required('VARIABLES',var)              ]).

ctr_typical(balance,
            ['BALANCE'         =< 2 + size('VARIABLES')/10,
	     size('VARIABLES') >  2                       ]).

ctr_pure_functional_dependency(balance, []).
ctr_functional_dependency(balance, 1, [2]).

ctr_graph(balance,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['RANGE_NSCC' = 'BALANCE'],
          ['EQUIVALENCE']).

ctr_example(balance,
            [balance(2,[[var-3],[var-1],[var-7],[var-1],[var-1]]),
	     balance(0,[[var-3],[var-3],[var-1],[var-1],[var-1],[var-3]]),
	     balance(4,[[var-3],[var-1],[var-1],[var-1],[var-1],[var-1]])]).

ctr_draw_example(balance,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-7],[var-1],[var-1]]],
                 ['CLIQUE'],
                 [1-1,
                  2-[2,4,5],
                  3-3,
                  4-[2,4,5],
                  5-[2,4,5]],
                 ['RANGE_NSCC'([1],[2,4,5])],
                 '','RANGE_NSCC=3-1=2',
                 [2.145,2.3,2.145,2.3]).

ctr_see_also(balance,
 [link('implies',          soft_all_equal_min_ctr, '',                                                                    []),
  link(generalisation,     balance_interval,       '%e replaced by %e',                                                   [variable,variable/constant]),
  link(generalisation,     balance_modulo,         '%e replaced by %e',                                                   [variable,variable mod constant]),
  link(generalisation,     balance_partition,      '%e replaced by %e',                                                   [variable,in_list(variable,partition)]),
  link('shift of concept', equilibrium,            '',                                                                    []),
  link('related',          nvalue,                 'no restriction on how balanced an assignment is',                     []),
  link('related',          tree_range,             'balanced assignment versus balanced tree',                            []),
  link('related',          balance_cycle,          'balanced assignment versus graph partitionning with balanced cycles', []),
  link('related',          balance_path,           'balanced assignment versus graph partitionning with balanced paths',  []),
  link('related',          balance_tree,           'balanced assignment versus graph partitionning with balanced trees',  [])]).

ctr_key_words(balance,['value constraint'                ,
                       'assignment'                      ,
                       'balanced assignment'             ,
                       'automaton'                       ,
                       'automaton with array of counters',
                       'equivalence'                     ,
                       'functional dependency'           ,
		       'pure functional dependency'      ]).

ctr_persons(balance,['Beldiceanu N.']).

ctr_eval(balance, [      checker(balance_c),
		   reformulation(balance_r)]).

ctr_sol(balance,2,0,2,9,[0-9]).
ctr_sol(balance,3,0,3,64,[0-28,1-36]).
ctr_sol(balance,4,0,4,625,[0-185,1-360,2-80]).
ctr_sol(balance,5,0,5,7776,[0-726,1-5700,2-1200,3-150]).
ctr_sol(balance,6,0,6,117649,[0-8617,1-75600,2-30030,3-3150,4-252]).
ctr_sol(balance,7,0,7,2097152,[0-40328,1-1342600,2-611520,3-95256,4-7056,5-392]).
ctr_sol(balance,8,0,8,43046721,[0-682929,1-24272640,2-15350832,3-2469600,4-256032,5-14112,6-576]).

balance_c(0, []) :- !.
balance_c(BALANCE, VARIABLES) :-
    check_type(dvar, BALANCE),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    N2 is max(N-2,0),
    BALANCE #>= 0,
    BALANCE #=< N2,
    samsort(VARS, SVARS),
    SVARS = [V|R],
    min_max_seq_size(R, 1, V, N, 1, MIN, MAX),
    BALANCE #= MAX - MIN.

min_max_seq_size([], C, _, BestMin, BestMax, ResMin, ResMax) :-
    !,
    ResMin is min(C,BestMin),
    ResMax is max(C,BestMax).
min_max_seq_size([V|R], C, V, BestMin, BestMax, ResMin, ResMax) :-
    !,
    C1 is C+1,
    min_max_seq_size(R, C1, V, BestMin, BestMax, ResMin, ResMax).
min_max_seq_size([V|R], C, Prev, BestMin, BestMax, ResMin, ResMax) :-
    C > 0,
    V =\= Prev,
    NewBestMin is min(C,BestMin),
    NewBestMax is max(C,BestMax),
    min_max_seq_size(R, 1, V, NewBestMin, NewBestMax, ResMin, ResMax).

balance_r(0, []) :- !.
balance_r(BALANCE, VARIABLES) :-
    check_type(dvar, BALANCE),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    N2 is max(N-2,0),
    BALANCE #>= 0,
    BALANCE #=< N2,
    union_dom_list_int(VARS, UnionDomainsVARS),
    NSquare is N*N,
    length(UnionDomainsVARS, SizeUnion),
    (SizeUnion =< NSquare ->
	balance1(UnionDomainsVARS, N, VALS, OCCS, OCCS1),
	eval(global_cardinality(VARIABLES, VALS))
    ;
	balance2(VARS, N, VARS, OCCS),
	OCCS1 = OCCS
    ),
    MIN in 1..N,
    MAX in 1..N,
    eval(minimum(MIN,OCCS1)),
    eval(maximum(MAX,OCCS)),
    BALANCE + MIN #= MAX.
