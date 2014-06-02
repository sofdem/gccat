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

ctr_date(increasing,['20040814','20060810','20091105']).

ctr_origin(increasing, 'KOALOG', []).

ctr_arguments(increasing,
	      ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(increasing,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(increasing,
                 [required('VARIABLES',var)]).

ctr_typical(increasing,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1]).

ctr_contractible(increasing, [], 'VARIABLES', any).

ctr_graph(increasing,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables1^var =< variables2^var],
          ['NARC' = size('VARIABLES')-1],
          []).

ctr_example(increasing,
            increasing([[var-1],[var-1],[var-4],[var-8]])).

ctr_draw_example(increasing,
                 ['VARIABLES'],
                 [[[var-1],[var-1],[var-4],[var-8]]],
                 ['PATH'],
                 [1-2,2-3,3-4],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,2]).

ctr_see_also(increasing,
 [link('implies',                   multi_global_contiguity,       '',                                             []),
  link('implies',                   no_peak,                       '',                                             []),
  link('implies',                   no_valley,                     '',                                             []),
  link('implied by',                strictly_increasing,           '',                                             []),
  link('implied by',                all_equal,                     '',                                             []),
  link('implied by',                increasing_global_cardinality, '',                                             []),
  link('implied by',                increasing_nvalue,             'remove $\\argument{NVAL}$ parameter from %c',  [increasing_nvalue]),
  link('implied by',                increasing_sum,                'remove $\\argument{SUM}$ parameter from %c',   [increasing_sum]),
  link('comparison swapped',        decreasing,                    '',                                             []),
  link('common keyword',            strictly_decreasing,           '%k',                                           ['order constraint']),
  link('common keyword',            precedence,                    '%k',                                           ['order constraint']),
  link('uses in its reformulation', sort_permutation,              '',                                             [])]).

ctr_key_words(increasing,['decomposition'                          ,
                          'order constraint'                       ,
                          'automaton'                              ,
                          'automaton without counters'             ,
                          'reified automaton constraint'           ,
                          'sliding cyclic(1) constraint network(1)',
                          'arc-consistency'                        ]).

ctr_eval(increasing, [      checker(increasing_c),
		      reformulation(increasing_r),
                          automaton(increasing_a)]).

ctr_sol(increasing,2,0,2,6,-).
ctr_sol(increasing,3,0,3,20,-).
ctr_sol(increasing,4,0,4,70,-).
ctr_sol(increasing,5,0,5,252,-).
ctr_sol(increasing,6,0,6,924,-).
ctr_sol(increasing,7,0,7,3432,-).
ctr_sol(increasing,8,0,8,12870,-).

increasing_c([[var-X],[var-Y]|_]) :-    
    X > Y,
    !,
    fail.
increasing_c([]) :- !.
increasing_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    increasing_c1(VARS).

increasing_c1([X,Y|R]) :-
    !,
    X =< Y,
    increasing_c1([Y|R]).
increasing_c1([_]) :- !.
increasing_c1([]).

increasing_r([]) :-
    !.
increasing_r(VARIABLES) :-
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    increasing1(VARS).

increasing1([_]) :- !.
increasing1([V1,V2|R]) :-
    V1 #=< V2,
    increasing1([V2|R]).

% 0: VAR1 >VAR2
% 1: VAR1=<VAR2
increasing_a(1, []) :- !.
increasing_a(0, []) :- !, fail.
increasing_a(FLAG, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    increasing_signature(VARIABLES, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,1,s)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

increasing_signature([_], []) :- !.
increasing_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss]) :-
        S in 0..1,
        VAR1 #=< VAR2 #<=> S,
        increasing_signature([[var-VAR2]|VARs], Ss).
