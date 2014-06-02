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

ctr_date(decreasing,['20040814','20060808']).

ctr_origin(decreasing, 'Inspired by %c.', [increasing]).

ctr_arguments(decreasing,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(decreasing,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(decreasing,
                 [required('VARIABLES',var)]).

ctr_typical(decreasing,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1]).

ctr_contractible(decreasing, [], 'VARIABLES', any).

ctr_graph(decreasing,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables1^var >= variables2^var],
          ['NARC' = size('VARIABLES')-1],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(decreasing,
            decreasing([[var-8],[var-4],[var-1],[var-1]])).

ctr_draw_example(decreasing,
                 ['VARIABLES'],
                 [[[var-8],[var-4],[var-1],[var-1]]],
                 ['PATH'],
                 [1-2,2-3,3-4],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,2]).

ctr_see_also(decreasing,
 [link('implies',            multi_global_contiguity, '',   []),
  link('implies',            no_peak,                 '',   []),
  link('implies',            no_valley,               '',   []),
  link('implied by',         strictly_decreasing,     '',   []),
  link('implied by',         all_equal,               '',   []),
  link('comparison swapped', increasing,              '',   []),
  link('common keyword',     strictly_increasing,     '%k', ['order constraint'])]).

ctr_key_words(decreasing,['decomposition'                          ,
                          'order constraint'                       ,
                          'automaton'                              ,
                          'automaton without counters'             ,
                          'reified automaton constraint'           ,
                          'sliding cyclic(1) constraint network(1)',
                          'arc-consistency'                        ,
                          'acyclic'                                ,
                          'bipartite'                              ,
                          'no loop'                                ]).

ctr_eval(decreasing, [  checker(decreasing_c),
		      automaton(decreasing_a)]).

ctr_sol(decreasing,2,0,2,6,-).
ctr_sol(decreasing,3,0,3,20,-).
ctr_sol(decreasing,4,0,4,70,-).
ctr_sol(decreasing,5,0,5,252,-).
ctr_sol(decreasing,6,0,6,924,-).
ctr_sol(decreasing,7,0,7,3432,-).
ctr_sol(decreasing,8,0,8,12870,-).

decreasing_c([[var-X],[var-Y]|_]) :-    
    X < Y,
    !,
    fail.
decreasing_c([]) :- !.
decreasing_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    decreasing_c1(VARS).

decreasing_c1([X,Y|R]) :-
    !,
    X >= Y,
    decreasing_c1([Y|R]).
decreasing_c1([_]) :- !.
decreasing_c1([]).

% 0: VAR1< VAR2
% 1: VAR1>=VAR2
decreasing_a(1, []) :- !.
decreasing_a(0, []) :- !, fail.
decreasing_a(FLAG, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    decreasing_signature(VARIABLES, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,1,s)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

decreasing_signature([_], []) :- !.
decreasing_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss]) :-
    S in 0..1,
    VAR1 #>= VAR2 #<=> S,
    decreasing_signature([[var-VAR2]|VARs], Ss).
