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

ctr_date(between_min_max,['20050824','20060804']).

ctr_origin(between_min_max, 'Used for defining %c.', [cumulative_convex]).

ctr_arguments(between_min_max,
              ['VAR'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(between_min_max,
                 [items('VARIABLES',all),
                  vals(['VAR'],int(['VAR','VARIABLES'^var]),=\=,all,dontcare)]).

ctr_restrictions(between_min_max,
                 [required('VARIABLES',var),
                  size('VARIABLES') > 0    ]).

ctr_typical(between_min_max,
            [ size('VARIABLES')     > 1,
             range('VARIABLES'^var) > 1]).

ctr_extensible(between_min_max, [], 'VARIABLES', any).

ctr_derived_collections(between_min_max,
                        [col('ITEM'-collection(var-dvar),
                             [item(var-'VAR')])]).

ctr_graph(between_min_max,
          ['ITEM','VARIABLES'],
          2,
          ['PRODUCT'>>collection(item,variables)],
          [item^var >= variables^var],
          ['NARC' >= 1],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_graph(between_min_max,
          ['ITEM','VARIABLES'],
          2,
          ['PRODUCT'>>collection(item,variables)],
          [item^var =< variables^var],
          ['NARC' >= 1],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(between_min_max,
            [between_min_max(3,[[var-1],[var-1],[var-4],[var-8]]),
	     between_min_max(1,[[var-1],[var-1],[var-4],[var-8]]),
	     between_min_max(8,[[var-1],[var-1],[var-4],[var-8]])]).

ctr_draw_example(between_min_max,
                 ['ITEM','VARIABLES'],
                 [[[var-3]],
                  [[var-1],[var-1],[var-4],[var-8]]],
                 ['PRODUCT'],
                 [1-[1,2]],
                 ['NARC'],
                 '','NARC=2',
                 [2.145,2.145,1.15,1.15]).

ctr_see_also(between_min_max,
 [link('implied by', and,                '', []),
  link('implied by', deepest_valley,     '', []),
  link('implied by', first_value_diff_0, '', []),
  link('implied by', highest_peak,       '', []),
  link('implied by', in,                 '', []),
  link('implied by', maximum,            '', []),
  link('implied by', minimum,            '', [])]).

ctr_key_words(between_min_max,['automaton'                               ,
                               'automaton without counters'              ,
                               'reified automaton constraint'            ,
                               'centered cyclic(1) constraint network(1)']).

ctr_eval(between_min_max, [checker(between_min_max_c),
			   reformulation(between_min_max_r),
                           automaton(between_min_max_a)]).

ctr_sol(between_min_max,2,0,2,17,[0-5,1-7,2-5]).
ctr_sol(between_min_max,3,0,3,184,[0-37,1-55,2-55,3-37]).
ctr_sol(between_min_max,4,0,4,2417,[0-369,1-543,2-593,3-543,4-369]).
ctr_sol(between_min_max,5,0,5,37806,[0-4651,1-6751,2-7501,3-7501,4-6751,5-4651]).
ctr_sol(between_min_max,6,0,6,689201,[0-70993,1-102023,2-113489,3-116191,4-113489,5-102023,6-70993]).
ctr_sol(between_min_max,7,0,7,14376608,[0-1273609,1-1817215,2-2018899,3-2078581,4-2078581,5-2018899,6-1817215,7-1273609]).
ctr_sol(between_min_max,8,0,8,338051265,[0-26269505,1-37281919,2-41366849,3-42649535,4-42915649,5-42649535,6-41366849,7-37281919,8-26269505]).

between_min_max_c(VAR, VARIABLES) :-
    integer(VAR),
    between_min_max_c(VARIABLES, VAR, 0, 0).

between_min_max_c([], _, 1, 1) :- !.
between_min_max_c([[var-V]|R], VAR, Min, Max) :-
    integer(V),
    (VAR = V ->
	between_min_max_c(R, VAR, 1, 1)
    ;
     VAR > V ->
	between_min_max_c(R, VAR, 1, Max)
     ;
	between_min_max_c(R, VAR, Min, 1)
    ).

between_min_max_r(VAR, VARIABLES) :-
    check_type(dvar, VAR),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    get_attr1(VARIABLES, VARS),
    get_minimum(VARS, MINIMUM),
    get_maximum(VARS, MAXIMUM),
    MIN in MINIMUM..MAXIMUM,
    MAX in MINIMUM..MAXIMUM,
    minimum(MIN, VARS),
    maximum(MAX, VARS),
    VAR #>= MIN,
    VAR #=< MAX.

% 0: VAR<VARi
% 1: VAR=VARi
% 2: VAR>VARi
between_min_max_a(FLAG, VAR, VARIABLES) :-
    check_type(dvar, VAR),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    between_min_max_signature(VARIABLES, VAR, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,i),
                           arc(s,1,t),
                           arc(s,2,j),
                           arc(i,0,i),
                           arc(i,1,t),
                           arc(i,2,t),
                           arc(j,0,t),
                           arc(j,1,t),
                           arc(j,2,j),
                           arc(t,0,t),
                           arc(t,1,t),
                           arc(t,2,t)],
                           [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

between_min_max_signature([], _, []).
between_min_max_signature([[var-VARi]|VARs], VAR, [S|Ss]) :-
    S in 0..2,
    VAR #< VARi #<=> S #= 0,
    VAR #= VARi #<=> S #= 1,
    VAR #> VARi #<=> S #= 2,
    between_min_max_signature(VARs, VAR, Ss).
