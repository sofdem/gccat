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

ctr_date(consecutive_groups_of_ones,['20091227']).

ctr_origin(consecutive_groups_of_ones, 'Derived from %c', [group]).

ctr_arguments(consecutive_groups_of_ones,
              ['GROUP_SIZES'-collection(nb-int),
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(consecutive_groups_of_ones,
                 [items_sync('GROUP_SIZES','VARIABLES',reverse)]).

ctr_restrictions(consecutive_groups_of_ones,
                 [required('GROUP_SIZES',nb)                                          ,
                  size('GROUP_SIZES') >= 1                                            ,
                  'GROUP_SIZES'^nb >= 1                                               ,
                  'GROUP_SIZES'^nb =< size('VARIABLES')                               ,
                  required('VARIABLES',var)                                           ,
                  size('VARIABLES') >= 2*size('GROUP_SIZES') - 1                      ,
                  size('VARIABLES') >= sum('GROUP_SIZES'^nb) + size('GROUP_SIZES') - 1,
                  'VARIABLES'^var   >= 0                                              ,
                  'VARIABLES'^var   =< 1                                              ]).

ctr_typical(consecutive_groups_of_ones,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_example(consecutive_groups_of_ones,
            consecutive_groups_of_ones([[nb-2],[nb-1]],
                                       [[var-1],[var-1],[var-0],[var-0],[var-0],[var-1],[var-0]])).

ctr_see_also(consecutive_groups_of_ones,
 [link('root concept', group, '', [])]).

ctr_key_words(consecutive_groups_of_ones,['Berge-acyclic constraint network',
                                          'automaton'                       ,
                                          'automaton without counters'      ,
                                          'reified automaton constraint'    ,
                                          'arc-consistency'                 ,
                                          'logigraphe'                      ]).

ctr_eval(consecutive_groups_of_ones, [automaton(consecutive_groups_of_ones_a)]).

% 0: VAR=0
% 1: VAR=1
consecutive_groups_of_ones_a(FLAG, GROUP_SIZES, VARIABLES) :-
    collection(VARIABLES, [dvar(0,1)]),
    length(VARIABLES, N),
    collection(GROUP_SIZES, [int(1,N)]),
    length(GROUP_SIZES, M),
    M >= 1,
    N >= M,
    N >= 2*M - 1,
    get_attr1(GROUP_SIZES, SIZES),
    get_attr1(VARIABLES, VARS),
    get_sum(SIZES, S),
    N >= S + M - 1,
    consecutive_groups_of_ones_transitions(SIZES, -1, TRANSITIONS, LAST),
    AUTOMATON = automaton(VARS, _, VARS, [source(0),sink(LAST)], TRANSITIONS, [], [], []),
    automaton_bool(FLAG, [0,1], AUTOMATON).

consecutive_groups_of_ones_transitions([], P, [arc(P,0,P)], P).
consecutive_groups_of_ones_transitions([N|R], P, L, Last) :-
    P1 is P+1,
    PN is N+P1,
    (P >= 0 -> L1 = [arc(P,0,P1),arc(P1,0,P1)] ; L1 = [arc(P1,0,P1)]),
    consecutive_groups_of_ones_trans(N, P1, L2),
    consecutive_groups_of_ones_transitions(R, PN, L3, Last),
    append(L1, L2, L12),
    append(L12, L3, L).

consecutive_groups_of_ones_trans(0, _, []) :-
    !.
consecutive_groups_of_ones_trans(I, P, [arc(P,1,P1)|R]) :-
    I > 0,
    P1 is P+1,
    I1 is I-1,
    consecutive_groups_of_ones_trans(I1, P1, R).
