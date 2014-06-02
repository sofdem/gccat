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

ctr_date(no_valley,['20031101','20040530']).

ctr_origin(no_valley, 'Derived from %c.', [valley]).

ctr_arguments(no_valley,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(no_valley,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(no_valley,
                 [size('VARIABLES') > 0    ,
                  required('VARIABLES',var)]).

ctr_typical(no_valley,
            [size('VARIABLES')      > 3,
             range('VARIABLES'^var) > 1]).

ctr_contractible(no_valley, [], 'VARIABLES', any).

ctr_example(no_valley,
            no_valley([[var-1],[var-1],[var-4],[var-8],[var-8],[var-2]])).

ctr_see_also(no_valley,
 [link('implies',            all_equal_valley_min, '',                                              []),
  link('implied by',         decreasing,           '',                                              []),
  link('implied by',         increasing,           '',                                              []),
  link('implied by',         global_contiguity,    '',                                              []),
  link('generalisation',     valley,               'introduce a %e counting the number of valleys', [variable]),
  link('comparison swapped', no_peak,              '',                                              []),
  link('related',            peak,                 '',                                              [])]).

ctr_key_words(no_valley,['sequence'                               ,
                         'automaton'                              ,
                         'automaton without counters'             ,
                         'automaton with same input symbol'       ,
                         'reified automaton constraint'           ,
                         'sliding cyclic(1) constraint network(1)']).

ctr_eval(no_valley, [  checker(no_valley_c),
		     automaton(no_valley_a)]).

ctr_sol(no_valley,2,0,2,9,-).
ctr_sol(no_valley,3,0,3,50,-).
ctr_sol(no_valley,4,0,4,295,-).
ctr_sol(no_valley,5,0,5,1792,-).
ctr_sol(no_valley,6,0,6,11088,-).
ctr_sol(no_valley,7,0,7,69498,-).
ctr_sol(no_valley,8,0,8,439791,-).

no_valley_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N > 0,
    no_valley_c(VARIABLES, 0).

no_valley_c([], _) :- !.
no_valley_c([_], _) :- !.
no_valley_c([[var-X],[var-Y]|R], 0) :-
    X =< Y,
    !,
    no_valley_c([[var-Y]|R], 0).
no_valley_c([_,[var-Y]|R], 0) :-
    !,
    no_valley_c([[var-Y]|R], 1).
no_valley_c([[var-X],[var-Y]|R], 1) :-
    X >= Y,
    no_valley_c([[var-Y]|R], 1).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
no_valley_a(FLAG, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    no_valley_signature(VARIABLES, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE,
                          [source(s),sink(t),sink(s)],
                          [arc(s,0,s),
                           arc(s,1,s),
                           arc(s,2,t),
                           arc(t,1,t),
                           arc(t,2,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

no_valley_signature([], []).
no_valley_signature([_], []) :- !.
no_valley_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss]) :-
    S in 0..2,
    VAR1 #< VAR2 #<=> S #= 0,
    VAR1 #= VAR2 #<=> S #= 1,
    VAR1 #> VAR2 #<=> S #= 2,
    no_valley_signature([[var-VAR2]|VARs], Ss).
