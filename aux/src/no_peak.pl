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

ctr_date(no_peak,['20031101','20040530']).

ctr_origin(no_peak, 'Derived from %c.', [peak]).

ctr_arguments(no_peak,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(no_peak,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(no_peak,
                 [size('VARIABLES') > 0    ,
                  required('VARIABLES',var)]).

ctr_typical(no_peak,
            [size('VARIABLES')      > 3,
             range('VARIABLES'^var) > 1]).

ctr_contractible(no_peak, [], 'VARIABLES', any).

ctr_example(no_peak,
            no_peak([[var-1],[var-1],[var-4],[var-8],[var-8]])).

ctr_see_also(no_peak,
 [link('implies',            all_equal_peak_max,   '',                                            []        ),
  link('implied by',         decreasing,           '',                                            []        ),
  link('implied by',         increasing,           '',                                            []        ),
  link('generalisation',     peak,                 'introduce a %e counting the number of peaks', [variable]),
  link('comparison swapped', no_valley,            '',                                            []        ),
  link('related',            valley,               '',                                            []        )]).

ctr_key_words(no_peak,['sequence'                               ,
                       'automaton'                              ,
                       'automaton without counters'             ,
                       'automaton with same input symbol'       ,
                       'reified automaton constraint'           ,
                       'sliding cyclic(1) constraint network(1)']).

ctr_eval(no_peak, [  checker(no_peak_c),
		   automaton(no_peak_a)]).

ctr_sol(no_peak,2,0,2,9,-).
ctr_sol(no_peak,3,0,3,50,-).
ctr_sol(no_peak,4,0,4,295,-).
ctr_sol(no_peak,5,0,5,1792,-).
ctr_sol(no_peak,6,0,6,11088,-).
ctr_sol(no_peak,7,0,7,69498,-).
ctr_sol(no_peak,8,0,8,439791,-).

no_peak_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N > 0,
    no_peak_c(VARIABLES, 0).

no_peak_c([], _) :- !.
no_peak_c([_], _) :- !.
no_peak_c([[var-X],[var-Y]|R], 0) :-
    X >= Y,
    !,
    no_peak_c([[var-Y]|R], 0).
no_peak_c([_,[var-Y]|R], 0) :-
    !,
    no_peak_c([[var-Y]|R], 1).
no_peak_c([[var-X],[var-Y]|R], 1) :-
    X =< Y,
    no_peak_c([[var-Y]|R], 1).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
no_peak_a(FLAG, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    no_peak_signature(VARIABLES, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE,
                          [source(s),sink(t),sink(s)],
                          [arc(s,1,s),
                           arc(s,2,s),
                           arc(s,0,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

no_peak_signature([], []).
no_peak_signature([_], []) :- !.
no_peak_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss]) :-
    S in 0..2,
    VAR1 #< VAR2 #<=> S #= 0,
    VAR1 #= VAR2 #<=> S #= 1,
    VAR1 #> VAR2 #<=> S #= 2,
    no_peak_signature([[var-VAR2]|VARs], Ss).
