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
    ctr_automaton_signature/3,
    ctr_sol/6,
    ctr_logic/3,
    ctr_application/2.

:-dynamic highest_peak_a/2.

ctr_date(highest_peak,['20040530']).

ctr_origin(highest_peak, 'Derived from %c.', [peak]).

ctr_arguments(highest_peak,
              ['HEIGHT'-dvar                    ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(highest_peak,
                 [items('VARIABLES',reverse)]).

ctr_restrictions(highest_peak,
                 [required('VARIABLES',var)]).

ctr_pure_functional_dependency(highest_peak, []).
ctr_functional_dependency(highest_peak, 1, [2]).

ctr_typical(highest_peak,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 2,
	     peak('VARIABLES'^var)  > 0]).

ctr_example(highest_peak,
            [highest_peak(8,[[var-1],[var-1],[var-4],[var-8],[var-6],[var-2],[var-7],[var-1]]),
	     highest_peak(1,[[var-0],[var-1],[var-1],[var-0],[var-0],[var-1],[var-0],[var-1]])]).

ctr_see_also(highest_peak,
 [link('implies',        between_min_max, '',   []),
  link('common keyword', peak,            '%k', ['sequence']),
  link('common keyword', deepest_valley,  '%k', ['sequence'])]).

ctr_key_words(highest_peak,['sequence'                               ,
                            'automaton'                              ,
                            'automaton with counters'                ,
                            'automaton with same input symbol'       ,
                            'reverse of a constraint'                ,
		            'glue matrix'                            ,
                            'sliding cyclic(1) constraint network(2)',
		            'functional dependency'                  ,
		            'pure functional dependency'             ]).

ctr_eval(highest_peak, [checker(highest_peak_c),
			automaton(highest_peak_a),
			automaton_with_signature(highest_peak_a_s)]).

ctr_sol(highest_peak,2,0,2,9,[-1000000-9]).
ctr_sol(highest_peak,3,0,3,64,[-1000000-50,1-1,2-4,3-9]).
ctr_sol(highest_peak,4,0,4,625,[-1000000-295,1-11,2-44,3-99,4-176]).
ctr_sol(highest_peak,5,0,5,7776,[-1000000-1792,1-92,2-380,3-900,4-1712,5-2900]).
ctr_sol(highest_peak,6,0,6,117649,[-1000000-11088,1-697,2-3000,3-7587,4-15680,5-29125,6-50472]).
ctr_sol(highest_peak,7,0,7,2097152,[-1000000-69498,1-5036,2-22632,3-61389,4-138544,5-283250,6-540576,7-976227]).
ctr_sol(highest_peak,8,0,8,43046721,[-1000000-439791,1-35443,2-166208,3-484020,4-1195056,5-2693425,6-5665896,7-11233250,8-21133632]).

highest_peak_c(HEIGHT, VARIABLES) :-
    check_type(dvar, HEIGHT),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    MININT = -1000000,
    highest_peak_c(VARS, s, MININT, HEIGHT).

highest_peak_c([V1,V2|R], s, C, HEIGHT) :-
    V1 >= V2,
    !,
    highest_peak_c([V2|R], s, C, HEIGHT).
highest_peak_c([_V1,V2|R], s, C, HEIGHT) :-
    !,
    highest_peak_c([V2|R], u, C, HEIGHT).
highest_peak_c([V1,V2|R], u, C, HEIGHT) :-
    V1 =< V2,
    !,
    highest_peak_c([V2|R], u, C, HEIGHT).
highest_peak_c([V1,V2|R], u, C, HEIGHT) :-
    !,
    C1 is max(C,V1),
    highest_peak_c([V2|R], s, C1, HEIGHT).
highest_peak_c([_], _, HEIGHT, HEIGHT) :- !.
highest_peak_c([], _, HEIGHT, HEIGHT).

highest_peak_counters_check([V1,V2|R], s, C, [C|S]) :-
    V1 >= V2,
    !,
    highest_peak_counters_check([V2|R], s, C, S).
highest_peak_counters_check([_V1,V2|R], s, C, [C|S]) :-
    !,
    highest_peak_counters_check([V2|R], u, C, S).
highest_peak_counters_check([V1,V2|R], u, C, [C|S]) :-
    V1 =< V2,
    !,
    highest_peak_counters_check([V2|R], u, C, S).
highest_peak_counters_check([V1,V2|R], u, C, [C1|S]) :-
    !,
    C1 is max(C,V1),
    highest_peak_counters_check([V2|R], s, C1, S).
highest_peak_counters_check([V|R], init, C, [C|S]) :- !,
    highest_peak_counters_check([V|R], s, C, S).
highest_peak_counters_check([_], _, _, []).

ctr_automaton_signature(highest_peak, highest_peak_a, pair_signature(2,signature)).

highest_peak_a(FLAG, HEIGHT, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    highest_peak_a_s(FLAG, HEIGHT, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
highest_peak_a_s(FLAG, HEIGHT, VARIABLES, SIGNATURE) :-
    check_type(dvar, HEIGHT),
    collection(VARIABLES, [dvar]),
    MININT = -1000000,
    pair_first_signature(VARIABLES, VARS),
    automaton(VARS, VAR1,
              SIGNATURE, 
              [source(s),sink(u),sink(s)],
              [arc(s,2,s              ),
               arc(s,1,s              ),
               arc(s,0,u              ),
               arc(u,2,s,[max(C,VAR1)]),
               arc(u,1,u              ),
               arc(u,0,u              )],
              [C],[MININT],[COUNT]),
    COUNT #= HEIGHT #<=> FLAG.
