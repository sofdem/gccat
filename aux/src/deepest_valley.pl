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

:-dynamic deepest_valley_a/2.

ctr_date(deepest_valley,['20040530']).

ctr_origin(deepest_valley, 'Derived from %c.', [valley]).

ctr_arguments(deepest_valley,
              ['DEPTH'-dvar                    ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(deepest_valley,
                 [items('VARIABLES',reverse)]).

ctr_restrictions(deepest_valley,
                 [required('VARIABLES',var)]).

ctr_pure_functional_dependency(deepest_valley, []).
ctr_functional_dependency(deepest_valley, 1, [2]).

ctr_typical(deepest_valley,
            [size('VARIABLES')       > 2,
             range('VARIABLES'^var)  > 2,
	     valley('VARIABLES'^var) > 0]).

ctr_example(deepest_valley,
            [deepest_valley(2,[[var-5],[var-3],[var-4],[var-8],[var-8],[var-2],[var-7],[var-1]]),
	     deepest_valley(7,[[var-1],[var-3],[var-4],[var-8],[var-8],[var-8],[var-7],[var-8]])]).

ctr_see_also(deepest_valley,
 [link('implies',        between_min_max, '',   []),
  link('common keyword', valley,          '%k', [sequence]),
  link('common keyword', highest_peak,    '%k', [sequence])]).

ctr_key_words(deepest_valley,['sequence'                               ,
                              'maxint'                                 ,
                              'automaton'                              ,
                              'automaton with counters'                ,
			      'automaton with same input symbol'       ,
                              'reverse of a constraint'                ,
		              'glue matrix'                            ,
                              'sliding cyclic(1) constraint network(2)',
		              'functional dependency'                  ,
		              'pure functional dependency'             ]).

ctr_eval(deepest_valley, [checker(deepest_valley_c),
			  automaton(deepest_valley_a),
			  automaton_with_signature(deepest_valley_a_s)]).

ctr_sol(deepest_valley,2,0,2,9,[1000000-9]).
ctr_sol(deepest_valley,3,0,3,64,[0-9,1-4,2-1,1000000-50]).
ctr_sol(deepest_valley,4,0,4,625,[0-176,1-99,2-44,3-11,1000000-295]).
ctr_sol(deepest_valley,5,0,5,7776,[0-2900,1-1712,2-900,3-380,4-92,1000000-1792]).
ctr_sol(deepest_valley,6,0,6,117649,[0-50472,1-29125,2-15680,3-7587,4-3000,5-697,1000000-11088]).
ctr_sol(deepest_valley,7,0,7,2097152,[0-976227,1-540576,2-283250,3-138544,4-61389,5-22632,6-5036,1000000-69498]).
ctr_sol(deepest_valley,8,0,8,43046721,[0-21133632,1-11233250,2-5665896,3-2693425,4-1195056,5-484020,6-166208,7-35443,1000000-439791]).

deepest_valley_c(DEPTH, VARIABLES) :-
    check_type(dvar, DEPTH),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    MAXINT = 1000000,
    deepest_valley_c(VARS, s, MAXINT, DEPTH).

deepest_valley_c([V1,V2|R], s, C, DEPTH) :-
    V1 =< V2,
    !,
    deepest_valley_c([V2|R], s, C, DEPTH).
deepest_valley_c([_V1,V2|R], s, C, DEPTH) :-
    !,
    deepest_valley_c([V2|R], u, C, DEPTH).
deepest_valley_c([V1,V2|R], u, C, DEPTH) :-
    V1 >= V2,
    !,
    deepest_valley_c([V2|R], u, C, DEPTH).
deepest_valley_c([V1,V2|R], u, C, DEPTH) :-
    !,
    C1 is min(C,V1),
    deepest_valley_c([V2|R], s, C1, DEPTH).
deepest_valley_c([_], _, DEPTH, DEPTH) :- !.
deepest_valley_c([], _, DEPTH, DEPTH).

deepest_valley_counters_check([V1,V2|R], s, C, [C|S]) :-
    V1 =< V2,
    !,
    deepest_valley_counters_check([V2|R], s, C, S).
deepest_valley_counters_check([_V1,V2|R], s, C, [C|S]) :-
    !,
    deepest_valley_counters_check([V2|R], u, C, S).
deepest_valley_counters_check([V1,V2|R], u, C, [C|S]) :-
    V1 >= V2,
    !,
    deepest_valley_counters_check([V2|R], u, C, S).
deepest_valley_counters_check([V1,V2|R], u, C, [C1|S]) :-
    !,
    C1 is min(C,V1),
    deepest_valley_counters_check([V2|R], s, C1, S).
deepest_valley_counters_check([V|R], init, C, [C|S]) :- !,
    deepest_valley_counters_check([V|R], s, C, S).
deepest_valley_counters_check([_], _, _, []).

ctr_automaton_signature(deepest_valley, deepest_valley_a, pair_signature(2,signature)).

deepest_valley_a(FLAG, DEPTH, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    deepest_valley_a_s(FLAG, DEPTH, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
deepest_valley_a_s(FLAG, DEPTH, VARIABLES, SIGNATURE) :-
    check_type(dvar, DEPTH),
    collection(VARIABLES, [dvar]),
    MAXINT = 1000000,
    pair_first_signature(VARIABLES, VARS),
    automaton(VARS, VAR1,
              SIGNATURE, 
              [source(s),sink(s),sink(u)],
              [arc(s,0,s              ),
               arc(s,1,s              ),
               arc(s,2,u              ),
               arc(u,0,s,[min(C,VAR1)]),
               arc(u,1,u              ),
               arc(u,2,u              )],
              [C],[MAXINT],[COUNT]),
    COUNT #= DEPTH #<=> FLAG.
