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

:-dynamic decreasing_peak_a/2.

ctr_date(decreasing_peak,['20130209']).

ctr_origin(decreasing_peak, 'Derived from %c and %c.', [peak,decreasing]).

ctr_arguments(decreasing_peak,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(decreasing_peak,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(decreasing_peak,
                 [size('VARIABLES') > 0    ,
                  required('VARIABLES',var)]).

ctr_typical(decreasing_peak,
            [size('VARIABLES')      >= 7,
             range('VARIABLES'^var) >  1,
             peak('VARIABLES'^var)  >= 3]).

ctr_contractible(decreasing_peak, [], 'VARIABLES', prefix).
ctr_contractible(decreasing_peak, [], 'VARIABLES', suffix).

ctr_example(decreasing_peak,
            decreasing_peak([[var-1],[var-7],[var-7],[var-4],[var-3],[var-7],[var-2],[var-2],[var-5],[var-4]])).

ctr_cond_imply(decreasing_peak, not_all_equal, [peak('VARIABLES'^var) > 0], [], id).

ctr_see_also(decreasing_peak,
 [link('implied by', all_equal_peak,  '', []),
  link('related',    peak,            '', []),
  link('related',    increasing_peak, '', [])]).

ctr_key_words(decreasing_peak,['sequence'                               ,
                               'automaton'                              ,
                               'automaton with counters'                ,
			       'automaton with same input symbol'       ,
                               'sliding cyclic(1) constraint network(2)']).

ctr_eval(decreasing_peak, [checker(decreasing_peak_c),
	   	           automaton(decreasing_peak_a),
			   automaton_with_signature(decreasing_peak_a_s)]).

ctr_sol(decreasing_peak,2,0,2,9,-).
ctr_sol(decreasing_peak,3,0,3,64,-).
ctr_sol(decreasing_peak,4,0,4,625,-).
ctr_sol(decreasing_peak,5,0,5,7553,-).
ctr_sol(decreasing_peak,6,0,6,105798,-).
ctr_sol(decreasing_peak,7,0,7,1666878,-).
ctr_sol(decreasing_peak,8,0,8,29090469,-).

decreasing_peak_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, L),
    L > 0,
    get_attr1(VARIABLES, VARS),
    decreasing_peak_c(VARS, s, 0).

decreasing_peak_c([V1,V2|R], s, A) :-
    V1 >= V2,
    !,
    decreasing_peak_c([V2|R], s, A).
decreasing_peak_c([_,V2|R], s, A) :-
    !,
    decreasing_peak_c([V2|R], u, A).
decreasing_peak_c([V1,V2|R], u, A) :-
    V1 =< V2,
    !,
    decreasing_peak_c([V2|R], u, A).
decreasing_peak_c([V1,V2|R], u, _) :-
    !,
    decreasing_peak_c([V2|R], v, V1).
decreasing_peak_c([V1,V2|R], v, A) :-
    V1 >= V2,
    !,
    decreasing_peak_c([V2|R], v, A).
decreasing_peak_c([_,V2|R], v, A) :-
    !,
    decreasing_peak_c([V2|R], w, A).
decreasing_peak_c([V1,V2|R], w, A) :-
    V1 =< V2,
    !,
    decreasing_peak_c([V2|R], w, A).
decreasing_peak_c([V1,V2|R], w, A) :-
    !,
    A >= V1,
    decreasing_peak_c([V2|R], v, V1).
decreasing_peak_c([_], _, _) :- !.
decreasing_peak_c([], _, _).

ctr_automaton_signature(decreasing_peak, decreasing_peak_a, pair_signature(1,signature)).

decreasing_peak_a(FLAG, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    decreasing_peak_a_s(FLAG, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
decreasing_peak_a_s(FLAG, VARIABLES, SIGNATURE) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    L >= 0,
    pair_first_signature(VARIABLES, VARS),
    automaton(VARS, Vi, SIGNATURE, 
              [source(s),sink(s),sink(u),sink(v),sink(w)],
              [arc(s,2,s                     ),
               arc(s,1,s                     ),
               arc(s,0,u                     ),
               arc(u,2,v,[Vi,F]              ),
               arc(u,1,u                     ),
               arc(u,0,u                     ),
	       arc(v,2,v                     ),
               arc(v,1,v                     ),
               arc(v,0,w                     ),
               arc(w,2,v,(A #>= Vi -> [Vi,F])),
               arc(w,2,v,(A #<  Vi -> [A ,0])),
               arc(w,1,w                     ),
               arc(w,0,w                     )],
              [A,F],[0,1],[_A,FLAG]).
