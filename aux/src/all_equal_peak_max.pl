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

ctr_date(all_equal_peak_max,['20130107']).

ctr_origin(all_equal_peak_max, 'Derived from %c and %c.', [peak,all_equal]).

ctr_arguments(all_equal_peak_max,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(all_equal_peak_max,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(all_equal_peak_max,
                 [size('VARIABLES') > 0    ,
                  required('VARIABLES',var)]).

ctr_typical(all_equal_peak_max,
            [size('VARIABLES')      >= 5,
             range('VARIABLES'^var) >  1,
             peak('VARIABLES'^var)  >= 2]).

ctr_contractible(all_equal_peak_max, [], 'VARIABLES', prefix).
ctr_contractible(all_equal_peak_max, [], 'VARIABLES', suffix).

ctr_example(all_equal_peak_max,
            all_equal_peak_max([[var-1],[var-5],[var-5],[var-4],[var-3],[var-5],[var-2],[var-5]])).

ctr_cond_imply(all_equal_peak_max, some_equal,    [peak('VARIABLES'^var) > 1], [], id).
ctr_cond_imply(all_equal_peak_max, not_all_equal, [peak('VARIABLES'^var) > 0], [], id).

ctr_see_also(all_equal_peak_max,
 [link('implies',    all_equal_peak,       '', []),
  link('implied by', no_peak,              '', []),
  link('related',    peak,                 '', []),
  link('related',    all_equal_valley_min, '', [])]).

ctr_key_words(all_equal_peak_max,['sequence'                               ,
                                  'automaton'                              ,
				  'automaton with counters'                ,
                                  'automaton with same input symbol'       ,
                                  'sliding cyclic(1) constraint network(2)']).

ctr_eval(all_equal_peak_max, [checker(all_equal_peak_max_c),
		              automaton(all_equal_peak_max_a),
                              automaton_with_signature(all_equal_peak_max_a_s)]).

ctr_sol(all_equal_peak_max,2,0,2,9,-).
ctr_sol(all_equal_peak_max,3,0,3,64,-).
ctr_sol(all_equal_peak_max,4,0,4,605,-).
ctr_sol(all_equal_peak_max,5,0,5,6707,-).
ctr_sol(all_equal_peak_max,6,0,6,81648,-).
ctr_sol(all_equal_peak_max,7,0,7,1065542,-).
ctr_sol(all_equal_peak_max,8,0,8,14829903,-).

all_equal_peak_max_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N > 0,
    get_attr1(VARIABLES, VARS),
    last(VARS, LastVAR),
    all_equal_peak_max_c(s, VARS, LastVAR).

all_equal_peak_max_c(s, [V1,V2|R], A) :-
    V1 >= V2,
    !,
    all_equal_peak_max_c(s, [V2|R], A).
all_equal_peak_max_c(s, [_,V2|R], A) :-
    !,
    all_equal_peak_max_c(i, [V2|R], A).
all_equal_peak_max_c(i, [V1,V2|R], A) :-
    V1 =< V2,
    !,
    all_equal_peak_max_c(i, [V2|R], A).
all_equal_peak_max_c(i, [V1,V2|R], _) :-
    !,
    all_equal_peak_max_c(j, [V2|R], V1).
all_equal_peak_max_c(j, [V1,V2|R], A) :-
    V1 >= V2,
    !,
    all_equal_peak_max_c(j, [V2|R], A).
all_equal_peak_max_c(j, [V1,V2|R], A) :-
    !,
    A >= V1,
    all_equal_peak_max_c(k, [V2|R], A).
all_equal_peak_max_c(k, [V1,V2|R], A) :-
    V1 =< V2,
    !,
    A >= V1,
    all_equal_peak_max_c(k, [V2|R], A).
all_equal_peak_max_c(k, [A,V2|R], A) :-
    !,
    all_equal_peak_max_c(j, [V2|R], A).
all_equal_peak_max_c(_, [LastVAR], A) :-
    A >= LastVAR.

ctr_automaton_signature(all_equal_peak_max, all_equal_peak_max_a, pair_signature(1,signature)).

all_equal_peak_max_a(FLAG, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    all_equal_peak_max_a_s(FLAG, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
all_equal_peak_max_a_s(FLAG, VARIABLES, SIGNATURE) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    pair_first_last_signature(VARIABLES, VARS, LastVAR),
    automaton(VARS, VARi, SIGNATURE, 
              [source(s),sink(i),sink(j),sink(k),sink(s)],
              [arc(s,1,s                                    ),
	       arc(s,2,s                                    ),
               arc(s,0,i                                    ),
               arc(i,0,i                                    ),
	       arc(i,1,i                                    ),
               arc(i,2,j,[VARi,F]                           ),
               arc(j,1,j                                    ),
               arc(j,2,j                                    ),
               arc(j,0,k,(Altitude #>= VARi -> [Altitude,F])),
               arc(j,0,k,(Altitude #<  VARi -> [Altitude,0])),
               arc(k,0,k,(Altitude #>= VARi -> [Altitude,F])),
               arc(k,0,k,(Altitude #<  VARi -> [Altitude,0])),
	       arc(k,1,k,(Altitude #>= VARi -> [Altitude,F])),
	       arc(k,1,k,(Altitude #<  VARi -> [Altitude,0])),
	       arc(k,2,j,(Altitude #=  VARi -> [Altitude,F])),
	       arc(k,2,j,(Altitude #\= VARi -> [Altitude,0]))],
               [Altitude,F],[LastVAR,1],[A,FL]),
               FLAG #<=> FL #/\ A #>= LastVAR.
