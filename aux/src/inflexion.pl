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

ctr_date(inflexion,['20000128','20030820','20040530']).

ctr_origin(inflexion, 'N.~Beldiceanu', []).

ctr_arguments(inflexion,
              ['N'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(inflexion,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(inflexion,
                 ['N' >= 0                         ,
                  'N' =< max(0,size('VARIABLES')-2),
                  required('VARIABLES',var)        ]).

ctr_pure_functional_dependency(inflexion, []).
ctr_functional_dependency(inflexion, 1, [2]).

ctr_typical(inflexion,
            ['N'                    > 0,
             size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1]).

ctr_example(inflexion,
            [inflexion(3,[[var-1],[var-1],[var-4],[var-8],[var-8],[var-2],[var-7],[var-1]]),
	     inflexion(0,[[var-1],[var-1],[var-4],[var-4],[var-6],[var-6],[var-7],[var-9]]),
	     inflexion(7,[[var-1],[var-0],[var-2],[var-0],[var-7],[var-2],[var-7],[var-1],[var-2]])]).

ctr_cond_imply(inflexion, atleast_nvalue, ['N' > 0],                     ['NVAL' = 2], [none,'VARIABLES']).
ctr_cond_imply(inflexion, peak,           [valley('VARIABLES'^var) = 0], [],           id).
ctr_cond_imply(inflexion, valley,         [peak('VARIABLES'^var)   = 0], [],           id).

ctr_see_also(inflexion,
 [link('common keyword', min_dist_between_inflexion, '%k', [sequence]),
  link('common keyword', global_contiguity,          '%k', [sequence]),
  link('common keyword', peak,                       '%k', [sequence]),
  link('common keyword', valley,                     '%k', [sequence])]).

ctr_key_words(inflexion,['sequence'                               ,
                         'automaton'                              ,
                         'automaton with counters'                ,
                         'automaton with same input symbol'       ,
                         'reverse of a constraint'                ,
		         'glue matrix'                            ,
                         'sliding cyclic(1) constraint network(2)',
			 'functional dependency'                  ,
		         'pure functional dependency'             ]).

ctr_persons(inflexion,['Beldiceanu N.']).

ctr_eval(inflexion, [checker(inflexion_c),
		     automaton(inflexion_a),
		     automaton_with_signature(inflexion_a_s)]).

ctr_sol(inflexion,2,0,2,9,[0-9]).
ctr_sol(inflexion,3,0,3,64,[0-36,1-28]).
ctr_sol(inflexion,4,0,4,625,[0-135,1-320,2-170]).
ctr_sol(inflexion,5,0,5,7776,[0-498,1-2588,2-3348,3-1342]).
ctr_sol(inflexion,6,0,6,117649,[0-1841,1-18494,2-44058,3-40446,4-12810]).
ctr_sol(inflexion,7,0,7,2097152,[0-6856,1-125284,2-492320,3-778936,4-549152,5-144604]).
ctr_sol(inflexion,8,0,8,43046721,[0-25731,1-828120,2-5069970,3-12341184,4-14547186,5-8354520,6-1880010]).

inflexion_c(N, VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, L),
    MAX is max(0,L-2),
    check_type(dvar(0,MAX), N),
    get_attr1(VARIABLES, VARS),
    inflexion_c(s, VARS, 0, N).

inflexion_c(s, [V,V|R], C, N) :-
    !,
    inflexion_c(s, [V|R], C, N).
inflexion_c(s, [V1,V2|R], C, N) :-
    V1 < V2,
    !,
    inflexion_c(i, [V2|R], C, N).
inflexion_c(s, [_,V2|R], C, N) :-
    !,
    inflexion_c(j, [V2|R], C, N).
inflexion_c(i, [V1,V2|R], C, N) :-
    V1 =< V2,
    !,
    inflexion_c(i, [V2|R], C, N).
inflexion_c(i, [_,V2|R], C, N) :-
    !,
    C1 is C+1,
    inflexion_c(j, [V2|R], C1, N).
inflexion_c(j, [V1,V2|R], C, N) :-
    V1 >= V2,
    !,
    inflexion_c(j, [V2|R], C, N).
inflexion_c(j, [_,V2|R], C, N) :-
    !,
    C1 is C+1,
    inflexion_c(i, [V2|R], C1, N).
inflexion_c(_, [_], N, N) :- !.
inflexion_c(_, [], N, N).

inflexion_counters_check(s, [V,V|R], C, [C|S]) :-
    !,
    inflexion_counters_check(s, [V|R], C, S).
inflexion_counters_check(s, [V1,V2|R], C, [C|S]) :-
    V1 < V2,
    !,
    inflexion_counters_check(i, [V2|R], C, S).
inflexion_counters_check(s, [_,V2|R], C, [C|S]) :-
    !,
    inflexion_counters_check(j, [V2|R], C, S).
inflexion_counters_check(i, [V1,V2|R], C, [C|S]) :-
    V1 =< V2,
    !,
    inflexion_counters_check(i, [V2|R], C, S).
inflexion_counters_check(i, [_,V2|R], C, [C1|S]) :-
    !,
    C1 is C+1,
    inflexion_counters_check(j, [V2|R], C1, S).
inflexion_counters_check(j, [V1,V2|R], C, [C|S]) :-
    V1 >= V2,
    !,
    inflexion_counters_check(j, [V2|R], C, S).
inflexion_counters_check(j, [_,V2|R], C, [C1|S]) :-
    !,
    C1 is C+1,
    inflexion_counters_check(i, [V2|R], C1, S).
inflexion_counters_check(init, [V|R], C, [0|S]) :- !,
    inflexion_counters_check(s, [V|R], C, S).
inflexion_counters_check(_, [_], _, []).

ctr_automaton_signature(inflexion, inflexion_a, pair_signature(2,signature)).

inflexion_a(FLAG, N, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    inflexion_a_s(FLAG, N, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
inflexion_a_s(FLAG, N, VARIABLES, SIGNATURE) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    MAX is max(0,L-2),
    check_type(dvar(0,MAX), N),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(j),sink(s)],
              [arc(s,1,s      ),
               arc(s,0,i      ),
               arc(s,2,j      ),
               arc(i,1,i      ),
               arc(i,0,i      ),
               arc(i,2,j,[C+1]),
               arc(j,1,j      ),
               arc(j,2,j      ),
               arc(j,0,i,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= N #<=> FLAG.
