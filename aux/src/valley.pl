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

ctr_date(valley,['20040530']).

ctr_origin(valley, 'Derived from %c.', [inflexion]).

ctr_arguments(valley,
              ['N'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(valley,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(valley,
                 ['N'   >= 0                         ,
                  2*'N' =< max(size('VARIABLES')-1,0),
                  required('VARIABLES',var)          ]).

ctr_pure_functional_dependency(valley, []).
ctr_functional_dependency(valley, 1, [2]).

ctr_typical(valley,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1]).

ctr_contractible(valley, ['N'=0], 'VARIABLES', any).

ctr_example(valley,
            [valley(1,[[var-1],[var-1],[var-4],[var-8],[var-8],[var-2],[var-7],[var-1]]),
             valley(0,[[var-1],[var-1],[var-4],[var-5],[var-8],[var-8],[var-4],[var-1]]),
	     valley(4,[[var-1],[var-0],[var-4],[var-0],[var-8],[var-2],[var-4],[var-1],[var-2]])]).

ctr_cond_imply(valley, atleast_nvalue, ['N' > 0], ['NVAL' = 2], [none,'VARIABLES']).
ctr_cond_imply(valley, inflexion, [], ['N' = peak('VARIABLES'^var)+valley('VARIABLES'^var)], [none,'VARIABLES']).

ctr_see_also(valley,
 [link('generalisation',     big_valley,                 'a tolerance parameter is added for counting only big valleys',         []),
  link('specialisation',     no_valley,                  'the variable counting the number of valleys is set to %e and removed', [0]),
  link('comparison swapped', peak,                       '',                                                                     []),
  link('common keyword',     deepest_valley,             '%k',                                                                   [sequence]),
  link('common keyword',     inflexion,                  '%k',                                                                   [sequence]),
  link('common keyword',     min_dist_between_inflexion, '%k',                                                                   [sequence]),
  link('common keyword',     min_width_valley,           '%k',                                                                   [sequence]),
  link('related',            no_peak,                    '',                                                                     []),
  link('related',            all_equal_valley,           '',                                                                     []),
  link('related',            decreasing_valley,          '',                                                                     []),
  link('related',            increasing_valley,          '',                                                                     []),
  link('related',            all_equal_valley_min,       '',                                                                     [])]).

ctr_key_words(valley,['sequence'                               ,
                      'automaton'                              ,
                      'automaton with counters'                ,
                      'automaton with same input symbol'       ,
                      'reverse of a constraint'                ,
		      'glue matrix'                            ,
                      'sliding cyclic(1) constraint network(2)',
		      'functional dependency'                  ,
		      'pure functional dependency'             ]).

ctr_eval(valley, [checker(valley_c),
		  automaton(valley_a),
		  automaton_with_signature(valley_a_s)]).

ctr_sol(valley,2,0,2,9,[0-9]).
ctr_sol(valley,3,0,3,64,[0-50,1-14]).
ctr_sol(valley,4,0,4,625,[0-295,1-330]).
ctr_sol(valley,5,0,5,7776,[0-1792,1-5313,2-671]).
ctr_sol(valley,6,0,6,117649,[0-11088,1-73528,2-33033]).
ctr_sol(valley,7,0,7,2097152,[0-69498,1-944430,2-1010922,3-72302]).
ctr_sol(valley,8,0,8,43046721,[0-439791,1-11654622,2-24895038,3-6057270]).

valley_c(N, VARIABLES) :-
    check_type(dvar_gteq(0), N),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    valley_c(VARS, s, 0, N).

valley_c([V1,V2|R], s, C, N) :-
    V1 =< V2,
    !,
    valley_c([V2|R], s, C, N).
valley_c([_V1,V2|R], s, C, N) :-
    !,
    valley_c([V2|R], u, C, N).
valley_c([V1,V2|R], u, C, N) :-
    V1 >= V2,
    !,
    valley_c([V2|R], u, C, N).
valley_c([_V1,V2|R], u, C, N) :-
    !,
    C1 is C+1,
    valley_c([V2|R], s, C1, N).
valley_c([_], _, N, N) :- !.
valley_c([], _, N, N).

valley_counters_check([V1,V2|R], s, C, [C|S]) :-
    V1 =< V2,
    !,
    valley_counters_check([V2|R], s, C, S).
valley_counters_check([_V1,V2|R], s, C, [C|S]) :-
    !,
    valley_counters_check([V2|R], u, C, S).
valley_counters_check([V1,V2|R], u, C, [C|S]) :-
    V1 >= V2,
    !,
    valley_counters_check([V2|R], u, C, S).
valley_counters_check([_V1,V2|R], u, C, [C1|S]) :-
    !,
    C1 is C+1,
    valley_counters_check([V2|R], s, C1, S).
valley_counters_check([V|R], init, C, [0|S]) :- !,
    valley_counters_check([V|R], s, C, S).
valley_counters_check([_], _, _, []).

ctr_automaton_signature(valley, valley_a, pair_signature(2,signature)).

valley_a(FLAG, N, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    valley_a_s(FLAG, N, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
valley_a_s(FLAG, N, VARIABLES, SIGNATURE) :-
    check_type(dvar_gteq(0), N),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    MAX is max(L-1,0),
    2*N #=< MAX,
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(u),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s      ),
               arc(s,2,u      ),
               arc(u,0,s,[C+1]),
               arc(u,1,u      ),
               arc(u,2,u      )],
              [C],[0],[COUNT]),
    COUNT #= N #<=> FLAG.
