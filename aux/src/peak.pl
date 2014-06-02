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

ctr_date(peak,['20040530']).

ctr_origin(peak, 'Derived from %c.', [inflexion]).

ctr_arguments(peak,
              ['N'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(peak,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(peak,
                 ['N'   >= 0                         ,
                  2*'N' =< max(size('VARIABLES')-1,0),
                  required('VARIABLES',var)          ]).

ctr_pure_functional_dependency(peak, []).
ctr_functional_dependency(peak, 1, [2]).

ctr_typical(peak,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1]).

ctr_contractible(peak, ['N'=0], 'VARIABLES', any).

ctr_example(peak,
            [peak(2,[[var-1],[var-1],[var-4],[var-8],[var-6],[var-2],[var-7],[var-1]]),
	     peak(0,[[var-1],[var-1],[var-4],[var-4],[var-4],[var-6],[var-7],[var-7]]),
	     peak(4,[[var-1],[var-5],[var-4],[var-9],[var-4],[var-6],[var-2],[var-7],[var-6]])]).

ctr_cond_imply(peak, atleast_nvalue, ['N' > 0], ['NVAL' = 2], [none,'VARIABLES']).
ctr_cond_imply(peak, inflexion, [], ['N' = peak('VARIABLES'^var)+valley('VARIABLES'^var)], [none,'VARIABLES']).

ctr_see_also(peak,
 [link('generalisation',     big_peak,                   'a tolerance parameter is added for counting only big peaks',         []),
  link('specialisation',     no_peak,                    'the variable counting the number of peaks is set to %e and removed', [0]),
  link('comparison swapped', valley,                     '',                                                                   []),
  link('common keyword',     highest_peak,               '%k',                                                                 [sequence]),
  link('common keyword',     inflexion,                  '%k',                                                                 [sequence]),
  link('common keyword',     min_dist_between_inflexion, '%k',                                                                 [sequence]),
  link('common keyword',     min_width_peak,             '%k',                                                                 [sequence]),
  link('related',            no_valley,                  '',                                                                   []),
  link('related',            all_equal_peak,             '',                                                                   []),
  link('related',            decreasing_peak,            '',                                                                   []),
  link('related',            increasing_peak,            '',                                                                   []),
  link('related',            all_equal_peak_max,         '',                                                                   [])]).

ctr_key_words(peak,['sequence'                               ,
                    'automaton'                              ,
                    'automaton with counters'                ,
                    'automaton with same input symbol'       ,
                    'reverse of a constraint'                ,
		    'glue matrix'                            ,
                    'sliding cyclic(1) constraint network(2)',
		    'functional dependency'                  ,
		    'pure functional dependency'             ]).

ctr_eval(peak, [checker(peak_c),
		automaton(peak_a),
		automaton_with_signature(peak_a_s)]).

ctr_sol(peak,2,0,2,9,[0-9]).
ctr_sol(peak,3,0,3,64,[0-50,1-14]).
ctr_sol(peak,4,0,4,625,[0-295,1-330]).
ctr_sol(peak,5,0,5,7776,[0-1792,1-5313,2-671]).
ctr_sol(peak,6,0,6,117649,[0-11088,1-73528,2-33033]).
ctr_sol(peak,7,0,7,2097152,[0-69498,1-944430,2-1010922,3-72302]).
ctr_sol(peak,8,0,8,43046721,[0-439791,1-11654622,2-24895038,3-6057270]).

peak_c(N, VARIABLES) :-
    check_type(dvar_gteq(0), N),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    peak_c(VARS, s, 0, N).

peak_c([V1,V2|R], s, C, N) :-
    V1 >= V2,
    !,
    peak_c([V2|R], s, C, N).
peak_c([_V1,V2|R], s, C, N) :-
    !,
    peak_c([V2|R], u, C, N).
peak_c([V1,V2|R], u, C, N) :-
    V1 =< V2,
    !,
    peak_c([V2|R], u, C, N).
peak_c([_V1,V2|R], u, C, N) :-
    !,
    C1 is C+1,
    peak_c([V2|R], s, C1, N).
peak_c([_], _, N, N) :- !.
peak_c([], _, N, N).

peak_counters_check([V1,V2|R], s, C, [C|S]) :-
    V1 >= V2,
    !,
    peak_counters_check([V2|R], s, C, S).
peak_counters_check([_V1,V2|R], s, C, [C|S]) :-
    !,
    peak_counters_check([V2|R], u, C, S).
peak_counters_check([V1,V2|R], u, C, [C|S]) :-
    V1 =< V2,
    !,
    peak_counters_check([V2|R], u, C, S).
peak_counters_check([_V1,V2|R], u, C, [C1|S]) :-
    !,
    C1 is C+1,
    peak_counters_check([V2|R], s, C1, S).
peak_counters_check([V|R], init, C, [0|S]) :- !,
    peak_counters_check([V|R], s, C, S).
peak_counters_check([_], _, _, []).

ctr_automaton_signature(peak, peak_a, pair_signature(2,signature)).

peak_a(FLAG, N, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    peak_a_s(FLAG, N, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
peak_a_s(FLAG, N, VARIABLES, SIGNATURE) :-
    check_type(dvar_gteq(0), N),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    MAX is max(L-1,0),
    2*N #=< MAX, 
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(u),sink(s)],
              [arc(s,2,s      ),
               arc(s,1,s      ),
               arc(s,0,u      ),
               arc(u,2,s,[C+1]),
               arc(u,1,u      ),
               arc(u,0,u      )],
              [C],[0],[COUNT]),
    COUNT #= N #<=> FLAG.
