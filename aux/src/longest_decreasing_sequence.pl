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

:-dynamic longest_decreasing_sequence_a/3.

ctr_date(longest_decreasing_sequence,['20121124']).

ctr_origin(longest_decreasing_sequence, 'constraint on sequences', []).

ctr_arguments(longest_decreasing_sequence,
              ['L'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(longest_decreasing_sequence,
                 [translate(['VARIABLES'^var])]).

ctr_synonyms(longest_decreasing_sequence,[size_longest_decreasing_sequence]).

ctr_restrictions(longest_decreasing_sequence,
                 ['L' >=  0                    ,
                  'L' <  range('VARIABLES'^var),
                  required('VARIABLES',var)    ]).

ctr_typical(longest_decreasing_sequence,
            ['L'                   > 0,
             size('VARIABLES')     > 1,
	     nval('VARIABLES'^var) > 2]).

ctr_pure_functional_dependency(longest_decreasing_sequence, []).
ctr_functional_dependency(longest_decreasing_sequence, 1, [2]).

ctr_example(longest_decreasing_sequence,
            [longest_decreasing_sequence(0, [[var-0],[var-1],[var-2],[var-5]]),
             longest_decreasing_sequence(0, [[var-8],[var-8]]),
             longest_decreasing_sequence(6, [[var-10],[var-8],[var-8],[var-6],[var-4],[var-9],[var-10],[var-8]])]).

ctr_see_also(longest_decreasing_sequence,
 [link('common keyword', longest_increasing_sequence, '%k', [sequence]),
  link('common keyword', min_dist_between_inflexion,  '%k', [sequence])]).

ctr_key_words(longest_decreasing_sequence,['sequence'                        ,
                                           'automaton'                       ,
                                           'automaton with counters'         ,
                                           'automaton with same input symbol',
                                           'reverse of a constraint'         ,
					   'glue matrix'                     ,
                                           'functional dependency'           ,
		                           'pure functional dependency'      ]).

ctr_eval(longest_decreasing_sequence, [checker(longest_decreasing_sequence_c),
				       automaton(longest_decreasing_sequence_a)]).

ctr_sol(longest_decreasing_sequence,2,0,2,9,[0-6,1-2,2-1]).
ctr_sol(longest_decreasing_sequence,3,0,3,64,[0-20,1-18,2-16,3-10]).
ctr_sol(longest_decreasing_sequence,4,0,4,625,[0-70,1-122,2-161,3-162,4-110]).
ctr_sol(longest_decreasing_sequence,5,0,5,7776,[0-252,1-750,2-1398,3-1942,4-2024,5-1410]).
ctr_sol(longest_decreasing_sequence,6,0,6,117649,[0-924,1-4412,2-11361,3-20816,4-28930,5-30134,6-21072]).
ctr_sol(longest_decreasing_sequence,7,0,7,2097152,[0-3432,1-25382,2-89132,3-211106,4-375084,5-506766,6-522648,7-363602]).
ctr_sol(longest_decreasing_sequence,8,0,8,43046721,[0-12870,1-144314,2-685090,3-2074365,4-4603682,5-7792840,6-10197174,7-10379696,8-7156690]).

longest_decreasing_sequence_c(0, []) :- !.
longest_decreasing_sequence_c(L, VARIABLES) :-
    check_type(dvar, L),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    longest_decreasing_sequence_c(VARS, s, 0, 0, 0, L).

longest_decreasing_sequence_c([V|R], s, _, _, Max, L) :- !,
    longest_decreasing_sequence_c(R, t, V, V, Max, L).
longest_decreasing_sequence_c([V|R], t, _, Last, Max, L) :-
    Last < V,
    !,
    longest_decreasing_sequence_c(R, t, V, V, Max, L).
longest_decreasing_sequence_c([V|R], t, First, _, Max, L) :-
    !,
    Max1 is max(Max,First-V),
    longest_decreasing_sequence_c(R, t, First, V, Max1, L).
longest_decreasing_sequence_c([], _, _, _, L, L). % fix L if not initially fixed (used in the context of learning)

longest_decreasing_sequence_counters_check([V|R], s, _, _, Max, [Max|S]) :- !,
    longest_decreasing_sequence_counters_check(R, t, V, V, Max, S).
longest_decreasing_sequence_counters_check([V|R], t, _, Last, Max, [Max|S]) :-
    Last < V,
    !,
    longest_decreasing_sequence_counters_check(R, t, V, V, Max, S).
longest_decreasing_sequence_counters_check([V|R], t, First, _, Max, [Max1|S]) :-
    !,
    Max1 is max(Max,First-V),
    longest_decreasing_sequence_counters_check(R, t, First, V, Max1, S).
longest_decreasing_sequence_counters_check([], _, _, _, _, []).

% 0: VAR1 < VAR2
% 1: VAR1 = VAR2
% 2: VAR1 > VAR2
longest_decreasing_sequence_a(FLAG, L, VARIABLES) :-
    check_type(dvar, L),
    length(VARIABLES, N),
    (N = 0 ->
	Max = 0
    ; 
	collection(VARIABLES, [dvar]),
	longest_decreasing_sequence_signature(VARIABLES, SIGNATURE, DIFFERENCES),
	automaton(DIFFERENCES, Di, SIGNATURE,
                  [source(s),sink(s),sink(t)],
		  [arc(s,0,s                   ),
		   arc(s,1,s                   ),
		   arc(s,2,t,[max(M,Di),Di    ]),
                   arc(t,0,s                   ),
                   arc(t,1,t                   ),
		   arc(t,2,t,[max(M,C+Di),C+Di])],
		  [M,C],[0,0],[Max,_])
    ),
    Max #= L #<=> FLAG.

longest_decreasing_sequence_signature([_], [], []) :- !.
longest_decreasing_sequence_signature([[var-VAR1],[var-VAR2]|VARs], [S|RS], [DIFFERENCE|RD]) :-
    VAR1 #< VAR2 #<=> S #= 0,
    VAR1 #= VAR2 #<=> S #= 1,
    VAR1 #> VAR2 #<=> S #= 2,
    VAR1 #= DIFFERENCE + VAR2,
    longest_decreasing_sequence_signature([[var-VAR2]|VARs], RS, RD).
