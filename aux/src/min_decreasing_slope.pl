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

ctr_date(min_decreasing_slope,['20130317']).

ctr_origin(min_decreasing_slope, 'Motivated by time series.', []).

ctr_arguments(min_decreasing_slope,
              ['MIN'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(min_decreasing_slope,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(min_decreasing_slope,
                 ['MIN' >= 0                    ,
                  'MIN' < range('VARIABLES'^var),
                  required('VARIABLES',var)     ,
		  size('VARIABLES') > 0         ]).

ctr_pure_functional_dependency(min_decreasing_slope, []).
ctr_functional_dependency(min_decreasing_slope, 1, [2]).

ctr_typical(min_decreasing_slope,
            ['MIN'                  > 1,
	     size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 2]).

ctr_example(min_decreasing_slope,
            [min_decreasing_slope(2,[[var-1],[var-1],[var-5],[var-8],[var-6],[var-2],[var-4],[var-1],[var-5]]),
	     min_decreasing_slope(0,[[var-1],[var-1],[var-1],[var-3],[var-4],[var-7],[var-7],[var-7],[var-9]]),
	     min_decreasing_slope(9,[[var-1],[var-1],[var-9],[var-0],[var-4],[var-7],[var-7],[var-7],[var-9]])]).

ctr_cond_imply(min_decreasing_slope, max_decreasing_slope,
	       [range('VARIABLES'^var) = 'MIN' + 1],
	       [range('VARIABLES'^var) = 'MAX' + 1],
	       [none,'VARIABLES']).

ctr_key_words(min_decreasing_slope,['sequence'                  ,
                                    'automaton'                 ,
                                    'automaton with counters'   ,
                                    'reverse of a constraint'   ,
		                    'glue matrix'               ,
		                    'functional dependency'     ,
		                    'pure functional dependency']).

ctr_eval(min_decreasing_slope, [checker(min_decreasing_slope_c),
		                automaton(min_decreasing_slope_a),
				automaton_with_signature(min_decreasing_slope_a_s)]).

ctr_sol(min_decreasing_slope,2,0,2,9,[0-6,1-2,2-1]).
ctr_sol(min_decreasing_slope,3,0,3,64,[0-20,1-22,2-14,3-8]).
ctr_sol(min_decreasing_slope,4,0,4,625,[0-70,1-256,2-145,3-98,4-56]).
ctr_sol(min_decreasing_slope,5,0,5,7776,[0-252,1-3512,2-1864,3-1062,4-704,5-382]).
ctr_sol(min_decreasing_slope,6,0,6,117649,[0-924,1-56537,2-28728,3-14729,4-8853,5-5266,6-2612]).
ctr_sol(min_decreasing_slope,7,0,7,2097152,[0-3432,1-1051936,2-515372,3-255076,4-133672,5-78198,6-41330,7-18136]).
ctr_sol(min_decreasing_slope,8,0,8,43046721,[0-12870,1-22280084,2-10601773,3-5106480,4-2475484,5-1369232,6-730161,7-341618,8-129019]).

min_decreasing_slope_c(MIN, VARIABLES) :-
    check_type(dvar_gteq(0), MIN),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARS, N),
    N > 0,
    min_decreasing_slope_c1(VARS, 0, MIN).

min_decreasing_slope_c1([_], MIN, MIN) :- !.
min_decreasing_slope_c1(_, 1, MIN) :-
    !,
    MIN = 1.
min_decreasing_slope_c1([V1,V2|R], M, MIN) :-
    V1 =< V2,
    !,
    min_decreasing_slope_c1([V2|R], M, MIN).
min_decreasing_slope_c1([V1,V2|R], M, MIN) :-
    (M = 0 ->
	N is V1-V2
    ;
	N is min(M,V1-V2)
    ),
    min_decreasing_slope_c1([V2|R], N, MIN).

min_decreasing_slope_counters_check(L, [0|S]) :-
   min_decreasing_slope_counters_check(L, 0, S).
	
min_decreasing_slope_counters_check([V1,V2|R], M, [M|S]) :-
    V1 =< V2,
    !,
    min_decreasing_slope_counters_check([V2|R], M, S).
min_decreasing_slope_counters_check([V1,V2|R], M, [N|S]) :-
    !,
    (M = 0 ->
	N is V1-V2
    ;
	N is min(M,V1-V2)
    ),
    min_decreasing_slope_counters_check([V2|R], N, S).
min_decreasing_slope_counters_check([_], _, []).


ctr_automaton_signature(min_decreasing_slope, min_decreasing_slope_a, pair_signature(2,signature)).

% 0: VAR1 =< VAR2
% 1: VAR1 >  VAR2
min_decreasing_slope_a(FLAG, MIN, VARIABLES) :-
    check_type(dvar_gteq(0), MIN),
    collection(VARIABLES, [dvar]),
    min_decreasing_slope_signature(VARIABLES, SIGNATURE, DIFFERENCES),    
    automaton(DIFFERENCES, Di,
              SIGNATURE, 
              [source(s),sink(t),sink(s)],
              [arc(s,0,s            ),
               arc(s,1,t,[Di]       ),
	       arc(t,0,t            ),
               arc(t,1,t,[min(M,Di)])],
              [M],[0],[MINIMUM]),
    MINIMUM #= MIN #<=> FLAG.

min_decreasing_slope_signature([_], [], []) :- !.
min_decreasing_slope_signature([[var-VAR1],[var-VAR2]|VARs], [S|RS], [DIFFERENCE|RD]) :-
    VAR1 #=< VAR2 #<=> S #= 0,
    VAR1 #>  VAR2 #<=> S #= 1,
    VAR1 #= DIFFERENCE + VAR2,
    min_decreasing_slope_signature([[var-VAR2]|VARs], RS, RD).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
min_decreasing_slope_a_s(FLAG, MIN, VARIABLES, SIGNATURE) :-
    check_type(dvar_gteq(0), MIN),
    collection(VARIABLES, [dvar]),
    difference_decreasing_slope_signature(VARIABLES, DIFFERENCES),    
    automaton(DIFFERENCES, Di,
              SIGNATURE, 
              [source(s),sink(t),sink(s)],
              [arc(s,0,s            ),
	       arc(s,1,s            ),
               arc(s,2,t,[Di]       ),
	       arc(t,0,t            ),
	       arc(t,1,t            ),
               arc(t,2,t,[min(M,Di)])],
              [M],[0],[MINIMUM]),
    MINIMUM #= MIN #<=> FLAG.
