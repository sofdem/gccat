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

ctr_date(max_decreasing_slope,['20130317']).

ctr_origin(max_decreasing_slope, 'Motivated by time series.', []).

ctr_arguments(max_decreasing_slope,
              ['MAX'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(max_decreasing_slope,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(max_decreasing_slope,
                 ['MAX' >= 0                    ,
                  'MAX' < range('VARIABLES'^var),
                  required('VARIABLES',var)     ,
		  size('VARIABLES') > 0         ]).

ctr_pure_functional_dependency(max_decreasing_slope, []).
ctr_functional_dependency(max_decreasing_slope, 1, [2]).

ctr_typical(max_decreasing_slope,
            ['MAX'                  > 0                       ,
	     'MAX'                  < range('VARIABLES'^var)-1,
	     size('VARIABLES')      > 2                       ,
             range('VARIABLES'^var) > 2                       ]).

ctr_example(max_decreasing_slope,
            [max_decreasing_slope(4,[[var-1],[var-1],[var-5],[var-8],[var-6],[var-2],[var-4],[var-1],[var-2]]),
	     max_decreasing_slope(0,[[var-1],[var-3],[var-5],[var-8]]),
	     max_decreasing_slope(8,[[var-3],[var-1],[var-9],[var-1]])]).

ctr_cond_imply(max_decreasing_slope, longest_decreasing_sequence,
	       [range('VARIABLES'^var) = 'MAX' + 1],
	       [range('VARIABLES'^var) = 'L' + 1],
	       [none,'VARIABLES']).
ctr_cond_imply(max_decreasing_slope, min_decreasing_slope, ['MAX' = 1], ['MIN' = 1], id).

ctr_key_words(max_decreasing_slope,['sequence'                  ,
                                    'automaton'                 ,
                                    'automaton with counters'   ,
                                    'reverse of a constraint'   ,
		                    'glue matrix'               ,
		                    'functional dependency'     ,
		                    'pure functional dependency']).

ctr_eval(max_decreasing_slope, [checker(max_decreasing_slope_c),
		                automaton(max_decreasing_slope_a),
                                automaton_with_signature(max_decreasing_slope_a_s)]).

ctr_sol(max_decreasing_slope,2,0,2,9,[0-6,1-2,2-1]).
ctr_sol(max_decreasing_slope,3,0,3,64,[0-20,1-20,2-16,3-8]).
ctr_sol(max_decreasing_slope,4,0,4,625,[0-70,1-151,2-188,3-142,4-74]).
ctr_sol(max_decreasing_slope,5,0,5,7776,[0-252,1-1036,2-1952,3-2106,4-1584,5-846]).
ctr_sol(max_decreasing_slope,6,0,6,117649,[0-924,1-6828,2-19200,3-29035,4-28266,5-21684,6-11712]).
ctr_sol(max_decreasing_slope,7,0,7,2097152,[0-3432,1-44220,2-183304,3-380116,4-483840,5-457632,6-353088,7-191520]).
ctr_sol(max_decreasing_slope,8,0,8,43046721,[0-12870,1-284405,2-1721425,3-4847301,4-8021350,5-9208124,6-8654931,7-6673834,8-3622481]).

max_decreasing_slope_c(MAX, VARIABLES) :-
    check_type(dvar_gteq(0), MAX),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARS, N),
    N > 0,
    max_decreasing_slope_c1(VARS, 0, MAX).

max_decreasing_slope_c1([_], MAX, MAX) :- !.
max_decreasing_slope_c1([V1,V2|R], M, MAX) :-
    V1 =< V2,
    !,
    max_decreasing_slope_c1([V2|R], M, MAX).
max_decreasing_slope_c1([V1,V2|R], M, MAX) :-
    N is max(M,V1-V2),
    max_decreasing_slope_c1([V2|R], N, MAX).

max_decreasing_slope_counters_check(L, [0|S]) :-
   max_decreasing_slope_counters_check(L, 0, S).

max_decreasing_slope_counters_check([V1,V2|R], M, [M|S]) :-
    V1 =< V2,
    !,
    max_decreasing_slope_counters_check([V2|R], M, S).
max_decreasing_slope_counters_check([V1,V2|R], M, [N|S]) :-
    N is max(M,V1-V2),
    max_decreasing_slope_counters_check([V2|R], N, S).
max_decreasing_slope_counters_check([_], _, []) :- !.

ctr_automaton_signature(max_decreasing_slope, max_decreasing_slope_a, pair_signature(2,signature)).

% 0: VAR1 =< VAR2
% 1: VAR1 >  VAR2
max_decreasing_slope_a(FLAG, MAX, VARIABLES) :-
    check_type(dvar_gteq(0), MAX),
    collection(VARIABLES, [dvar]),
    max_decreasing_slope_signature(VARIABLES, SIGNATURE, DIFFERENCES),    
    automaton(DIFFERENCES, Di,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s            ),
               arc(s,1,s,[max(M,Di)])],
              [M],[0],[MAXIMUM]),
    MAXIMUM #= MAX #<=> FLAG.

max_decreasing_slope_signature([_], [], []) :- !.
max_decreasing_slope_signature([[var-VAR1],[var-VAR2]|VARs], [S|RS], [DIFFERENCE|RD]) :-
    VAR1 #=< VAR2 #<=> S #= 0,
    VAR1 #>  VAR2 #<=> S #= 1,
    VAR1 #= DIFFERENCE + VAR2,
    max_decreasing_slope_signature([[var-VAR2]|VARs], RS, RD).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
max_decreasing_slope_a_s(FLAG, MAX, VARIABLES, SIGNATURE) :-
    check_type(dvar_gteq(0), MAX),
    collection(VARIABLES, [dvar]),
    difference_decreasing_slope_signature(VARIABLES, DIFFERENCES),    
    automaton(DIFFERENCES, Di,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s            ),
	       arc(s,1,s            ),
               arc(s,2,s,[max(M,Di)])],
              [M],[0],[MAXIMUM]),
    MAXIMUM #= MAX #<=> FLAG.
