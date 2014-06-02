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

:-dynamic min_width_valley_a/3.

ctr_date(min_width_valley,['20121202']).

ctr_origin(min_width_valley, 'derived from %c', [valley]).

ctr_arguments(min_width_valley,
              ['MIN_WIDTH'-dvar                ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(min_width_valley,
                 [items('VARIABLES',reverse)  ,
		  translate(['VARIABLES'^var])]).

ctr_synonyms(min_width_valley,[min_base_valley]).

ctr_restrictions(min_width_valley,
                 ['MIN_WIDTH' >=  0                  ,
                  'MIN_WIDTH' =<  size('VARIABLES')-2,
                  required('VARIABLES',var)          ]).

ctr_typical(min_width_valley,
            ['MIN_WIDTH'       > 1,
             size('VARIABLES') > 2]).

ctr_pure_functional_dependency(min_width_valley, []).
ctr_functional_dependency(min_width_valley, 1, [2]).

ctr_example(min_width_valley,
            [min_width_valley(5,[[var-3],[var-3],[var-5],[var-5],[var-4],[var-2],[var-2],[var-3],[var-4],
			         [var-6],[var-6],[var-5],[var-5],[var-5],[var-5],[var-5],[var-5],[var-6]]),
	     min_width_valley(0,[[var-3],[var-8],[var-8],[var-5],[var-0],[var-0]]),
             min_width_valley(4,[[var-9],[var-8],[var-8],[var-0],[var-0],[var-2]])]).

ctr_see_also(min_width_valley,
 [link('common keyword', valley, '%k', [sequence])]).

ctr_key_words(min_width_valley,['sequence'                  ,
                                'automaton'                 ,
                                'automaton with counters'   ,
                                'reverse of a constraint'   ,
		                'glue matrix'               ,
                                'functional dependency'     ,
		                'pure functional dependency']).

ctr_eval(min_width_valley, [checker(min_width_valley_c),
			    automaton(min_width_valley_a),
			    automaton_with_signature(min_width_valley_a_s)]).

ctr_sol(min_width_valley,2,0,2,9,[0-9]).
ctr_sol(min_width_valley,3,0,3,64,[0-50,1-14]).
ctr_sol(min_width_valley,4,0,4,625,[0-295,1-230,2-100]).
ctr_sol(min_width_valley,5,0,5,7776,[0-1792,1-3205,2-2100,3-679]).
ctr_sol(min_width_valley,6,0,6,117649,[0-11088,1-56637,2-28420,3-17024,4-4480]).
ctr_sol(min_width_valley,7,0,7,2097152,[0-69498,1-1174398,2-424928,3-268722,4-130452,5-29154]).
ctr_sol(min_width_valley,8,0,8,43046721,[0-439791,1-26327058,2-9363060,3-3413256,4-2345982,5-968946,6-188628]).

min_width_valley_c(0, []) :- !.
min_width_valley_c(MIN_WIDTH, VARIABLES) :-
    check_type(dvar, MIN_WIDTH),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARS, N),
    min_width_valley_c(VARS, s, 1, 0, N, 0, N, MIN_WIDTH).

min_width_valley_c([VAR1,VAR2|R], s, I, C, W, F, N, MIN_WIDTH) :-
    VAR1 =< VAR2,
    !,
    I1 is I+1,
    min_width_valley_c([VAR2|R], s, I1, C, W, F, N, MIN_WIDTH).
min_width_valley_c([VAR1,VAR2|R], s, I, C, W, _, N, MIN_WIDTH) :-
    VAR1 > VAR2,
    !,
    I1 is I+1,
    min_width_valley_c([VAR2|R], j, I1, C, W, I, N, MIN_WIDTH).
min_width_valley_c([VAR1,VAR2|R], j, I, C, W, F, N, MIN_WIDTH) :-
    VAR1 >= VAR2,
    !,
    I1 is I+1,
    min_width_valley_c([VAR2|R], j, I1, C, W, F, N, MIN_WIDTH).
min_width_valley_c([VAR1,VAR2|R], j, I, _, W, F, N, MIN_WIDTH) :-
    VAR1 < VAR2,
    !,
    I1 is I+1,
    C1 is I-F,
    min_width_valley_c([VAR2|R], k, I1, C1, W, F, N, MIN_WIDTH).
min_width_valley_c([VAR1,VAR2|R], k, I, C, W, F, N, MIN_WIDTH) :-
    VAR1 = VAR2,
    !,
    I1 is I+1,
    min_width_valley_c([VAR2|R], k, I1, C, W, F, N, MIN_WIDTH).
min_width_valley_c([VAR1,VAR2|R], k, I, _, W, F, N, MIN_WIDTH) :-
    VAR1 < VAR2,
    !,
    I1 is I+1,
    C1 is I-F,
    min_width_valley_c([VAR2|R], k, I1, C1, W, F, N, MIN_WIDTH).
min_width_valley_c([VAR1,VAR2|R], k, I, C, W, _, N, MIN_WIDTH) :-
    VAR1 > VAR2,
    !,
    I1 is I+1,
    W1 is min(W,C),
    min_width_valley_c([VAR2|R], j, I1, C, W1, I, N, MIN_WIDTH).
min_width_valley_c([_], _, _, C, W, _, _, MIN_WIDTH) :-
    MIN_WIDTH #= min(W,C).

min_width_valley_counters_check(VARS, N, [0|COUNTERS]) :-
	min_width_valley_counters_check(VARS, s, 1, 0, N, 0, N, COUNTERS).

min_width_valley_counters_check([VAR1,VAR2|R], s, I, C, W, F, N, [MIN_WIDTH|REST]) :-
    VAR1 =< VAR2,
    !,
    I1 is I+1,
    MIN_WIDTH #= min(W,C),
    min_width_valley_counters_check([VAR2|R], s, I1, C, W, F, N, REST).
min_width_valley_counters_check([VAR1,VAR2|R], s, I, C, W, _, N, [MIN_WIDTH|REST]) :-
    VAR1 > VAR2,
    !,
    I1 is I+1,
    MIN_WIDTH #= min(W,C),
    min_width_valley_counters_check([VAR2|R], j, I1, C, W, I, N, REST).
min_width_valley_counters_check([VAR1,VAR2|R], j, I, C, W, F, N, [MIN_WIDTH|REST]) :-
    VAR1 >= VAR2,
    !,
    I1 is I+1,
    MIN_WIDTH #= min(W,C),
    min_width_valley_counters_check([VAR2|R], j, I1, C, W, F, N, REST).
min_width_valley_counters_check([VAR1,VAR2|R], j, I, _, W, F, N, [MIN_WIDTH|REST]) :-
    VAR1 < VAR2,
    !,
    I1 is I+1,
    C1 is I-F,
    MIN_WIDTH #= min(W,C1),
    min_width_valley_counters_check([VAR2|R], k, I1, C1, W, F, N, REST).
min_width_valley_counters_check([VAR1,VAR2|R], k, I, C, W, F, N, [MIN_WIDTH|REST]) :-
    VAR1 = VAR2,
    !,
    I1 is I+1,
    MIN_WIDTH #= min(W,C),
    min_width_valley_counters_check([VAR2|R], k, I1, C, W, F, N, REST).
min_width_valley_counters_check([VAR1,VAR2|R], k, I, _, W, F, N, [MIN_WIDTH|REST]) :-
    VAR1 < VAR2,
    !,
    I1 is I+1,
    C1 is I-F,
    MIN_WIDTH #= min(W,C1),
    min_width_valley_counters_check([VAR2|R], k, I1, C1, W, F, N, REST).
min_width_valley_counters_check([VAR1,VAR2|R], k, I, C, W, _, N, [MIN_WIDTH|REST]) :-
    VAR1 > VAR2,
    !,
    I1 is I+1,
    W1 is min(W,C),
    MIN_WIDTH #= min(W1,C),
    min_width_valley_counters_check([VAR2|R], j, I1, C, W1, I, N, REST).
min_width_valley_counters_check([_], _, _, _, _, _, _, []).

ctr_automaton_signature(min_width_valley, min_width_valley_a, pair_signature(2,signature)).

min_width_valley_a(FLAG, MIN_WIDTH, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    min_width_valley_a_s(FLAG, MIN_WIDTH, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2   INVERSER LE 0 ET LE 2
min_width_valley_a_s(FLAG, MIN_WIDTH, VARIABLES, SIGNATURE) :-
    check_type(dvar, MIN_WIDTH),
    length(VARIABLES, N),
    (N = 0 ->
	Wn = 0,
	Cn = 0
    ;
	collection(VARIABLES, [dvar]),
	pair_index_signature(VARIABLES, 1, INDICES),
	automaton(INDICES, I, SIGNATURE,
                  [source(s),sink(s),sink(j),sink(k)],
		  [arc(s,0,s),
		   arc(s,1,s),
		   arc(s,2,j,[C,W,I]),		   
		   arc(j,0,k,[I-F,W,F]),
		   arc(j,1,j),
		   arc(j,2,j),
		   arc(k,0,k,[I-F,W,F]),
		   arc(k,1,k),
		   arc(k,2,j,[C,min(W,C),I])],
		  [C,W,F],[0,N,0],[Cn,Wn,_])
    ),
    MIN_WIDTH #= min(Wn,Cn) #<=> FLAG.
