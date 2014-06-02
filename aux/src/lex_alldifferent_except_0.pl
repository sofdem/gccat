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

ctr_date(lex_alldifferent_except_0,['20120515']).

ctr_origin(lex_alldifferent_except_0, 'H.~Simonis', []).

ctr_types(lex_alldifferent_except_0,
          ['VECTOR'-collection(var-dvar)]).

ctr_synonyms(lex_alldifferent_except_0,[lex_alldiff_except_0           ,
                                        lex_alldistinct_except_0       ,
                                        alldiff_on_tuples_except_0     ,
                                        alldifferent_on_tuples_except_0,
                                        alldistinct_on_tuples_except_0 ]).

ctr_arguments(lex_alldifferent_except_0,
              ['VECTORS'-collection(vec-'VECTOR')]).

ctr_restrictions(lex_alldifferent_except_0,
                 [size('VECTOR') >= 1     ,
                  required('VECTOR',var)  ,
                  required('VECTORS',vec) ,
                  same_size('VECTORS',vec)]).

ctr_typical(lex_alldifferent_except_0,
            [size('VECTOR')  > 1,
             size('VECTORS') > 1]).

ctr_contractible(lex_alldifferent_except_0, [], 'VECTORS', any).

ctr_predefined(lex_alldifferent_except_0).

ctr_example(lex_alldifferent_except_0,
            lex_alldifferent_except_0([[vec-[[var-0], [var-0], [var-0]]],
				       [vec-[[var-5], [var-2], [var-0]]],
                                       [vec-[[var-5], [var-8], [var-0]]],
                                       [vec-[[var-0], [var-0], [var-0]]]])).

ctr_see_also(lex_alldifferent_except_0,
 [link('implied by', lex_alldifferent, '', [])]).

ctr_key_words(lex_alldifferent_except_0,['vector'                               ,
                                         'difference between pairs of variables',
					 'joker value'                          ]).

ctr_persons(lex_alldifferent_except_0,['Simonis H.']).

ctr_eval(lex_alldifferent_except_0, [reformulation(lex_alldifferent_except_0_r),
				     checker(lex_alldifferent_except_0_c)      ,
				     density(lex_alldifferent_except_0_d)      ]).

% sort (which remove duplicates) and compare length taking zeros vectors into account
lex_alldifferent_except_0_c([]) :- !.
lex_alldifferent_except_0_c(VECTORS) :-
    collection(VECTORS, [col([int])]),
    same_size(VECTORS),
    length(VECTORS, L),
    count_zero_vectors(VECTORS, 0, Z),
    sort(VECTORS, SVECTORS),
    length(SVECTORS, S),
    (Z = 0 -> S1 = S ; S1 is S-1),
    L =:= S1+Z.

lex_alldifferent_except_0_d(Density, VECTORS) :-
    remove_zeros(VECTORS, VECTORS1),
    count_zero_vectors(VECTORS1, 0, Z),
    length(VECTORS1, L),
    VECTORS1 = [[vec-V]|_],
    length(V, M),
    Density1 is Z / (L*M),
    lex_alldifferent_density(Density2, VECTORS1),
    Density is min(Density1,Density2).

count_zero_vectors([], Z, Z) :- !.
count_zero_vectors([[vec-V]|R], Cur, Z) :-
    V = [[var-_]|_],
    (zero_vector(V) -> Next is Cur+1 ; Next is Cur),
    count_zero_vectors(R, Next, Z).

remove_zeros([], []) :- !.
remove_zeros([[vec-V]|R], S) :-
    zero_vector(V),
    !,
    remove_zeros(R, S).
remove_zeros([VEC|R], [VEC|S]) :-
    remove_zeros(R, S).

lex_alldifferent_except_0_r([]) :- !.
lex_alldifferent_except_0_r(VECTORS) :-
    collection(VECTORS, [col([dvar])]),
    lex_alldifferent_except_01(VECTORS).

lex_alldifferent_except_01([]) :- !.
lex_alldifferent_except_01([[_-VECTOR]|R]) :-
    VECTOR = [[var-_]|_],
    lex_different_except_zero(VECTOR, ZERO),
    lex_alldifferent_except_02(R, VECTOR, ZERO),
    lex_alldifferent_except_01(R).

lex_alldifferent_except_02([], _, _) :- !.
lex_alldifferent_except_02([[_-VECTORi]|R], VECTOR, ZERO) :-
    lex_different_except_03(VECTOR, VECTORi, DIFF),
    call(ZERO #\/ DIFF),
    lex_alldifferent_except_02(R, VECTOR, ZERO).

lex_different_except_03([], [], 0) :- !.
lex_different_except_03([[var-U]|R], [[var-V]|S], U#\=V #\/ T) :-
    lex_different_except_03(R, S, T).

lex_different_except_zero([], 1) :- !.
lex_different_except_zero([[var-V]|R], V#=0 #/\ S) :-
    lex_different_except_zero(R, S).
