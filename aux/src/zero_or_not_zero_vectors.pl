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

ctr_date(zero_or_not_zero_vectors,['20120516']).

ctr_origin(zero_or_not_zero_vectors, 'Tournament scheduling', []).

ctr_types(zero_or_not_zero_vectors, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(zero_or_not_zero_vectors,
              ['VECTORS'-collection(vec-'VECTOR')]).

ctr_synonyms(zero_or_not_zero_vectors,[zeros_or_not_zeros_vectors,
				       not_zero_or_zero_vectors  ,
				       not_zeros_or_zeros_vectors]).

ctr_predefined(zero_or_not_zero_vectors).

ctr_restrictions(zero_or_not_zero_vectors,
                 [size('VECTOR')  >= 1    ,
                  required('VECTOR',var)  ,
                  size('VECTORS') >= 1    ,
                  required('VECTORS',vec) ,
                  same_size('VECTORS',vec)]).

ctr_typical(zero_or_not_zero_vectors,
            [size('VECTOR')  > 1,
             size('VECTORS') > 1]).

ctr_contractible(zero_or_not_zero_vectors, [], 'VECTORS', any).

ctr_example(zero_or_not_zero_vectors,
            zero_or_not_zero_vectors([[vec-[[var-5], [var-6]]],
                                      [vec-[[var-5], [var-6]]],
                                      [vec-[[var-0], [var-0]]],
                                      [vec-[[var-9], [var-3]]],
                                      [vec-[[var-0], [var-0]]]])).

ctr_key_words(zero_or_not_zero_vectors,['predefined constraint',
                                        'arithmetic constraint',
                                        'vector'               ]).

ctr_eval(zero_or_not_zero_vectors, [checker(zero_or_not_zero_vectors_c)      ,
				    reformulation(zero_or_not_zero_vectors_r),
				    density(zero_or_not_zero_vectors_d)      ]).

zero_or_not_zero_vectors_c(VECTORS) :-
    % at least one vector and one component
    VECTORS = [[vec-[[var-_]|_]]|_],
    collection(VECTORS, [col([int])]),
    same_size(VECTORS),
    zero_or_not_zero_vectors_c(VECTORS, 1).

zero_or_not_zero_vectors_c([], 0) :- !.
zero_or_not_zero_vectors_c([[vec-V]|R], F) :-
    (V = [[var-0]|W] ->
	NextF = 0,
	zero_vector(W)
    ;
	NextF = F,
	not_zero_vector(W)
    ),
    zero_or_not_zero_vectors_c(R, NextF).

zero_or_not_zero_vectors_r(VECTORS) :-
    % at least one vector and one component
    VECTORS = [[vec-[[var-_]|_]]|_],
    collection(VECTORS, [col([dvar])]),
    zero_or_not_zero_vectors_r1(VECTORS, AtleastOneZero),
    call(AtleastOneZero).

zero_or_not_zero_vectors_r1([], 0) :- !.
zero_or_not_zero_vectors_r1([[vec-V]|R], Zero #\/ S) :-
    zero_or_not_zero_vectors_r2(V, Zero, NotZero),
    call(Zero #\/ NotZero),
    zero_or_not_zero_vectors_r1(R, S).

zero_or_not_zero_vectors_r2([], 1, 1) :- !.
zero_or_not_zero_vectors_r2([[var-V]|R], V#=0 #/\ S, V#\=0 #/\ T) :-
    zero_or_not_zero_vectors_r2(R, S, T).

zero_or_not_zero_vectors_d(Density, VECTORS) :-
    count_zeros_in_vectors(VECTORS, 0, ZEROS),
    VECTORS = [[vec-[V|_]]|_],
    length(VECTORS, N),
    length(V, M),
    A is N*M,
    Density is (A-ZEROS) / A.
