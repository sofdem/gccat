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

ctr_date(order,['20120502']).

ctr_origin(order, 'Derived from %c', [sort_permutation]).

ctr_types(order, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(order,
              ['VECTORS'-collection(vec-'VECTOR'),
               'PERMUTATION'-collection(var-dvar)]).

ctr_restrictions(order,
                 [size('VECTOR')  >= 1                      ,
		  size('VECTORS') >= 1                      ,
                  required('VECTORS',vec)                   ,
                  same_size('VECTORS',vec)                  ,
		  required('PERMUTATION',var)               ,
		  'PERMUTATION'^var   >= 1                  ,
		  'PERMUTATION'^var   =< size('PERMUTATION'),
                  size('PERMUTATION') = size('VECTORS')     ]).

ctr_typical(order,
            [size('VECTOR')  > 1,
             size('VECTORS') > 1]).

ctr_predefined(order).

ctr_functional_dependency(order, 2, [1]).

ctr_example(order,
            order([[vec-[[var-1], [var-1], [var-2], [var-2]]],
                   [vec-[[var-2], [var-1], [var-2], [var-1]]],
                   [vec-[[var-2], [var-1], [var-1], [var-1]]],
                   [vec-[[var-1], [var-1], [var-1], [var-2]]],
                   [vec-[[var-1], [var-2], [var-2], [var-1]]],
                   [vec-[[var-1], [var-1], [var-1], [var-1]]],
                   [vec-[[var-2], [var-2], [var-1], [var-1]]],
                   [vec-[[var-2], [var-1], [var-1], [var-2]]]],
	          [[var-3],[var-7],[var-5],[var-2],[var-4],[var-1],[var-8],[var-6]])).

ctr_see_also(order,
 [link('common keyword', sort_permutation,  '%k, %k', [sort, permutation])]).

ctr_key_words(order,['sort'                 ,
                     'permutation'          ,
                     'functional dependency']).

ctr_eval(order, [reformulation(order_c)]).

order_c(VECTORS, PERMUTATION) :-
    collection(VECTORS, [col([int])]),
    VECTORS = [[vec-VECTOR]|_],
    same_size(VECTORS),
    length(VECTOR, M),
    M >= 1,
    length(VECTORS, N),
    collection(PERMUTATION, [int(1,N)]),
    length(PERMUTATION, N),
    get_attr1(PERMUTATION, P),
    sort(P, S),
    length(S, N),
    order_c1(VECTORS, P, VP),
    sort(VP, SVP),
    order_c2(SVP, 1).

order_c1([], [], []) :- !.
order_c1([VEC|R], [O|S], [VEC-O|T]) :-
    order_c1(R, S, T).

order_c2([], _) :- !.
order_c2([_VEC-O], O) :- !.
order_c2([VEC1-O1,VEC2-O2|R], O1) :-
    VEC1 \== VEC2,
    Next is O1+1,
    order_c2([VEC2-O2|R], Next).
