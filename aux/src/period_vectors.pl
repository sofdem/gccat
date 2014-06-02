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

ctr_date(period_vectors,['20110614']).

ctr_origin(period_vectors, 'Derived from %c', [period]).

ctr_types(period_vectors, ['VECTOR'-collection(var-dvar),
                           'CTR'-atom                   ]).

ctr_arguments(period_vectors,
              ['PERIOD'-dvar                     ,
               'VECTORS'-collection(vec-'VECTOR'),
               'CTRS'-collection(ctr-'CTR')      ]).

ctr_exchangeable(period_vectors,
                 [items('VECTORS',reverse)]).

ctr_restrictions(period_vectors,
                 [size('VECTOR') >= 1             ,
                  required('VECTOR',var)          ,
                  in_list('CTR',[=,=\=,<,>=,>,=<]),
                  'PERIOD' >= 1                   ,
                  'PERIOD' =< size('VECTORS')     ,
                  required('VECTORS',vec)         ,
                  same_size('VECTORS',vec)        ,
                  required('CTRS',ctr)            ,
                  size('CTRS') = size('VECTOR')   ]).

ctr_typical(period_vectors,
            [in_list('CTR',[=])               ,
             size('VECTOR')  > 1              ,
             'PERIOD'        > 1              ,
             'PERIOD'        < size('VECTORS'),
             size('VECTORS') > 2              ]).

ctr_pure_functional_dependency(period_vectors, []).
ctr_functional_dependency(period_vectors, 1, [2,3]).

ctr_contractible(period_vectors, [], 'VECTORS', prefix).
ctr_contractible(period_vectors, [], 'VECTORS', suffix).

ctr_predefined(period_vectors).

ctr_example(period_vectors,
            period_vectors(3,
                           [[vec-[[var-1], [var-0]]],
                            [vec-[[var-1], [var-5]]],
                            [vec-[[var-4], [var-4]]],
                            [vec-[[var-1], [var-0]]],
                            [vec-[[var-1], [var-5]]],
                            [vec-[[var-4], [var-4]]],
                            [vec-[[var-1], [var-0]]],
                            [vec-[[var-1], [var-5]]]],
                           [[ctr-(=)],[ctr-(=)]])).

ctr_see_also(period_vectors,
 [link('specialisation', period, '%k replaced by %e', [vector, variable])]).

ctr_key_words(period_vectors,['predefined constraint'     ,
                              'periodic'                  ,
                              'sequence'                  ,
                              'vector'                    ,
                              'functional dependency'     ,
		              'pure functional dependency']).

ctr_eval(period_vectors, [reformulation(period_vectors_r)]).

period_vectors_r(PERIOD, VECTORS, CTRS) :-
	check_type(dvar, PERIOD),
	collection(VECTORS, [col([dvar])]),
	collection(CTRS, [atom([=,=\=,<,>=,>,=<])]),
	length(VECTORS, N),
	PERIOD #>= 1,
	PERIOD #=< N,
	get_attr11(VECTORS, VECTS),
	get_attr1(CTRS, LCTRS),
	period1(N, VECTS, LISTS),
	period4(LISTS, 2, LCTRS, BOOLS),
	reverse(BOOLS, RBOOLS),
	period7(RBOOLS, 1, PERIOD, 1, EXPR),
	call(EXPR).
