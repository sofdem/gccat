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

ctr_date(differ_from_at_most_k_pos,['20120227']).

ctr_origin(differ_from_at_most_k_pos, 'Inspired by %c.', [differ_from_at_least_k_pos]).

ctr_types(differ_from_at_most_k_pos,
          ['VECTOR'-collection(var-dvar)]).

ctr_arguments(differ_from_at_most_k_pos,
              ['K'-int         ,
               'VECTOR1'-'VECTOR',
               'VECTOR2'-'VECTOR']).

ctr_exchangeable(differ_from_at_most_k_pos,
                 [args([['K'],['VECTOR1','VECTOR2']]),
                  vals(['K'],int(=<(size('VECTOR1'))),<,dontcare,dontcare),
                  items_sync('VECTOR1','VECTOR2',all)]).

ctr_restrictions(differ_from_at_most_k_pos,
                 [size('VECTOR') >= 1              ,
                  required('VECTOR',var)           ,
                  'K'            >= 0              ,
                  'K'            =< size('VECTOR1'),
                  size('VECTOR1') = size('VECTOR2')]).

ctr_typical(differ_from_at_most_k_pos,
            ['K'             > 0              ,
	     'K'             < size('VECTOR1'),
             size('VECTOR1') > 1              ]).

ctr_contractible(differ_from_at_most_k_pos, [], ['VARIABLES1','VARIABLES2'], any).

ctr_graph(differ_from_at_most_k_pos,
          ['VECTOR1','VECTOR2'],
          2,
          ['PRODUCT'(=)>>collection(vector1,vector2)],
          [vector1^var =\= vector2^var],
          ['NARC' =< 'K'],
          []).

ctr_example(differ_from_at_most_k_pos,
            differ_from_at_most_k_pos(3,
                                      [[var-2],[var-5],[var-2],[var-0]],
                                      [[var-3],[var-6],[var-2],[var-0]])).

ctr_draw_example(differ_from_at_most_k_pos,
                 ['VECTOR1','VECTOR2'],
                 [[[var-2],[var-5],[var-2],[var-0]],
                  [[var-3],[var-6],[var-2],[var-0]]],
                 ['PRODUCT'(=)],
                 [1-1,2-2],
                 ['NARC'],
                 '','NARC=2',
                 [2.145,2.145,1.9,1.3]).

ctr_see_also(differ_from_at_most_k_pos,
 [link('system of constraints', all_differ_from_at_most_k_pos, '',                                   []),
  link('implied by', differ_from_exactly_k_pos, '$\\leq\\argument{K}$ replaced by $=\\argument{K}$', [])]).

ctr_key_words(differ_from_at_most_k_pos,['value constraint',
                                         'vector'         ]).

ctr_eval(differ_from_at_most_k_pos, [reformulation(differ_from_at_most_k_pos_r),
				     checker(differ_from_at_most_k_pos_c)]).

differ_from_at_most_k_pos_r(K, VECTOR1, VECTOR2) :-
    integer(K),
    collection(VECTOR1, [dvar]),
    collection(VECTOR2, [dvar]),
    length(VECTOR1, N1),
    length(VECTOR2, N2),
    K >= 0,
    K =< N1,
    N1 = N2,
    N1 >= 1,
    differ_from_k_pos(VECTOR1, VECTOR2, SumBool),
    call(K #>= SumBool).

differ_from_at_most_k_pos_c(K, VECTOR1, VECTOR2) :-
    integer(K),
    collection(VECTOR1, [int]),
    collection(VECTOR2, [int]),
    length(VECTOR1, N),
    length(VECTOR2, N),
    N >= 1,
    K >= 0,
    K =< N,
    (N =< K -> true ; differ_from_at_most_k_pos_check(VECTOR1, VECTOR2, N, K)).

differ_from_at_most_k_pos_check([], [], _, 0) :- !.
differ_from_at_most_k_pos_check([[_-U]|R], [[_-V]|S], N, K) :-
    (U = V ->
	NewK is K
    ;
	NewK is K-1, NewK >= 0
    ),
    NewN is N-1,
    (NewN =< NewK -> true ; differ_from_at_most_k_pos_check(R, S, NewN, NewK)).
