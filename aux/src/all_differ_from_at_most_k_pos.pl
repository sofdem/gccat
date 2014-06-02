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
    ctr_extensible/2,
    ctr_extensible/3,
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

ctr_date(all_differ_from_at_most_k_pos,['20120228']).

ctr_origin(all_differ_from_at_most_k_pos, 'Inspired by %c.', [all_differ_from_at_least_k_pos]).

ctr_types(all_differ_from_at_most_k_pos, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(all_differ_from_at_most_k_pos,
              ['K'-int                           ,
               'VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(all_differ_from_at_most_k_pos,
                 [items('VECTORS',all),
                  items_sync('VECTORS'^vec,all)]).

ctr_restrictions(all_differ_from_at_most_k_pos,
                 [required('VECTOR',var)  ,
                   size('VECTOR') >=  1   ,
                   size('VECTOR') >= 'K'  ,
                  'K'             >=  0   ,
                  required('VECTORS',vec) ,
                  same_size('VECTORS',vec)]).

ctr_typical(all_differ_from_at_most_k_pos,
            ['K'             > 0              ,
	     'K'             < size('VECTOR') ,
             size('VECTORS') > 1              ]).

ctr_contractible(all_differ_from_at_most_k_pos, [], 'VECTORS', any).

ctr_contractible(all_differ_from_at_most_k_pos, [], 'VECTORS'^vec, any).

ctr_graph(all_differ_from_at_most_k_pos,
          ['VECTORS'],
          2,
          ['CLIQUE'(=\=)>>collection(vectors1,vectors2)],
          [differ_from_at_most_k_pos('K',vectors1^vec,vectors2^vec)],
          ['NARC' = size('VECTORS')*size('VECTORS') - size('VECTORS')],
          ['NO_LOOP','SYMMETRIC']).

ctr_example(all_differ_from_at_most_k_pos,
            all_differ_from_at_most_k_pos(2,
                                          [[vec-[[var-0], [var-3], [var-0], [var-6]]],
                                           [vec-[[var-0], [var-3], [var-4], [var-1]]],
                                           [vec-[[var-0], [var-3], [var-4], [var-6]]]])).

ctr_draw_example(all_differ_from_at_most_k_pos,
                 ['VECTORS'],
                 [[[vec-[[var-0], [var-3], [var-0], [var-6]]],
                   [vec-[[var-0], [var-3], [var-4], [var-1]]],
                   [vec-[[var-0], [var-3], [var-4], [var-6]]]]],
                 ['CLIQUE'(=\=)],
                 [1-[2,3],2-[1,3],3-[1,2]],
                 ['NARC'],
                  '','NARC=6',
                 []).

ctr_see_also(all_differ_from_at_most_k_pos,
 [link('part of system of constraints', differ_from_at_most_k_pos,     '',                              []),
  link('used in graph description',     differ_from_at_most_k_pos,     '',                              []),
  link('implied by',                    all_differ_from_exactly_k_pos, '$\\leq$ %e replaced by $=$ %e', ['K','K'])]).

ctr_key_words(all_differ_from_at_most_k_pos,['system of constraints',
                                             'decomposition'        ,
                                             'disequality'          ,
                                             'vector'               ,
                                             'no loop'              ,
                                             'symmetric'            ]).

ctr_eval(all_differ_from_at_most_k_pos, [reformulation(all_differ_from_at_most_k_pos_r),
					       checker(all_differ_from_at_most_k_pos_c)]).

all_differ_from_at_most_k_pos_r(K, VECTORS) :-
    integer(K),
    K >= 0,
    all_differ_from_at_most_k_pos_rr(VECTORS, K).

all_differ_from_at_most_k_pos_c(K, []) :-
    !,
    integer(K),
    K >= 0.
all_differ_from_at_most_k_pos_c(K, VECTORS) :-
    integer(K),
    K >= 0,
    VECTORS = [[_-VECTOR]|_],
    length(VECTOR, N),
    N >= 1,
    N >= K,
    same_size(VECTORS),
    get_attr11(VECTORS, VECTS),
    all_differ_from_at_most_k_pos_cc(VECTS, N, K).

%--------------------------------------------------------------------------------------------------
all_differ_from_at_most_k_pos_rr([], _) :- !.
all_differ_from_at_most_k_pos_rr([[_-VECTOR1]|R], K) :-
    length(VECTOR1, N),
    N >= 1,
    N >= K,
    all_differ_from_at_most_k_pos_rr(R, VECTOR1, K),
    all_differ_from_at_most_k_pos_rr(R, K).

all_differ_from_at_most_k_pos_rr([], _, _).
all_differ_from_at_most_k_pos_rr([[_-VECTOR2]|R], VECTOR1, K) :-
    eval(differ_from_at_most_k_pos(K, VECTOR1, VECTOR2)),
    all_differ_from_at_most_k_pos_rr(R, VECTOR1, K).
%--------------------------------------------------------------------------------------------------
all_differ_from_at_most_k_pos_cc([], _, _) :- !.
all_differ_from_at_most_k_pos_cc([VECTOR1|R], N, K) :-
    all_differ_from_at_most_k_pos_cc(R, VECTOR1, N, K),
    all_differ_from_at_most_k_pos_cc(R, N, K).

all_differ_from_at_most_k_pos_cc([], _, _, _).
all_differ_from_at_most_k_pos_cc([VECTOR2|R], VECTOR1, N, K) :-
    all_differ_from_at_most_k_pos_check(VECTOR1, VECTOR2, N, K),
    all_differ_from_at_most_k_pos_cc(R, VECTOR1, N, K).

all_differ_from_at_most_k_pos_check([], [], _, 0) :- !.
all_differ_from_at_most_k_pos_check([U|R], [V|S], N, K) :-
    (U = V ->
	NewK is K
    ;
	NewK is K-1, NewK >= 0
    ),
    NewN is N-1,
    (NewN =< NewK -> true ; all_differ_from_at_most_k_pos_check(R, S, NewN, NewK)).
