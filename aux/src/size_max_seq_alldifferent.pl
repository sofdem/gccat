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

ctr_date(size_max_seq_alldifferent,['20030820','20060814','20121124']).

ctr_origin(size_max_seq_alldifferent, 'N.~Beldiceanu', []).

ctr_arguments(size_max_seq_alldifferent,
              ['SIZE'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(size_max_seq_alldifferent,
                 [translate(['VARIABLES'^var])]).

ctr_synonyms(size_max_seq_alldifferent,[size_maximal_sequence_alldiff     ,
                                        size_maximal_sequence_alldistinct ,
                                        size_maximal_sequence_alldifferent]).

ctr_restrictions(size_max_seq_alldifferent,
                 ['SIZE' >= 0                ,
                  'SIZE' =< size('VARIABLES'),
                  required('VARIABLES',var)  ]).

ctr_typical(size_max_seq_alldifferent,
            ['SIZE'                 > 2                ,
             'SIZE'                 < size('VARIABLES'),
             range('VARIABLES'^var) > 1                ]).

ctr_pure_functional_dependency(size_max_seq_alldifferent, []).
ctr_functional_dependency(size_max_seq_alldifferent, 1, [2]).

ctr_graph(size_max_seq_alldifferent,
          ['VARIABLES'],
          *,
          ['PATH_N'>>collection],
          [alldifferent(collection)],
          ['NARC' = 'SIZE'],
          []).

ctr_example(size_max_seq_alldifferent,
            [size_max_seq_alldifferent(4,[[var-2],[var-2],[var-4],[var-5],[var-2],[var-7],[var-4]]),
	     size_max_seq_alldifferent(1,[[var-2],[var-2],[var-2],[var-2],[var-2],[var-2],[var-2]]),
	     size_max_seq_alldifferent(2,[[var-2],[var-2],[var-4],[var-4],[var-4],[var-7],[var-4]]),
	     size_max_seq_alldifferent(7,[[var-2],[var-0],[var-4],[var-6],[var-5],[var-7],[var-3]])]).

ctr_see_also(size_max_seq_alldifferent,
 [link('implies',        atleast_nvalue,                     '',      []),
  link('common keyword', alldifferent,                       '%k,%k', ['all different', 'disequality']),
  link('common keyword', size_max_starting_seq_alldifferent, '%k,%k', ['all different', 'disequality']),
  link('common keyword', open_alldifferent,                  '%k,%k', ['all different', 'disequality'])]).

ctr_key_words(size_max_seq_alldifferent,
              ['sliding sequence constraint',
               'conditional constraint'     ,
               'sequence'                   ,
               'all different'              ,
               'disequality'                ,
               'hypergraph'                 ,
               'functional dependency'      ,
	       'pure functional dependency' ]).

ctr_persons(size_max_seq_alldifferent,['Beldiceanu N.']).

ctr_eval(size_max_seq_alldifferent,
         [checker(size_max_seq_alldifferent_c)      ,
	  reformulation(size_max_seq_alldifferent_r)]).

ctr_sol(size_max_seq_alldifferent,2,0,2,9,[1-3,2-6]).
ctr_sol(size_max_seq_alldifferent,3,0,3,64,[1-4,2-36,3-24]).
ctr_sol(size_max_seq_alldifferent,4,0,4,625,[1-5,2-200,3-300,4-120]).
ctr_sol(size_max_seq_alldifferent,5,0,5,7776,[1-6,2-1050,3-3480,4-2520,5-720]).
ctr_sol(size_max_seq_alldifferent,6,0,6,117649,[1-7,2-5922,3-38640,4-45360,5-22680,6-5040]).
ctr_sol(size_max_seq_alldifferent,7,0,7,2097152,[1-8,2-34104,3-428400,4-801360,5-571200,6-221760,7-40320]).
ctr_sol(size_max_seq_alldifferent,8,0,8,43046721,[1-9,2-208224,3-4981032,4-14028336,5-13728960,6-7378560,7-2358720,8-362880]).

% SIZE is the size of the maximal sequence (among all possible
% sequences of consecutive variables of the collection VARIABLES)
% for which the alldifferent constraint holds.
size_max_seq_alldifferent_c(SIZE, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    size_max_seq_alldifferent0(VARS, VARS, [], 0, SIZE).

size_max_seq_alldifferent0([X|Tail], Head, Set0, Size0, Size) :-
    fdset_add_element(Set0, X, Set),
    Set0\==Set, !,
    fdset_size(Set, Size1),
    Size2 is max(Size0,Size1),
    size_max_seq_alldifferent0(Tail, Head, Set, Size2, Size).
size_max_seq_alldifferent0(Tail, [X|Head], Set0, Size0, Size) :- !,
    fdset_del_element(Set0, X, Set),
    size_max_seq_alldifferent0(Tail, Head, Set, Size0, Size).
size_max_seq_alldifferent0(_, _, _, Size, Size).

size_max_seq_alldifferent_r(SIZE, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    collection(VARIABLES, [dvar]),
    size_max_seq_alldifferent1(VARIABLES, N, SIZES),
    eval(maximum(SIZE, SIZES)).

size_max_seq_alldifferent1([], _, []).
size_max_seq_alldifferent1([AV|R], N, [[var-SIZE]|S]) :-
    SIZE in 0..N,
    eval(size_max_starting_seq_alldifferent(SIZE, [AV|R])),
    N1 is N-1,
    size_max_seq_alldifferent1(R, N1, S).
