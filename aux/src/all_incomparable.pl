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

ctr_date(all_incomparable,['20120202']).

ctr_origin(all_incomparable, 'Inspired by incomparable rectangles.', []).

ctr_types(all_incomparable, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(all_incomparable,
              ['VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(all_incomparable,
                 [items('VECTORS',all)]).

ctr_synonyms(all_incomparable, [all_incomparables]).

ctr_restrictions(all_incomparable,
                 [required('VECTOR',var)  ,
                  size('VECTOR') >=  1    ,
                  required('VECTORS',vec) ,
                  size('VECTORS') >=  1   ,
                  same_size('VECTORS',vec)]).

ctr_typical(all_incomparable,
            [size('VECTOR')  > 1             ,
             size('VECTORS') > 1             ,
	     size('VECTORS') > size('VECTOR')]).

ctr_contractible(all_incomparable, [], 'VECTORS', any).

ctr_graph(all_incomparable,
          ['VECTORS'],
          2,
          ['CLIQUE'(=\=)>>collection(vectors1,vectors2)],
          [incomparable(vectors1^vec,vectors2^vec)],
          ['NARC' = size('VECTORS')*size('VECTORS') - size('VECTORS')],
          ['NO_LOOP','SYMMETRIC']).

ctr_example(all_incomparable,
            all_incomparable([[vec-[[var-1], [var-18]]],
                              [vec-[[var-2], [var-16]]],
                              [vec-[[var-3], [var-13]]],
                              [vec-[[var-4], [var-11]]],
                              [vec-[[var-5], [var-10]]],
                              [vec-[[var-6], [var-9]]],
			      [vec-[[var-7], [var-7]]]])).

ctr_draw_example(all_incomparable,
                 ['VECTORS'],
                 [[[vec-[[var-1], [var-18]]],
                   [vec-[[var-2], [var-16]]],
                   [vec-[[var-3], [var-13]]],
                   [vec-[[var-4], [var-11]]],
                   [vec-[[var-5], [var-10]]],
                   [vec-[[var-6], [var-9]]],
                   [vec-[[var-7], [var-7]]]]],
                 ['CLIQUE'(=\=)],
                 [1-[2,3,4,5,6,7], 2-[1,3,4,5,6,7], 3-[1,2,4,5,6,7], 4-[1,2,3,5,6,7],
		  5-[1,2,3,4,6,7], 6-[1,2,3,4,5,7], 7-[1,2,3,4,5,6]],
                 ['NARC'],
                  '','NARC=42',
                 [2.145,2.145,2.145,2.145]).

ctr_cond_imply(all_incomparable, k_disjoint, [size('VECTOR') = 2], [], same).
ctr_cond_imply(all_incomparable, twin,       [size('VECTOR') = 2], [], same).

ctr_see_also(all_incomparable,
 [link('implies',                       lex_alldifferent, '', []),
  link('part of system of constraints', incomparable,     '', []),
  link('used in graph description',     incomparable,     '', [])]).

ctr_key_words(all_incomparable,['system of constraints',
                                'decomposition'        ,
				'vector'               ,
				'no loop'              ,
				'symmetric'            ]).

ctr_eval(all_incomparable, [reformulation(all_incomparable_r),
			    checker(all_incomparable_c)]).

all_incomparable_r(VECTORS) :-
    collection(VECTORS, [col([dvar])]),
    same_size(VECTORS),
    get_attr11(VECTORS, VECTS),
    VECTS = [VEC|_],
    length(VEC, N),
    N >= 1,
    all_incomparable(VECTS, N).

all_incomparable([_], _) :- !.     % succeed if only one vector
all_incomparable(_, 1) :- !, fail. % fail if more than one vector and one single component
all_incomparable(VECTS, _) :-      % check each pair of vectors
	all_incomparable1(VECTS, NEW_PVARS),
	flattern(VECTS, VARS),
	when(ground(VARS), once(labeling([], NEW_PVARS))).

all_incomparable1([], []).
all_incomparable1([_], []).
all_incomparable1([V,W|R], P) :-
	all_incomparable2([W|R], V, P1),
	all_incomparable1([W|R], P2),
	append(P1, P2, P).

all_incomparable2([], _, []).
all_incomparable2([V|R], U, P) :-
	all_incomparable3(U, V, PUV),
	all_incomparable2(R, U, PR),
	append(PUV, PR, P).

all_incomparable3(U, V, PUV) :-
	length(U, N),
	length(V, N),
	N > 1,
	length(PU, N),
	length(PV, N),
	domain(PU, 1, N),
	domain(PV, 1, N),
	get_minimum(U, MinU),
	get_maximum(U, MaxU),
	get_minimum(V, MinV),
	get_maximum(V, MaxV),
	length(SU, N),
	length(SV, N),
	domain(SU, MinU, MaxU),
	domain(SV, MinV, MaxV),
	sorting(U, PU, SU),
	sorting(V, PV, SV),
	all_incomparable4(SU, SV, Cond1),
	all_incomparable4(SV, SU, Cond2),
	call(Cond1),
	call(Cond2),
	append(PU, PV, PUV).

all_incomparable4([], [], 0).
all_incomparable4([U|R], [V|S], U#>V #\/ T) :-
	all_incomparable4(R, S, T).

all_incomparable_c(VECTORS) :-
    collection(VECTORS, [col([int])]),
    same_size(VECTORS),
    get_attr11(VECTORS, VECTS),
    VECTS = [VEC|_],
    length(VEC, N),
    N >= 1,
    all_incomparable_check(VECTS, N).

all_incomparable_check([_], _) :- !.     % succeed if only one vector
all_incomparable_check(_, 1) :- !, fail. % fail if more than one vector and one single component
all_incomparable_check(VECTS, _) :-      % check each pair of vectors
	all_incomparablec1(VECTS).

all_incomparablec1([]) :- !.
all_incomparablec1([_]) :- !.
all_incomparablec1([V|R]) :-
	all_incomparablec2(R, V),
	all_incomparablec1(R).

all_incomparablec2([], _) :- !.
all_incomparablec2([V|R], U) :-
	incomparablec(U, V),
	all_incomparablec2(R, U).
