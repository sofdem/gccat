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

ctr_date(incomparable,['20120202']).

ctr_origin(incomparable, 'Inspired by incomparable rectangles.', []).

ctr_arguments(incomparable,
              ['VECTOR1'-collection(var-dvar),
               'VECTOR2'-collection(var-dvar)]).

ctr_exchangeable(incomparable,
                 [items('VECTOR1',all),
                  items('VECTOR2',all),
		  args([['VECTOR1','VECTOR2']])]).

ctr_synonyms(incomparable, [incomparables]).

ctr_restrictions(incomparable,
                 [required('VECTOR1',var)          ,
                  required('VECTOR2',var)          ,
                  size('VECTOR1') >=  1            ,
                  size('VECTOR2') >=  1            ,
                  size('VECTOR1') = size('VECTOR2')]).

ctr_typical(incomparable,
            [size('VECTOR1') > 1]).

ctr_predefined(incomparable).

ctr_example(incomparable,
            [incomparable([[var-16], [var-2]],
                          [[var-4], [var-11]])]).

ctr_cond_imply(incomparable, disjoint,                [size('VECTOR1') = 2], [], [same('VECTOR1'),same('VECTOR2')]).
ctr_cond_imply(incomparable, int_value_precede_chain, [size('VECTOR1') = 2], [], [same('VECTOR1'),same('VECTOR2')]).

ctr_see_also(incomparable,
 [link('implies',               lex_different,    '', []),
  link('system of constraints', all_incomparable, '', [])]).

ctr_key_words(incomparable,['predefined constraint',
			    'vector'               ]).

ctr_eval(incomparable, [reformulation(incomparable_r),
			checker(incomparable_c)]).

incomparable_r(VECTOR1, VECTOR2) :-
    collection(VECTOR1, [dvar]),
    collection(VECTOR2, [dvar]),
    length(VECTOR1, L),
    length(VECTOR2, L),
    get_attr1(VECTOR1, VECT1),
    get_attr1(VECTOR2, VECT2),
    incomparable(VECT1, VECT2).

incomparable(U, V) :-
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
    incomparable(SU, SV, Cond1),
    incomparable(SV, SU, Cond2),
    call(Cond1),
    call(Cond2),
    append(U, V, UV),
    append(PU, PV, PUV),
    when(ground(UV), once(labeling([], PUV))).

incomparable([], [], 0).
incomparable([U|R], [V|S], U#>V #\/ T) :-
    incomparable(R, S, T).

incomparable_c(VECTOR1, VECTOR2) :-
    collection(VECTOR1, [int]),
    collection(VECTOR2, [int]),
    length(VECTOR1, L),
    length(VECTOR2, L),
    get_attr1(VECTOR1, VECT1),
    get_attr1(VECTOR2, VECT2),
    incomparablec(VECT1, VECT2).
