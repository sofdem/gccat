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

ctr_date(vec_eq_tuple,['20030820','20060820']).

ctr_origin(vec_eq_tuple, 'Used for defining %c.', [in_relation]).

ctr_arguments(vec_eq_tuple,
              ['VARIABLES'-collection(var-dvar),
               'TUPLE'-collection(val-int)]).

ctr_exchangeable(vec_eq_tuple,
                 [args([['VARIABLES','TUPLE']]),
                  items_sync('VARIABLES','TUPLE',all)]).

ctr_restrictions(vec_eq_tuple,
                 [required('VARIABLES',var)        ,
                  required('TUPLE' ,val)           ,
                  size('VARIABLES') = size('TUPLE')]).

ctr_typical(vec_eq_tuple,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1,
             range('TUPLE'^val)     > 1]).

ctr_contractible(vec_eq_tuple, [], ['VARIABLES','TUPLE'], any).

ctr_graph(vec_eq_tuple,
          ['VARIABLES','TUPLE'],
          2,
          ['PRODUCT'(=)>>collection(variables,tuple)],
          [variables^var = tuple^val],
          ['NARC' = size('VARIABLES')],
          []).

ctr_example(vec_eq_tuple,
            vec_eq_tuple([[var-5], [var-3], [var-3]],
                         [[val-5], [val-3], [val-3]])).

ctr_draw_example(vec_eq_tuple,
                 ['VARIABLES','TUPLE'],
                 [[[var-5], [var-3], [var-3]],
                  [[val-5], [val-3], [val-3]]],
                 ['PRODUCT'(=)],
                 [1-1,2-2,3-3],
                 ['NARC'],
                 '','NARC=3',
                 [1.4,1.4,1.4,1.3]).

ctr_see_also(vec_eq_tuple,
 [link('implies',        lex_equal, '',                                     []),
  link('generalisation', lex_equal, '%e replaced by %e in second argument', [integer,variable])]).

ctr_key_words(vec_eq_tuple,['value constraint',
                            'tuple'           ,
                            'arc-consistency' ]).

ctr_eval(vec_eq_tuple, [reformulation(vec_eq_tuple_r)]).

vec_eq_tuple_r(VARIABLES, TUPLE) :-
    collection(VARIABLES, [dvar]),
    collection(TUPLE, [int]),
    length(VARIABLES, N),
    length(TUPLE, M),
    N = M,
    get_attr1(VARIABLES, VARS),
    get_attr1(TUPLE, VALS),
    vec_eq_tuple1(VARS, VALS).

vec_eq_tuple1([], []).
vec_eq_tuple1([VAR|R], [VAL|S]) :-
    VAR #= VAL,
    vec_eq_tuple1(R, S).
