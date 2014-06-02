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

ctr_date(in_relation,['20030820','20040530','20060810']).

ctr_origin(in_relation, 'Constraint explicitly defined by tuples of values.', []).

ctr_types(in_relation,
          ['TUPLE_OF_VARS'-collection(var-dvar),
           'TUPLE_OF_VALS'-collection(val-int) ]).

ctr_arguments(in_relation,
              ['VARIABLES'-'TUPLE_OF_VARS'                       ,
               'TUPLES_OF_VALS'-collection(tuple-'TUPLE_OF_VALS')]).

ctr_exchangeable(in_relation,
                 [items('TUPLES_OF_VALS',all),
                  items_sync('VARIABLES','TUPLES_OF_VALS'^tuple,all),
                  vals(['VARIABLES','TUPLES_OF_VALS'^tuple],int,=\=,all,dontcare)]).

ctr_synonyms(in_relation, [case                  ,
                           extension             ,
                           extensional           ,
                           extensional_support   ,
                           extensional_supportva ,
                           extensional_supportmdd,
                           extensional_supportstr,
                           feastupleac           ,
                           table                 ]).

ctr_restrictions(in_relation,
                 [required('TUPLE_OF_VARS',var)            ,
                  size('TUPLE_OF_VARS') >= 1               ,
                  size('TUPLE_OF_VALS') >= 1               ,
                  size('TUPLE_OF_VALS') = size('VARIABLES'),
                  required('TUPLE_OF_VALS',val)            ,
                  required('TUPLES_OF_VALS',tuple)         ]).

ctr_typical(in_relation,
            [size('TUPLE_OF_VARS') > 1]).

ctr_extensible(in_relation, [], 'TUPLES_OF_VALS', any).

ctr_derived_collections(in_relation,
                        [col('TUPLES_OF_VARS'-collection(vec-'TUPLE_OF_VARS'),
                             [item(vec-'VARIABLES')])]).

ctr_graph(in_relation,
          ['TUPLES_OF_VARS','TUPLES_OF_VALS'],
          2,
          ['PRODUCT'>>collection(tuples_of_vars,tuples_of_vals)],
          [vec_eq_tuple(tuples_of_vars^vec,tuples_of_vals^tuple)],
          ['NARC' >= 1],
          []).

ctr_example(in_relation,
            in_relation([[var-5], [var-3], [var-3]],
                        [[tuple-[[val-5], [val-2], [val-3]]],
                         [tuple-[[val-5], [val-2], [val-6]]],
                         [tuple-[[val-5], [val-3], [val-3]]]])).

ctr_draw_example(in_relation,
                 ['TUPLES_OF_VARS','TUPLES_OF_VALS'],
                 [[[vec-[[var-5], [var-3], [var-3]]]],
                  [[tuple-[[val-5], [val-2], [val-3]]],
                   [tuple-[[val-5], [val-2], [val-6]]],
                   [tuple-[[val-5], [val-3], [val-3]]]]],
                 ['PRODUCT'],
                 [1-3],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,2.145,2.145,1.9]).

ctr_see_also(in_relation,
 [link('cost variant',              cond_lex_cost, '%e parameter added', ['COST']),
  link('common keyword',            element,       '%k',                 ['data constraint']),
  link('used in graph description', vec_eq_tuple,  '',                   [])]).

ctr_key_words(in_relation,['data constraint'   ,
                           'tuple'             ,
                           'extension'         ,
                           'relation'          ,
                           'derived collection',
                           'arc-consistency'   ]).

ctr_persons(in_relation,['Bourdais S.',
                         'Galinier P.',
                         'Pesant G.'  ]).

ctr_eval(in_relation, [reformulation(in_relation_r)]).

in_relation_r(VARIABLES, TUPLES_OF_VALS) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    collection(TUPLES_OF_VALS, [col(N, [int])]),
    get_attr1(VARIABLES, VARS),
	get_col_attr1(TUPLES_OF_VALS, 1, TUPLES),
	table([VARS], TUPLES).
