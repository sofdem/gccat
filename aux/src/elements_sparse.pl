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

ctr_date(elements_sparse,['20030820','20060809']).

ctr_origin(elements_sparse, 'Derived from %c.', [element_sparse]).

ctr_arguments(elements_sparse,
              ['ITEMS'-collection(index-dvar, value-dvar),
               'TABLE'-collection(index-int , value-int ),
               'DEFAULT'-int]).

ctr_exchangeable(elements_sparse,
                 [items('ITEMS',all),
                  items('TABLE',all),
                  vals(['ITEMS'^value,'TABLE'^value,'DEFAULT'],int,=\=,all,dontcare)]).

ctr_restrictions(elements_sparse,
                 [required('ITEMS',[index,value]),
                  'ITEMS'^index >= 1             ,
                  required('TABLE',[index,value]),
                  'TABLE'^index >= 1             ,
                  distinct('TABLE',index)        ]).

ctr_typical(elements_sparse,
            [size('ITEMS')        > 1,
             range('ITEMS'^value) > 1,
             size('TABLE')        > 1,
             range('TABLE'^value) > 1]).

ctr_derived_collections(elements_sparse,
                        [col('DEF'-collection(index-int, value-int),
                             [item(index-0, value-'DEFAULT')]),
                         col('TABLE_DEF'-collection(index-dvar,value-dvar),
                             [item(index-'TABLE'^index, value-'TABLE'^index),
                              item(index-'DEF'^index  , value-'DEF'^value  )])
                        ]).

ctr_graph(elements_sparse, 
          ['ITEMS','TABLE_DEF'],
          2,
          ['PRODUCT'>>collection(items,table_def)],
          [items^value = table_def^value                        ,
           items^index = table_def^index #\/ table_def^index = 0],
          ['NSOURCE' = size('ITEMS')],
          []).

ctr_example(elements_sparse,
            elements_sparse([[index-8, value-9],
                             [index-3, value-5],
                             [index-2, value-5]],
                            [[index-1, value-6],
                             [index-2, value-5],
                             [index-4, value-2],
                             [index-8, value-9]],
                            5)).

ctr_draw_example(elements_sparse,
                 ['ITEMS','TABLE_DEF'],
                 [[[index-8, value-9],
                   [index-3, value-5],
                   [index-2, value-5]],
                  [[index-1, value-6],
                   [index-2, value-5],
                   [index-4, value-2],
                   [index-8, value-9],
                   [index-0, value-5]]],
                 ['PRODUCT'],
                 [1-4,2-5,3-[2,5]],
                 ['NSOURCE'([1,2,3])],
                 '','NSOURCE=3',
                 [2.145,2.145,2,1.6]).

ctr_see_also(elements_sparse,
 [link('implied by',                    element_sparse, '',   []),
  link('part of system of constraints', element_sparse, '',   []),
  link('common keyword',                element_sparse, '%k', ['sparse table']),
  link('common keyword',                element,        '%k', ['data constraint']),
  link('common keyword',                elem,           '%k', ['data constraint'])]).

ctr_key_words(elements_sparse, ['data constraint'             ,
                                'system of constraints'       ,
                                'table'                       ,
                                'shared table'                ,
                                'sparse table'                ,
                                'sparse functional dependency',
                                'derived collection'          ,
                                'arc-consistency'             ]).

ctr_eval(elements_sparse, [reformulation(elements_sparse_r)]).

elements_sparse_r(ITEMS, TABLE, DEFAULT) :-
    collection(ITEMS, [dvar_gteq(1),dvar]),
    collection(TABLE, [ int_gteq(1),dvar]),
    check_type(int, DEFAULT),
    get_attr1(ITEMS, IS),
    get_attr2(ITEMS, VS),
    get_attr1(TABLE, INDEXES),
    get_attr2(TABLE, VALUES ),
    all_different(INDEXES),
    elements_sparse1(IS, VS, INDEXES, VALUES, DEFAULT).

elements_sparse1([], [], _, _, _).
elements_sparse1([I|R], [V|S], INDEXES, VALUES, DEFAULT) :-
    elements_sparse2(INDEXES, VALUES, I, V, DEFAULT, Term1, Term2),
    call(Term1 #\/ Term2),
    elements_sparse1(R, S, INDEXES, VALUES, DEFAULT).

elements_sparse2([], [], _, V, DEFAULT, 0, V#=DEFAULT).
elements_sparse2([IND|R], [VAL|S], I, V, DEFAULT, (I#=IND #/\ V#=VAL) #\/ T, I#\=IND #/\ U) :-
    elements_sparse2(R, S, I, V, DEFAULT, T, U).
