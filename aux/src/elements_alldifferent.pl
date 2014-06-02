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

ctr_date(elements_alldifferent,['20030820','20060809']).

ctr_origin(elements_alldifferent, 'Derived from %c and %c.', [elements,alldifferent]).

ctr_arguments(elements_alldifferent,
              ['ITEMS'-collection(index-dvar, value-dvar),
               'TABLE'-collection(index-int , value-dvar)]).

ctr_exchangeable(elements_alldifferent,
                 [args([['ITEMS','TABLE']]),
                  items('ITEMS',all),
                  items('TABLE',all),
                  vals(['ITEMS'^value,'TABLE'^value],int,=\=,all,dontcare)]).

ctr_synonyms(elements_alldifferent,[elements_alldiff    ,
                                    elements_alldistinct]).

ctr_restrictions(elements_alldifferent,
                 [required('ITEMS',[index,value]),
                  'ITEMS'^index >= 1             ,
                  'ITEMS'^index =< size('TABLE') ,
                  size('ITEMS') = size('TABLE')  ,
                  required('TABLE',[index,value]),
                  'TABLE'^index >= 1             ,
                  'TABLE'^index =< size('TABLE') ,
                  distinct('TABLE',index)        ]).

ctr_typical(elements_alldifferent,
            [size('ITEMS')        > 1,
             range('ITEMS'^value) > 1,
             size('TABLE')        > 1,
             range('TABLE'^value) > 1]).

ctr_functional_dependency(elements_alldifferent, 1-2, [1-1,2]).

ctr_graph(elements_alldifferent, 
          ['ITEMS','TABLE'],
          2,
          ['PRODUCT'>>collection(items,table)],
          [items^index = table^index,
           items^value = table^value],
          ['NVERTEX' = size('ITEMS')+size('TABLE')],
          []).

ctr_example(elements_alldifferent,
            elements_alldifferent([[index-2, value-9],
                                   [index-1, value-6],
                                   [index-4, value-9],
                                   [index-3, value-2]],
                                  [[index-1, value-6],
                                   [index-2, value-9],
                                   [index-3, value-2],
                                   [index-4, value-9]])).

ctr_draw_example(elements_alldifferent,
                 ['ITEMS','TABLE'],
                 [[[index-2, value-9],
                   [index-1, value-6],
                   [index-4, value-9],
                   [index-3, value-2]],
                  [[index-1, value-6],
                   [index-2, value-9],
                   [index-3, value-2],
                   [index-4, value-9]]],
                 ['PRODUCT'],
                 [1-2,2-1,3-4,4-3],
                 ['NVERTEX'],
                 '','NVERTEX=8',
                 [2.145,2.145,2.145,1.8]).

ctr_cond_imply(elements_alldifferent, bin_packing_capa, ['TABLE'^value >= 0], [], ['TABLE','ITEMS']).

ctr_see_also(elements_alldifferent,
 [link('implies',               elements,     '',   []),
  link('implies',               indexed_sum,  '',   []),
  link('used in reformulation', element,      '',   []),
  link('used in reformulation', elem,         '',   []),
  link('used in reformulation', alldifferent, '',   [])]).

ctr_key_words(elements_alldifferent,['array constraint'     ,
                                     'data constraint'      ,
                                     'table'                ,
                                     'functional dependency',
                                     'permutation'          ,
                                     'disequality'          ]).

ctr_eval(elements_alldifferent, [reformulation(elements_alldifferent_r)]).

elements_alldifferent_r(ITEMS, TABLE) :-
    length(TABLE, N),
    collection(ITEMS, [dvar(1,N),dvar]),
    collection(TABLE, [ int(1,N),dvar]),
    get_attr1(TABLE, INDEXES),
    all_different(INDEXES),
    sort(TABLE, STAB),
    get_attr2(STAB, VALUES),
    get_attr1(ITEMS, INDS),
    get_attr2(ITEMS, VALS),
    all_different(INDS),
    elements_alldifferent1(INDS, VALS, VALUES).

elements_alldifferent1([], [], _).
elements_alldifferent1([IND|R], [VAL|S], VALUES) :-
    element(IND, VALUES, VAL),
    elements_alldifferent1(R, S, VALUES).
