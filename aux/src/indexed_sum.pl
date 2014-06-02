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

ctr_date(indexed_sum,['20040814','20060810','20090422']).

ctr_origin(indexed_sum, 'N.~Beldiceanu', []).

ctr_arguments(indexed_sum,
              ['ITEMS'-collection(index-dvar, weight-dvar)  ,
               'TABLE'-collection(index-int, summation-dvar)]).

ctr_exchangeable(indexed_sum,
                 [items('ITEMS',all),
                  items('TABLE',all)]).

ctr_restrictions(indexed_sum,
                 [size('ITEMS') >  0                 ,
                  size('TABLE') >  0                 ,
                  required('ITEMS',[index,weight])   ,
                  'ITEMS'^index >= 1                 ,
                  'ITEMS'^index =< size('TABLE')     ,
                  required('TABLE',[index,summation]),
                  'TABLE'^index >= 1                 ,
                  'TABLE'^index =< size('TABLE')     ,
                  increasing_seq('TABLE',index)      ]).

ctr_typical(indexed_sum,
            [size('ITEMS')            > 1,
             range('ITEMS'^index)     > 1,
             size('TABLE')            > 1,
             range('TABLE'^summation) > 1]).

ctr_graph(indexed_sum,
          ['ITEMS','TABLE'],
          2,
          foreach('TABLE',['PRODUCT'>>collection(items,table)]),
          [items^index = table^index],
          [],
          [],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'ITEMS'^weight)])]],
          [sum_ctr(variables,=,'TABLE'^summation)]).

ctr_example(indexed_sum,
            indexed_sum([[index-3, weight-(-4)   ],
                         [index-1, weight-6      ],
                         [index-3, weight-1      ]],
                        [[index-1, summation-6   ],
                         [index-2, summation-0   ],
                         [index-3, summation-(-3)]])).

ctr_draw_example(indexed_sum,
                 ['ITEMS','TABLE'],
                 [[[index-3, weight-(-4)   ],
                   [index-1, weight-6      ],
                   [index-3, weight-1      ]],
                  [[index-1, summation-6   ],
                   [index-2, summation-0   ],
                   [index-3, summation-(-3)]]],
                 ['PRODUCT'],
                 [1-3,
                  2-1,
                  3-3],
                 ['FOREACH'('TABLE',[1-[2,4],
                                     2-[],
                                     3-[1,3,6]])],
                 '','',
                 [2.145,1.7,2.145,1.7]).

ctr_see_also(indexed_sum,
 [link('implied by',                elements_alldifferent,
       '',                                                                                                                 []),
  link('specialisation',            bin_packing,
       'negative contribution not allowed, effective use variable for each bin replaced by an overall fixed capacity',     []),
  link('specialisation',            bin_packing_capa,
       'negative contribution not allowed, effective use variable for each bin replaced by a fixed capacity for each bin', []),
  link('used in graph description', sum_ctr,
       '',                                                                                                                 [])]).

ctr_key_words(indexed_sum,['assignment'        ,
                           'variable indexing' ,
                           'variable subscript']).

ctr_persons(indexed_sum,['Beldiceanu N.']).

ctr_eval(indexed_sum, [reformulation(indexed_sum_r)]).

indexed_sum_r(ITEMS, TABLE) :-
    length(ITEMS, I),
    length(TABLE, T),
    I > 0,
    T > 0,
    collection(ITEMS, [dvar(1,T),dvar]),
    collection(TABLE, [int(1,T),dvar]),
    collection_increasing_seq(TABLE,[1]),
    get_attr1(ITEMS, ITEMS_INDEXES),
    get_attr2(ITEMS, ITEMS_WEIGHTS),
    get_attr2(TABLE, TABLE_TSUMS),
    indexed_sum1(1, T, TABLE_TSUMS, ITEMS_INDEXES, ITEMS_WEIGHTS).

indexed_sum1(I, T, [], _, _) :-
    I > T, !.
indexed_sum1(I, T, [SUM|R], ITEMS_INDEXES, ITEMS_WEIGHTS) :-
    indexed_sum2(ITEMS_INDEXES, ITEMS_WEIGHTS, I, TERM),
    call(SUM #= TERM),
    I1 is I+1,
    indexed_sum1(I1, T, R, ITEMS_INDEXES, ITEMS_WEIGHTS).

indexed_sum2([], [], _, 0).
indexed_sum2([J|R], [W|S], I, W*B+T) :-
    B #<=> J #= I,
    indexed_sum2(R, S, I, T).
