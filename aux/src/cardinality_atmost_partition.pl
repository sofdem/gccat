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

ctr_date(cardinality_atmost_partition,['20030820','20060805']).

ctr_origin(cardinality_atmost_partition, 'Derived from %c.', [global_cardinality]).

ctr_types(cardinality_atmost_partition,
          ['VALUES'-collection(val-int)]).

ctr_arguments(cardinality_atmost_partition,
              ['ATMOST'-dvar                      ,
               'VARIABLES'-collection(var-dvar)   ,
               'PARTITIONS'-collection(p-'VALUES')]).

ctr_exchangeable(cardinality_atmost_partition,
                 [items('VARIABLES',all),
                  items('PARTITIONS',all),
                  items('PARTITIONS'^p,all)]).

ctr_restrictions(cardinality_atmost_partition,
                 [size('VALUES') >= 1          ,
                  required('VALUES',val)       ,
                  distinct('VALUES',val)       ,
                  'ATMOST' >= 0                ,
                  'ATMOST' =< size('VARIABLES'),
                  required('VARIABLES',var)    ,
                  required('PARTITIONS',p)     ,
                  size('PARTITIONS') >= 2      ]).

ctr_typical(cardinality_atmost_partition,
            ['ATMOST' > 0                          ,
             'ATMOST' < size('VARIABLES')          ,
             size('VARIABLES') > 1                 ,
             size('VARIABLES') > size('PARTITIONS')]).

ctr_pure_functional_dependency(cardinality_atmost_partition, []).
ctr_functional_dependency(cardinality_atmost_partition, 1, [2,3]).

ctr_graph(cardinality_atmost_partition,
          ['VARIABLES','PARTITIONS'],
          2,
          ['PRODUCT'>>collection(variables,partitions)],
          [in(variables^var,partitions^p)],
          ['MAX_ID' = 'ATMOST'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(cardinality_atmost_partition,
            cardinality_atmost_partition(2,
                                         [[var-2],[var-3],[var-7],[var-1],[var-6],[var-0]],
                                         [[p-[[val-1], [val-3]]],
                                          [p-[[val-4]         ]],
                                          [p-[[val-2], [val-6]]]])).

ctr_draw_example(cardinality_atmost_partition,
                 ['VARIABLES','PARTITIONS'],
                 [[[var-2],[var-3],[var-7],[var-1],[var-6],[var-0]],
                  [[p-[[val-1], [val-3]]],
                   [p-[[val-4]         ]],
                   [p-[[val-2], [val-6]]]]],
                 ['PRODUCT'],
                 [1-3,2-1,4-1,5-3],
                 ['MAX_ID'([7])],
                 '','MAX_ID=2',
                 [2.145,2.5,2.145,1.5]).

ctr_see_also(cardinality_atmost_partition,
 [link(generalisation             , global_cardinality, 'single %e replaced by an individual %e for each value and %e replaced by %e', ['count~variable','count~variable',variable,in_list(variable,partition)]),
  link('used in graph description', in,                 '',                                                                            [])]).

ctr_key_words(cardinality_atmost_partition,['value constraint'          ,
                                            'partition'                 ,
                                            'at most'                   ,
                                            'acyclic'                   ,
                                            'bipartite'                 ,
                                            'no loop'                   ,
                                            'arc-consistency'           ,
                                            'functional dependency'     ,
					    'pure functional dependency']).

ctr_eval(cardinality_atmost_partition, [reformulation(cardinality_atmost_partition_r)]).

cardinality_atmost_partition_r(ATMOST, VARIABLES, PARTITIONS) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    check_type(dvar(0,N), ATMOST),
    collection(PARTITIONS, [col_len_gteq(1, [int])]),
    length(PARTITIONS, P),
    P > 1,
    get_attr1(VARIABLES, VARS),
    get_col_attr1(PARTITIONS, 1, PVALS),
    flattern(PVALS, VALS),
    all_different(VALS),
    length(PVALS, LPVALS),
    LPVALS1 is LPVALS+1,
    get_partition_var(VARS, PVALS, PVARS, LPVALS1, 0),
    complete_card_consec(1, LPVALS1, ATMOST, N, VALUES),
    global_cardinality(PVARS, VALUES).
