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

ctr_date(common_partition,['20030820','20060806']).

ctr_origin(common_partition, 'Derived from %c.', [common]).

ctr_types(common_partition,
          ['VALUES'-collection(val-int)]).

ctr_arguments(common_partition,
              ['NCOMMON1'-dvar                    ,
               'NCOMMON2'-dvar                    ,
               'VARIABLES1'-collection(var-dvar)  ,
               'VARIABLES2'-collection(var-dvar)  ,
               'PARTITIONS'-collection(p-'VALUES')]).

ctr_exchangeable(common_partition,
                 [args([['NCOMMON1','NCOMMON2'],['VARIABLES1','VARIABLES2'],['PARTITIONS']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  items('PARTITIONS',all),
                  items('PARTITIONS'^p,all),
                  vals(['VARIABLES1'^var],part('PARTITIONS'),=,dontcare,dontcare),
                  vals(['VARIABLES2'^var],part('PARTITIONS'),=,dontcare,dontcare)]).

ctr_restrictions(common_partition,
                 [size('VALUES') >= 1             ,
                  required('VALUES',val)          ,
                  distinct('VALUES',val)          ,
                  'NCOMMON1' >= 0                 ,
                  'NCOMMON1' =< size('VARIABLES1'),
                  'NCOMMON2' >= 0                 ,
                  'NCOMMON2' =< size('VARIABLES2'),
                  required('VARIABLES1',var)      ,
                  required('VARIABLES2',var)      ,
                  required('PARTITIONS',p)        ,
                  size('PARTITIONS') >= 2         ]).

ctr_typical(common_partition,
            [size('VARIABLES1')      > 1                 ,
             range('VARIABLES1'^var) > 1                 ,
             size('VARIABLES2')      > 1                 ,
             range('VARIABLES2'^var) > 1                 ,
             size('VARIABLES1')      > size('PARTITIONS'),
             size('VARIABLES2')      > size('PARTITIONS')]).

ctr_pure_functional_dependency(common_partition, []).
ctr_functional_dependency(common_partition, 1, [3,4,5]).
ctr_functional_dependency(common_partition, 2, [3,4,5]).

ctr_graph(common_partition,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [in_same_partition(variables1^var,variables2^var,'PARTITIONS')],
          ['NSOURCE' = 'NCOMMON1',
           'NSINK'   = 'NCOMMON2'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(common_partition,
            common_partition(3,4,
                             [[var-2], [var-3], [var-6], [var-0]],
                             [[var-0], [var-6], [var-3], [var-3], [var-7], [var-1]],
                             [[p-[[val-1], [val-3]]],
                              [p-[[val-4]         ]],
                              [p-[[val-2], [val-6]]]])).

ctr_draw_example(common_partition,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-2], [var-3], [var-6], [var-0]],
                  [[var-0], [var-6], [var-3], [var-3], [var-7], [var-1]]],
                 ['PRODUCT'],
                 [1-2,
                  2-[3,4,6],
                  3-2],
                 ['NSOURCE'([1,2,3]), 'NSINK'([6,7,8,10])],
                 '','NSOURCE=3,NSINK=4',
                 [2.3,2.145,2.145,1.4]).

ctr_see_also(common_partition,
 [link(specialisation,              common,            '%e replaced by %e', [in_list(variable,partition),variable]),
  link('used in graph description', in_same_partition, '',                  [])]).

ctr_key_words(common_partition,['constraint between two collections of variables',
                                'partition'                                      ,
                                'acyclic'                                        ,
                                'bipartite'                                      ,
                                'no loop'                                        ,
                                'functional dependency'                          ,
				'pure functional dependency'                     ]).

ctr_eval(common_partition, [reformulation(common_partition_r)]).

common_partition_r(NCOMMON1, NCOMMON2, VARIABLES1, VARIABLES2, PARTITIONS) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    length(VARIABLES1, N1),
    length(VARIABLES2, N2),
    check_type(dvar(0,N1), NCOMMON1),
    check_type(dvar(0,N2), NCOMMON2),
    collection(PARTITIONS, [col_len_gteq(1, [int])]),
    length(PARTITIONS, P),
    P > 1,
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    get_col_attr1(PARTITIONS, 1, PVALS),
    flattern(PVALS, VALS),
    all_different(VALS),
    length(PVALS, LPVALS),
    LPVALS1 is LPVALS+1,
    get_partition_var(VARS1, PVALS, PVARS1, LPVALS1, 0),
    LPVALS2 is LPVALS1+1,
    get_partition_var(VARS2, PVALS, PVARS2, LPVALS2, LPVALS1),
    common1(PVARS1, PVARS2, _MAT12, SUM1),
    call(NCOMMON1 #= SUM1),
    common1(PVARS2, PVARS1, _MAT21, SUM2),
    call(NCOMMON2 #= SUM2).
