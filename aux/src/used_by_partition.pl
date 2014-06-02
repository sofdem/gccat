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

ctr_date(used_by_partition,['20030820','20060820']).

ctr_origin(used_by_partition, 'Derived from %c.', [used_by]).

ctr_types(used_by_partition,
          ['VALUES'-collection(val-int)]).

ctr_arguments(used_by_partition,
              ['VARIABLES1'-collection(var-dvar)  ,
               'VARIABLES2'-collection(var-dvar)  ,
               'PARTITIONS'-collection(p-'VALUES')]).

ctr_exchangeable(used_by_partition,
                 [items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  items('PARTITIONS',all),
                  items('PARTITIONS'^p,all),
                  vals(['VARIABLES1'^var],part('PARTITIONS'),=,dontcare,dontcare),
                  vals(['VARIABLES2'^var],part('PARTITIONS'),=,dontcare,dontcare)]).

ctr_restrictions(used_by_partition,
                 [size('VALUES') >= 1                     ,
                  required('VALUES',val)                  ,
                  distinct('VALUES',val)                  ,
                  size('VARIABLES1') >= size('VARIABLES2'),
                  required('VARIABLES1',var)              ,
                  required('VARIABLES2',var)              ,
                  required('PARTITIONS',p)                ,
                  size('PARTITIONS') >= 2                 ]).

ctr_typical(used_by_partition,
            [size('VARIABLES1')      > 1                 ,
             range('VARIABLES1'^var) > 1                 ,
             size('VARIABLES2')      > 1                 ,
             range('VARIABLES2'^var) > 1                 ,
             size('VARIABLES1')      > size('PARTITIONS'),
             size('VARIABLES2')      > size('PARTITIONS')]).

% used_by_partition('VARIABLES11', 'VARIABLES12', 'PARTITIONS') and
% used_by_partition('VARIABLES21', 'VARIABLES22', 'PARTITIONS') =>
% used_by_partition(union('VARIABLES11','VARIABLES21'), union('VARIABLES12','VARIABLES22'), 'PARTITIONS')
ctr_aggregate(used_by_partition, [], [union, union, id]).

ctr_contractible(used_by_partition, [], 'VARIABLES2', any).

ctr_extensible(used_by_partition, [], 'VARIABLES1', any).

ctr_graph(used_by_partition,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [in_same_partition(variables1^var,variables2^var,'PARTITIONS')],
          [for_all('CC','NSOURCE' >= 'NSINK'),
           'NSINK' = size('VARIABLES2')],
          []).

ctr_example(used_by_partition,
            used_by_partition([[var-1],[var-9],[var-1],[var-6],[var-2],[var-3]],
                              [[var-1],[var-3],[var-6],[var-6]],
                              [[p-[[val-1], [val-3]]],
                               [p-[[val-4]         ]],
                               [p-[[val-2], [val-6]]]])).

ctr_draw_example(used_by_partition,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-6],[var-2],[var-3]],
                  [[var-1],[var-3],[var-6],[var-6]]],
                 ['PRODUCT'],
                 [1-[1,2],
                  3-[1,2],
                  4-[3,4],
                  5-[3,4],
                  6-[1,2]],
                 ['NSINK'([7,8,9,10]),
                  'NCC'([[1,3,6,7,8],[4,10],[5,9]])],
                 '','CC#1:NSINK=2,CC#2:NSINK=2\\nNSINK=4',
                 [3,2.145,3.1,2.145]).

ctr_see_also(used_by_partition,
 [link('soft variant',              soft_used_by_partition_var, '%k',                ['variable-based violation measure']),
  link('implied by',                same_partition,             '',                  []),
  link('system of constraints',     k_used_by_partition,        '',                  []),
  link('specialisation',            used_by,                    '%e replaced by %e', [in_list(variable,partition),variable]),
  link('used in graph description', in_same_partition,          '',                  [])]).

ctr_key_words(used_by_partition,['constraint between two collections of variables',
                                 'inclusion'                                      ,
                                 'partition'                                      ,
                                 'sort based reformulation'                       ]).

ctr_eval(used_by_partition, [reformulation(used_by_partition_r)]).

used_by_partition_r(VARIABLES1, VARIABLES2, PARTITIONS) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
    collection(PARTITIONS, [col_len_gteq(1, [int])]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
    N1 >= N2,
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    get_col_attr1(PARTITIONS, 1, PVALS),
	flattern(PVALS, VALS),
	all_different(VALS),
	length(PARTITIONS, P),
    P > 1,
	length(PVALS, LPVALS),
	LPVALS1 is LPVALS+1,
	get_partition_var(VARS1, PVALS, PVARS1, LPVALS1, 0),
	get_partition_var(VARS2, PVALS, PVARS2, LPVALS1, 0),
    used_by_reified(PVARS2, PVARS1, PVARS2).
