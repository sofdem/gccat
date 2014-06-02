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

ctr_date(change_partition,['20000128','20030820','20040530','20060805']).

ctr_origin(change_partition, 'Derived from %c.', [change]).

ctr_types(change_partition,
          ['VALUES'-collection(val-int)]).

ctr_arguments(change_partition,
              ['NCHANGE'-dvar                     ,
               'VARIABLES'-collection(var-dvar)   ,
               'PARTITIONS'-collection(p-'VALUES')]).

ctr_exchangeable(change_partition,
                 [items('VARIABLES',reverse),
                  items('PARTITIONS',all),
                  items('PARTITIONS'^p,all),
                  vals(['VARIABLES'^var],part('PARTITIONS'),=,dontcare,dontcare)]).

ctr_restrictions(change_partition,
                 [size('VALUES') >= 1          ,
                  required('VALUES',val)       ,
                  distinct('VALUES',val)       ,
                  'NCHANGE' >= 0               ,
                  'NCHANGE' < size('VARIABLES'),
                  required('VARIABLES',var)    ,
                  required('PARTITIONS',p)     ,
                  size('PARTITIONS') >= 2      ]).

ctr_typical(change_partition,
            ['NCHANGE'              > 0                 ,
             size('VARIABLES')      > 1                 ,
             range('VARIABLES'^var) > 1                 ,
             size('VARIABLES')      > size('PARTITIONS')]).

ctr_pure_functional_dependency(change_partition, []).
ctr_functional_dependency(change_partition, 1, [2,3]).

ctr_graph(change_partition,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [in_same_partition(variables1^var,variables2^var,'PARTITIONS')],
          ['NARC' = 'NCHANGE'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(change_partition,
            change_partition(2,
                             [[var-6],[var-6],[var-2],[var-1],[var-3],[var-3],[var-1],[var-6],[var-2],[var-2],[var-2]],
                             [[p-[[val-1], [val-3]]],
                              [p-[[val-4]         ]],
                              [p-[[val-2], [val-6]]]])).

ctr_draw_example(change_partition,
                 ['VARIABLES'],
                 [[[var-6],[var-6],[var-2],[var-1],[var-3],[var-3],
                   [var-1],[var-6],[var-2],[var-2],[var-2]]],
                 ['PATH'],
                 [3-4,7-8],
                 ['NARC'],
                 '','NARC=2',
                 [2.145,5,2.145,1]).

ctr_see_also(change_partition,
 [link('common keyword',            change,            '%k in a sequence of %e with respect to a %e', ['number of changes',variables,'binary~constraint']),
  link('used in graph description', in_same_partition, '',                                            [])]).

ctr_key_words(change_partition,['timetabling constraint'    ,
                                'number of changes'         ,
                                'partition'                 ,
                                'acyclic'                   ,
                                'bipartite'                 ,
                                'no loop'                   ,
                                'functional dependency'     ,
				'pure functional dependency']).

ctr_persons(change_partition,['Beldiceanu N.',
                              'Carlsson M.'  ]).
