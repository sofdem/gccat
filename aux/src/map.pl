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

ctr_date(map,['20000128','20030820','20060811']).

ctr_origin(map, 'Inspired by \\cite{SedgewickFlajolet96}', []).

ctr_arguments(map,
              ['NBCYCLE'-dvar                          ,
               'NBTREE'-dvar                           ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(map,
                 [items('NODES',all)]).

ctr_restrictions(map,
                 ['NBCYCLE'     >= 0            ,
                  'NBTREE'      >= 0            ,
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(map,
            ['NBCYCLE'     > 0            ,
             'NBTREE'      > 0            ,
             'NBCYCLE'     < size('NODES'),
             'NBCYCLE'     < 'NBTREE'     ,
             size('NODES') > 2            ]).

ctr_pure_functional_dependency(map, []).
ctr_functional_dependency(map, 1, [3]).
ctr_functional_dependency(map, 2, [3]).

ctr_graph(map,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['NCC'   = 'NBCYCLE',
           'NTREE' = 'NBTREE' ],
          []).

ctr_example(map,
            map(2,3,
                [[index-1, succ-5],
                 [index-2, succ-9],
                 [index-3, succ-8],
                 [index-4, succ-2],
                 [index-5, succ-9],
                 [index-6, succ-2],
                 [index-7, succ-9],
                 [index-8, succ-8],
                 [index-9, succ-1]])).

ctr_draw_example(map,
                 ['NODES'],
                 [[[index-1, succ-5],
                   [index-2, succ-9],
                   [index-3, succ-8],
                   [index-4, succ-2],
                   [index-5, succ-9],
                   [index-6, succ-2],
                   [index-7, succ-9],
                   [index-8, succ-8],
                   [index-9, succ-1]]],
                 ['CLIQUE'],
                 [1-5,2-9,3-8,4-2,5-9,6-2,7-9,8-8,9-1],
                 ['NCC'([[1,2,4,5,6,7,9],[3,8]]),'NTREE'([2,7,3])],
                 '','NCC=2,NTREE=3',
                 [2.145,2.4,2.4,2.4]).

ctr_see_also(map,
 [link('common keyword', cycle,          '%k', ['graph partitioning constraint']),
  link('common keyword', tree,           '%k', ['graph partitioning constraint']),
  link('common keyword', graph_crossing, '%k', ['graph partitioning constraint'])]).

ctr_key_words(map,['graph constraint'             ,
                   'graph partitioning constraint',
                   'connected component'          ,
		   'DFS-bottleneck'               ,
                   'functional dependency'        ,
		   'pure functional dependency'   ]).

ctr_persons(map,['Sedgewick R.',
                 'Flajolet O.' ]).

ctr_application(map, [3]).
