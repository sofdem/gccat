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

ctr_date(circuit_cluster,['20000128','20030820','20060805']).

ctr_origin(circuit_cluster, 'Inspired by \\cite{LaporteAsefVaziriSriskandarajah96}.', []).

ctr_arguments(circuit_cluster,
              ['NCIRCUIT'-dvar                                      ,
               'NODES'-collection(index-int, cluster-int, succ-dvar)]).

ctr_exchangeable(circuit_cluster,
                 [items('NODES',all)]).

ctr_restrictions(circuit_cluster,
                 ['NCIRCUIT'    >= 1                    ,
                  'NCIRCUIT'    =< size('NODES')        ,
                  required('NODES',[index,cluster,succ]),
                  'NODES'^index >= 1                    ,
                  'NODES'^index =< size('NODES')        ,
                  distinct('NODES',index)               ,
                  'NODES'^succ  >= 1                    ,
                  'NODES'^succ  =< size('NODES')        ]).

ctr_typical(circuit_cluster,
            ['NCIRCUIT'             < size('NODES'),
             size('NODES')          > 2            ,
             range('NODES'^cluster) > 1            ]).

ctr_graph(circuit_cluster,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ =\= nodes1^index,
           nodes1^succ  =  nodes2^index],
          ['NTREE'=  0        ,
           'NSCC' = 'NCIRCUIT'],
          ['ONE_SUCC'],
          ['ALL_VERTICES'>>[variables-col('VARIABLES'-collection(var-dvar),
                                          [item(var-'NODES'^cluster)])]],
          [alldifferent(variables),
           nvalues(variables,=,size('NODES',cluster))]).

ctr_example(circuit_cluster,
            [circuit_cluster(1,[[index-1, cluster-1, succ-1],
                                [index-2, cluster-1, succ-4],
                                [index-3, cluster-2, succ-3],
                                [index-4, cluster-2, succ-5],
                                [index-5, cluster-3, succ-8],
                                [index-6, cluster-3, succ-6],
                                [index-7, cluster-3, succ-7],
                                [index-8, cluster-4, succ-2],
                                [index-9, cluster-4, succ-9]]),
             circuit_cluster(2,[[index-1, cluster-1, succ-1],
                                [index-2, cluster-1, succ-4],
                                [index-3, cluster-2, succ-3],
                                [index-4, cluster-2, succ-2],
                                [index-5, cluster-3, succ-5],
                                [index-6, cluster-3, succ-9],
                                [index-7, cluster-3, succ-7],
                                [index-8, cluster-4, succ-8],
                                [index-9, cluster-4, succ-6]])]).

ctr_draw_example(circuit_cluster,
                 ['NODES'],
                 [[[index-1, cluster-1, succ-1],
                   [index-2, cluster-1, succ-4],
                   [index-3, cluster-2, succ-3],
                   [index-4, cluster-2, succ-2],
                   [index-5, cluster-3, succ-5],
                   [index-6, cluster-3, succ-9],
                   [index-7, cluster-3, succ-7],
                   [index-8, cluster-4, succ-8],
                   [index-9, cluster-4, succ-6]]],
                 ['CLIQUE'],
                 [2-4,4-2,6-9,9-6],
                 ['NSCC'([[2,4],[6,9]])],
                 '','NTREE=0,NSCC=2',
                 [2.145,2.145,2.145,1.3]).

ctr_see_also(circuit_cluster,
 [link('common keyword',            circuit,      '%k, %k', ['graph constraint', 'one\\_succ']),
  link('common keyword',            cycle,        '%k, %k', ['graph constraint', 'one\\_succ']),
  link('common keyword',            alldifferent, '%k',     [permutation]),
  link('used in graph description', alldifferent, '',       []),
  link('used in graph description', nvalues,      '',       [])]).

ctr_key_words(circuit_cluster,['graph constraint'            ,
                               'strongly connected component',
                               'cluster'                     ,
                               'one\\_succ'                  ,
                               'permutation'                 ]).

ctr_persons(circuit_cluster,['Laporte G.'       ,
                             'Asef-Vaziri A.'   ,
                             'Sriskandarajah C.']).

ctr_application(circuit_cluster, [2]).
