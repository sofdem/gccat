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

ctr_date(cycle_or_accessibility,['20000128','20030820','20060807']).

ctr_origin(cycle_or_accessibility, 'Inspired by \\cite{LabbeLaporteRodriguezMartin98}.', []).

ctr_arguments(cycle_or_accessibility,
              ['MAXDIST'-int                                         ,
               'NCYCLE'-dvar                                         ,
               'NODES'-collection(index-int, succ-dvar, x-int, y-int)]).

ctr_exchangeable(cycle_or_accessibility,
                 [items('NODES',all),
                  attrs_sync('NODES',[[index],[succ],[x,y]]),
                  translate(['NODES'^x]),
                  translate(['NODES'^y])]).

ctr_restrictions(cycle_or_accessibility,
                 ['MAXDIST'      >= 0                ,
                  'NCYCLE'       >= 1                ,
                  'NCYCLE'       =< size('NODES')    ,
                  required('NODES',[index,succ,x,y]),
                  'NODES'^index  >= 1                ,
                  'NODES'^index  =< size('NODES')    ,
                  distinct('NODES',index)            ,
                  'NODES'^succ   >= 0                ,
                  'NODES'^succ   =< size('NODES')    ,
                  'NODES'^x      >= 0                ,
                  'NODES'^y      >= 0                ]).

ctr_typical(cycle_or_accessibility,
            ['MAXDIST'     > 0            ,
             'NCYCLE'      < size('NODES'),
             size('NODES') > 2            ]).

ctr_functional_dependency(cycle_or_accessibility, 2, [3]).

ctr_graph(cycle_or_accessibility,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['NTREE' = 0       ,
           'NCC'   = 'NCYCLE'],
          []).

ctr_graph(cycle_or_accessibility,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [(nodes1^succ = nodes2^index) #\/
           (nodes1^succ  =  0  #/\
            nodes2^succ =\= 0  #/\
            abs(nodes1^x-nodes2^x) + abs(nodes1^y-nodes2^y) =< 'MAXDIST')],
          ['NVERTEX' = size('NODES')],
          [],
          ['PRED'>>[variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'NODES'^succ)]),
                    destination                                    ]],
          [nvalues_except_0(variables,=,1)]).

ctr_example(cycle_or_accessibility,
            cycle_or_accessibility(3,2,
                                   [[index-1, succ-6, x-4, y-5],
                                    [index-2, succ-0, x-9, y-1],
                                    [index-3, succ-0, x-2, y-4],
                                    [index-4, succ-1, x-2, y-6],
                                    [index-5, succ-5, x-7, y-2],
                                    [index-6, succ-4, x-4, y-7],
                                    [index-7, succ-0, x-6, y-4]])).

ctr_draw_example(cycle_or_accessibility,
                 ['NODES'],
                 [[[index-1, succ-6, x-4, y-5],
                   [index-2, succ-0, x-9, y-1],
                   [index-3, succ-0, x-2, y-4],
                   [index-4, succ-1, x-2, y-6],
                   [index-5, succ-5, x-7, y-2],
                   [index-6, succ-4, x-4, y-7],
                   [index-7, succ-0, x-6, y-4]]],
                 ['CLIQUE'],
                 [1-6,2-5,3-[1,4],4-1,5-5,6-4,7-[1,5]],
                 ['NVERTEX'],
                 '','NVERTEX=7',
                 [2.2,2.2,2.2,2.2]).

ctr_see_also(cycle_or_accessibility,
 [link('common keyword',            cycle,            '%k', ['graph constraint']),
  link('used in graph description', nvalues_except_0, '',   [])]).

ctr_key_words(cycle_or_accessibility,['graph constraint'            ,
                                      'geometrical constraint'      ,
                                      'strongly connected component',
                                      'facilities location problem' ,
                                      'functional dependency'       ]).

ctr_persons(cycle_or_accessibility,['Labb\\\'e M.'                      ,
                                    'Laporte G.'                       ,
                                    'Rodr{\\\'\\i}guez-Mart{\\\'\\i}n I.']).

ctr_application(cycle_or_accessibility, [3]).
