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

ctr_date(cycle_card_on_path,['20000128','20030820','20040530','20060807']).

ctr_origin(cycle_card_on_path, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(cycle_card_on_path,
              ['NCYCLE'-dvar                                        ,
               'NODES'-collection(index-int, succ-dvar, colour-dvar),
               'ATLEAST'-int                                        ,
               'ATMOST'-int                                         ,
               'PATH_LEN'-int                                       ,
               'VALUES'-collection(val-int)                         ]).

ctr_exchangeable(cycle_card_on_path,
                 [items('NODES',all),
                  vals(['NODES'^colour],comp('VALUES'^val),=,dontcare,dontcare),                  
                  vals(['ATLEAST'],int(>=(0)),>,dontcare,dontcare),
                  vals(['ATMOST'],int,<,dontcare,dontcare),
                  items('VALUES',all)]).

ctr_restrictions(cycle_card_on_path,
                 ['NCYCLE'       >= 1                   ,
                  'NCYCLE'       =< size('NODES')       ,
                  required('NODES',[index,succ,colour]) ,
                  'NODES'^index  >= 1                   ,
                  'NODES'^index  =< size('NODES')       ,
                  distinct('NODES',index)               ,
                  'NODES'^succ   >= 1                   ,
                  'NODES'^succ   =< size('NODES')       ,
                  'ATLEAST'      >= 0                   ,
                  'ATLEAST'      =< 'PATH_LEN'          ,
                  'ATMOST'       >= 'ATLEAST'           ,
                  'PATH_LEN'     >= 0                   ,
                  size('VALUES') >= 1                   ,
                  required('VALUES',val)                ,
                  distinct('VALUES',val)                ]).

ctr_typical(cycle_card_on_path,
            [size('NODES') > 2                      ,
             'NCYCLE'      < size('NODES')          ,
             'ATLEAST'     < 'PATH_LEN'             ,
             'ATMOST'      > 0                      ,
             'PATH_LEN'    > 1                      ,
             size('NODES') > size('VALUES')         ,
             'ATLEAST' > 0 #\/ 'ATMOST' < 'PATH_LEN']).

ctr_graph(cycle_card_on_path,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['NTREE' =  0      ,
           'NCC'   = 'NCYCLE'],
          ['ONE_SUCC'],
          ['PATH_LENGTH'('PATH_LEN')>>[variables-col('VARIABLES'-collection(var-dvar),
                                                     [item(var-'NODES'^colour)])]],
          [among_low_up('ATLEAST','ATMOST',variables,'VALUES')]).

ctr_example(cycle_card_on_path,
            cycle_card_on_path(2,
                               [[index-1, succ-7, colour-2],
                                [index-2, succ-4, colour-3],
                                [index-3, succ-8, colour-2],
                                [index-4, succ-9, colour-1],
                                [index-5, succ-1, colour-2],
                                [index-6, succ-2, colour-1],
                                [index-7, succ-5, colour-1],
                                [index-8, succ-6, colour-1],
                                [index-9, succ-3, colour-1]],
                               1,2,3,[[val-1]])).

ctr_draw_example(cycle_card_on_path,
                 ['NODES'],
                 [[[index-1, succ-7, colour-2],
                   [index-2, succ-4, colour-3],
                   [index-3, succ-8, colour-2],
                   [index-4, succ-9, colour-1],
                   [index-5, succ-1, colour-2],
                   [index-6, succ-2, colour-1],
                   [index-7, succ-5, colour-1],
                   [index-8, succ-6, colour-1],
                   [index-9, succ-3, colour-1]]],
                 ['CLIQUE'],
                 [1-7,2-4,3-8,4-9,5-1,6-2,7-5,8-6,9-3],
                 ['NCC'([[1,5,7],[2,3,4,6,8,9]])],
                 '','NTREE=0,NCC=2',
                 [1.9,2.8,2.8,2.8]).

ctr_see_also(cycle_card_on_path,
 [link('common keyword',            cycle,        '%k', ['graph partitioning constraint']),
  link('used in graph description', among_low_up, '',   [])]).

ctr_key_words(cycle_card_on_path,['graph constraint'             ,
                                  'graph partitioning constraint',
                                  'sliding sequence constraint'  ,
                                  'sequence'                     ,
                                  'connected component'          ,
                                  'coloured'                     ,
                                  'one\\_succ'                   ]).

ctr_persons(cycle_card_on_path,['Bourreau \\\'E.']).

ctr_application(cycle_card_on_path, [2]).
