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

ctr_date(cycle_resource,['20030820','20040530','20060807']).

ctr_origin(cycle_resource, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(cycle_resource,
              ['RESOURCE'-collection(id-int, first_task-dvar, nb_task-dvar ),
                   'TASK'-collection(id-int, next_task-dvar , resource-dvar)]).

ctr_exchangeable(cycle_resource,
                 [items('RESOURCE',all),
                  items('TASK',all),
                  vals(['RESOURCE'^id,'TASK'^resource],int,=\=,all,in)]).

ctr_restrictions(cycle_resource,
                 [required('RESOURCE',[id,first_task,nb_task])          ,
                  'RESOURCE'^id >= 1                                    ,
                  'RESOURCE'^id =< size('RESOURCE')                     ,
                  distinct('RESOURCE',id)                               ,
                  'RESOURCE'^first_task >= 1                            ,
                  'RESOURCE'^first_task =< size('RESOURCE')+size('TASK'),
                  'RESOURCE'^nb_task >= 0                               ,
                  'RESOURCE'^nb_task =< size('TASK')                    ,
                  required('TASK',[id,next_task,resource])              ,
                  'TASK'^id > size('RESOURCE')                          ,
                  'TASK'^id =< size('RESOURCE')+size('TASK')            ,
                  distinct('TASK',id)                                   ,
                  'TASK'^next_task >= 1                                 ,
                  'TASK'^next_task =< size('RESOURCE')+size('TASK')     ,
                  'TASK'^resource >= 1                                  ,
                  'TASK'^resource =< size('RESOURCE')                   ]).

ctr_typical(cycle_resource,
            [size('RESOURCE') > 1               ,
             size('TASK')     > 1               ,
             size('TASK')     > size('RESOURCE')]).

ctr_derived_collections(cycle_resource,
    [col('RESOURCE_TASK'-collection(index-int, succ-dvar, name-dvar),
         [item(index-'RESOURCE'^id, succ-'RESOURCE'^first_task, name-'RESOURCE'^id  ),
          item(index-'TASK'^id    , succ-'TASK'^next_task     , name-'TASK'^resource)])
    ]).

ctr_graph(cycle_resource,
          ['RESOURCE_TASK'],
          2,
          ['CLIQUE'>>collection(resource_task1,resource_task2)],
          [resource_task1^succ = resource_task2^index,
           resource_task1^name = resource_task2^name ],
          ['NTREE'   = 0                            ,
           'NCC'     = size('RESOURCE')             ,
           'NVERTEX' = size('RESOURCE')+size('TASK')],
          ['ONE_SUCC']).

ctr_graph(cycle_resource,
          ['RESOURCE_TASK'],
          2,
          foreach('RESOURCE',['CLIQUE'>>collection(resource_task1,resource_task2)]),
          [resource_task1^succ = resource_task2^index,
           resource_task1^name = resource_task2^name ,
           resource_task1^name = 'RESOURCE'^id       ],
          ['NVERTEX' = 'RESOURCE'^nb_task+1],
          []).

ctr_example(cycle_resource,
            cycle_resource([[id-1, first_task-5,  nb_task-3],
                            [id-2, first_task-2,  nb_task-0],
                            [id-3, first_task-8,  nb_task-2]],
                           [[id-4,  next_task-7, resource-1],
                            [id-5,  next_task-4, resource-1],
                            [id-6,  next_task-3, resource-3],
                            [id-7,  next_task-1, resource-1],
                            [id-8,  next_task-6, resource-3]])).

ctr_draw_example(cycle_resource,
                 ['RESOURCE_TASK'],
                 [[[index-1, succ-5, name-1],
                   [index-2, succ-2, name-2],
                   [index-3, succ-8, name-3],
                   [index-4, succ-7, name-1],
                   [index-5, succ-4, name-1],
                   [index-6, succ-3, name-3],
                   [index-7, succ-1, name-1],
                   [index-8, succ-6, name-3]]],
                 ['CLIQUE'],
                 [1-5,2-2,3-8,4-7,5-4,6-3,7-1,8-6],
                 ['NVERTEX',
                  'FOREACH'('RESOURCE',[1-[1,4,5,7],2-[2],3-[3,6,8]])],
                 '','1:NVERTEX=4\\n2:NVERTEX=1\\n3:NVERTEX=3',
                 [2.3,2.41,2.5,2.3]).

ctr_see_also(cycle_resource,
 [link('common keyword', cycle, '%k', ['graph partitioning constraint'])]).

ctr_key_words(cycle_resource,['graph constraint'             ,
                              'resource constraint'          ,
                              'graph partitioning constraint',
                              'connected component'          ,
                              'strongly connected component' ,
                              'derived collection'           ]).

ctr_persons(cycle_resource,['Bourreau \\\'E.']).

ctr_application(cycle_resource, [2]).
