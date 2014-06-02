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

ctr_date(cumulative_with_level_of_priority,['20040530','20060807']).

ctr_origin(cumulative_with_level_of_priority, 'H.~Simonis', []).

ctr_arguments(cumulative_with_level_of_priority,
              ['TASKS'-collection(priority-int,
                                  origin-dvar , duration-dvar, end-dvar,
                                  height-dvar                          ),
               'PRIORITIES'-collection(id-int, capacity-int)            ]).

ctr_exchangeable(cumulative_with_level_of_priority,
                 [items('TASKS',all),
                  vals(['TASKS'^priority],int(=<(size('PRIORITIES'))),<,dontcare,dontcare),
                  vals(['TASKS'^height],int(>=(0)),>,dontcare,dontcare),
                  translate(['TASKS'^origin,'TASKS'^end]),
                  vals(['PRIORITIES'^capacity],int,<,dontcare,dontcare)]).

ctr_restrictions(cumulative_with_level_of_priority,
                 [required('TASKS',[priority,height])              ,
                  require_at_least(2,'TASKS',[origin,duration,end]),
                  'TASKS'^priority >= 1                            ,
                  'TASKS'^priority =< size('PRIORITIES')           ,
                  'TASKS'^duration >= 0                            ,
                  'TASKS'^origin   =< 'TASKS'^end                  ,
                  'TASKS'^height   >= 0                            ,
                  required('PRIORITIES',[id,capacity])             ,
                  'PRIORITIES'^id >= 1                             ,
                  'PRIORITIES'^id =< size('PRIORITIES')            ,
                  increasing_seq('PRIORITIES',id)                  ,
                  increasing_seq('PRIORITIES',capacity)            ]).

ctr_typical(cumulative_with_level_of_priority,
            [size('TASKS')           > 1                  ,
             range('TASKS'^priority) > 1                  ,
             range('TASKS'^origin)   > 1                  ,
             range('TASKS'^duration) > 1                  ,
             range('TASKS'^end)      > 1                  ,
             range('TASKS'^height)   > 1                  ,
             'TASKS'^duration        > 0                  ,
             'TASKS'^height          > 0                  ,
             size('PRIORITIES')      > 1                  ,
             'PRIORITIES'^capacity   > 0                  ,
             'PRIORITIES'^capacity   < sum('TASKS'^height),
             size('TASKS')           > size('PRIORITIES') ]).

ctr_contractible(cumulative_with_level_of_priority, [], 'TASKS', any).

ctr_derived_collections(cumulative_with_level_of_priority,
    [col('TIME_POINTS'-collection(idp-int, duration-dvar, point-dvar),
         [item(idp-'TASKS'^priority, duration-'TASKS'^duration, point-'TASKS'^origin),
          item(idp-'TASKS'^priority, duration-'TASKS'^duration, point-'TASKS'^end   )])]).

ctr_graph(cumulative_with_level_of_priority,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin+tasks^duration = tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(cumulative_with_level_of_priority,
          ['TIME_POINTS','TASKS'],
          2,
          foreach('PRIORITIES',['PRODUCT'>>collection(time_points,tasks)]),
          [time_points^idp      = 'PRIORITIES'^id  ,
           time_points^idp     >=  tasks^priority  ,
           time_points^duration > 0                ,
           tasks^origin        =< time_points^point,
           time_points^point    < tasks^end        ],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^height)])]],
          [sum_ctr(variables,=<,'PRIORITIES'^capacity)]).

ctr_example(cumulative_with_level_of_priority,
            cumulative_with_level_of_priority(
              [[priority-1, origin-1, duration-2, end-3, height-1],
               [priority-1, origin-2, duration-3, end-5, height-1],
               [priority-1, origin-5, duration-2, end-7, height-2],
               [priority-2, origin-3, duration-2, end-5, height-2],
               [priority-2, origin-6, duration-3, end-9, height-1]],
              [[id-1, capacity-2],
               [id-2, capacity-3]])).

ctr_draw_example(cumulative_with_level_of_priority,
                 ['TIME_POINTS','TASKS'],
                 [[[idp-1, duration-2, point-1],
                   [idp-1, duration-2, point-3],
                   [idp-1, duration-3, point-2],
                   [idp-1, duration-3, point-5],
                   [idp-1, duration-2, point-5],
                   [idp-2, duration-2, point-3],
                   [idp-2, duration-2, point-5],
                   [idp-2, duration-3, point-6]],
                  [[priority-1, origin-1, duration-2, end-3, height-1],
                   [priority-1, origin-2, duration-3, end-5, height-1],
                   [priority-1, origin-5, duration-2, end-7, height-2],
                   [priority-1, origin-2, duration-3, end-5, height-1],
                   [priority-1, origin-5, duration-2, end-7, height-2],
                   [priority-2, origin-3, duration-2, end-5, height-2],
                   [priority-2, origin-6, duration-3, end-9, height-1]]],
                 ['PRODUCT'],
                 [ 1-1,
                   2-2,
                   3-[1,2],
                   4-3,
                   5-3,
                   6-[4,6],
                   7-5,
                   8-[5,7]],
                 ['FOREACH'('PRIORITIES',[1-[1,2,3, 4, 5, 9,10,11],
                                          2-[6,7,8,12,13,14,15   ]])],
                 '','',
                 [3,2.25,5,2.145]).

ctr_see_also(cumulative_with_level_of_priority,
 [link('common keyword',             cumulative, '%k', ['resource constraint']),
  link('used in graph description',  sum_ctr,    '',   [])]).

ctr_key_words(cumulative_with_level_of_priority,['scheduling constraint',
                                                 'resource constraint'  ,
                                                 'temporal constraint'  ,
                                                 'derived collection'   ,
                                                 'zero-duration task'   ]).

ctr_persons(cumulative_with_level_of_priority,['Simonis H.']).

ctr_application(cumulative_with_level_of_priority, [1]).
