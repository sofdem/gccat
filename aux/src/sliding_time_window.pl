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

ctr_date(sliding_time_window,['20030820','20060815','20090530']).

ctr_origin(sliding_time_window, 'N.~Beldiceanu', []).

ctr_arguments(sliding_time_window,
              ['WINDOW_SIZE'-int                             ,
               'LIMIT'-int                                   ,
               'TASKS'-collection(origin-dvar, duration-dvar)]).

ctr_exchangeable(sliding_time_window,
                 [vals(['WINDOW_SIZE'],int,>,dontcare,dontcare),
                  vals(['LIMIT'],int,<,dontcare,dontcare),
                  items('TASKS',all),
                  translate(['TASKS'^origin]),
                  vals(['TASKS'^duration],int(>=(0)),>,dontcare,dontcare)]).

ctr_restrictions(sliding_time_window,
                 ['WINDOW_SIZE'    >  0              ,
                  'LIMIT'          >= 0              ,
                  required('TASKS',[origin,duration]),
                  'TASKS'^duration >= 0              ]).

ctr_typical(sliding_time_window,
            ['WINDOW_SIZE'    > 1                    ,
             'LIMIT'          > 0                    ,
             'LIMIT'          < sum('TASKS'^duration),
             size('TASKS')    > 1                    ,
             'TASKS'^duration > 0                    ]).

ctr_contractible(sliding_time_window, [], 'TASKS', any).

ctr_graph(sliding_time_window,
          ['TASKS'],
          2,
          ['CLIQUE'>>collection(tasks1,tasks2)],
          [tasks1^origin =< tasks2^origin,
           tasks2^origin - tasks1^origin < 'WINDOW_SIZE'],
          [],
          [],
          ['SUCC'>>[source,tasks]],
          [sliding_time_window_from_start('WINDOW_SIZE','LIMIT',tasks,source^origin)]).

ctr_example(sliding_time_window,
            sliding_time_window(9,6,
                                [[origin-10, duration-3],
                                 [origin-5 , duration-1],
                                 [origin-6 , duration-2],
                                 [origin-14, duration-2],
                                 [origin-2 , duration-2]])).

ctr_draw_example(sliding_time_window,
                 ['TASKS'],
                 [[[origin-10, duration-3],
                   [origin-5 , duration-1],
                   [origin-6 , duration-2],
                   [origin-14, duration-2],
                   [origin-2 , duration-2]]],
                 ['CLIQUE'],
                 [1-[1,4],
                  2-[1,2,3],
                  3-[1,3,4],
                  4-4,
                  5-[1,2,3,5]],
                 [],
                 '','',
                 [2.145,2.3,2.4,2.3]).

ctr_see_also(sliding_time_window,
 [link('common keyword', shift, '%k', ['temporal constraint']),
  link('related', sliding_time_window_sum,
       'sum of intersections of tasks with sliding time window replaced by sum of the points of intersecting tasks with sliding time window', []),
  link('used in graph description', sliding_time_window_from_start, '', [])]).

ctr_key_words(sliding_time_window,['sliding sequence constraint',
                                   'temporal constraint'        ]).

ctr_persons(sliding_time_window,['Beldiceanu N.']).

ctr_application(sliding_time_window, [3]).

ctr_eval(sliding_time_window, [reformulation(sliding_time_window_r)]).

sliding_time_window_r(WINDOW_SIZE, LIMIT, TASKS) :-
    integer(WINDOW_SIZE),
    WINDOW_SIZE > 0,
    integer(LIMIT),
    LIMIT >= 0,
    collection(TASKS,[dvar,dvar_gteq(0)]),
    get_attr1(TASKS, ORIGINS),
    get_attr2(TASKS, DURATIONS),
    sliding_time_window1(ORIGINS, DURATIONS, 1, ORIGINS, DURATIONS, WINDOW_SIZE, LIMIT).
