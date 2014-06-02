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

ctr_date(sliding_time_window_from_start,['20030820','20060815','20090530']).

ctr_origin(sliding_time_window_from_start, 'Used for defining %c.', [sliding_time_window]).

ctr_arguments(sliding_time_window_from_start,
              ['WINDOW_SIZE'-int                             ,
               'LIMIT'-int                                   ,
               'TASKS'-collection(origin-dvar, duration-dvar),
               'START'-dvar                                  ]).

ctr_exchangeable(sliding_time_window_from_start,
                 [vals(['WINDOW_SIZE'],int,>,dontcare,dontcare),
                  vals(['LIMIT'],int,<,dontcare,dontcare),
                  items('TASKS',all),
                  vals(['TASKS'^duration],int(>=(0)),>,dontcare,dontcare),
                  translate(['START','TASKS'^origin])]).

ctr_restrictions(sliding_time_window_from_start,
                 ['WINDOW_SIZE'    >  0              ,
                  'LIMIT'          >= 0              ,
                  required('TASKS',[origin,duration]),
                  'TASKS'^duration >= 0              ]).

ctr_typical(sliding_time_window_from_start,
            ['WINDOW_SIZE'    > 1            ,
             'LIMIT'          > 0            ,
             'LIMIT'          < 'WINDOW_SIZE',
             size('TASKS')    > 1            ,
             'TASKS'^duration > 0            ]).

ctr_contractible(sliding_time_window_from_start, [], 'TASKS', any).

ctr_derived_collections(sliding_time_window_from_start,
                        [col('S'-collection(var-dvar),
                             [item(var-'START')])]).

ctr_graph(sliding_time_window_from_start,
          ['S','TASKS'],
          2,
          ['PRODUCT'>>collection(s,tasks)],
          ['TRUE'],
          ['SUM_WEIGHT_ARC'(max(0,
                                min(s^var+'WINDOW_SIZE',tasks^origin+tasks^duration)
                                -
                                max(s^var,tasks^origin))) =< 'LIMIT'],
          []).

ctr_example(sliding_time_window_from_start,
            sliding_time_window_from_start(9,6,
                                           [[origin-10, duration-3],
                                            [origin-5 , duration-1],
                                            [origin-6 , duration-2]],
                                           5)).

ctr_draw_example(sliding_time_window_from_start,
                 ['S','TASKS'],
                 [[[var-5]],
                  [[origin-10, duration-3],
                   [origin-5 , duration-1],
                   [origin-6 , duration-2]]],
                 ['PRODUCT'],
                 [1-[1,2,3]],
                 ['SUM_WEIGHT_ARC'([3-[1-2],1-[1-3],2-[1-4]])],
                 '','SUM_WEIGHT_ARC=3+1+2=6',
                 [1.9,2.145,2.145,2.145]).

ctr_key_words(sliding_time_window_from_start,['sliding sequence constraint',
                                              'temporal constraint'        ,
                                              'derived collection'         ]).

ctr_application(sliding_time_window_from_start, [3]).

ctr_eval(sliding_time_window_from_start,
         [reformulation(sliding_time_window_from_start_r)]).

sliding_time_window_from_start_r(WINDOW_SIZE, LIMIT, TASKS, START) :-
    integer(WINDOW_SIZE),
    WINDOW_SIZE > 0,
    integer(LIMIT),
    LIMIT >= 0,
    collection(TASKS,[dvar,dvar_gteq(0)]),
	check_type(dvar, START),
    get_attr1(TASKS, ORIGINS),
    get_attr2(TASKS, DURATIONS),
    sliding_time_window1([START], [WINDOW_SIZE], 0,
                         ORIGINS, DURATIONS, WINDOW_SIZE, LIMIT).