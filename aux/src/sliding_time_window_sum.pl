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

ctr_date(sliding_time_window_sum,['20030820','20060815','20090530']).

ctr_origin(sliding_time_window_sum, 'Derived from %c.', [sliding_time_window]).

ctr_arguments(sliding_time_window_sum,
              ['WINDOW_SIZE'-int                                     ,
               'LIMIT'-int                                           ,
               'TASKS'-collection(origin-dvar, end-dvar, npoint-dvar)]).

ctr_exchangeable(sliding_time_window_sum,
                 [vals(['WINDOW_SIZE'],int,>,dontcare,dontcare),
                  vals(['LIMIT'],int,<,dontcare,dontcare),
                  items('TASKS',all),
                  vals(['TASKS'^npoint],int(>=(0)),>,dontcare,dontcare),
                  translate(['TASKS'^origin,'TASKS'^end])]).

ctr_restrictions(sliding_time_window_sum,
                 ['WINDOW_SIZE'    >  0                ,
                  'LIMIT'          >= 0                ,
                  required('TASKS',[origin,end,npoint]),
                  'TASKS'^origin   =< 'TASKS'^end      ,
                  'TASKS'^npoint   >= 0                ]).

ctr_typical(sliding_time_window_sum,
            ['WINDOW_SIZE'  > 1                  ,
             'LIMIT'        > 0                  ,
             'LIMIT'        < sum('TASKS'^npoint),
             size('TASKS')  > 1                  ,
             'TASKS'^origin < 'TASKS'^end        ,
             'TASKS'^npoint > 0                  ]).

ctr_contractible(sliding_time_window_sum, [], 'TASKS', any).

ctr_graph(sliding_time_window_sum,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin =< tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(sliding_time_window_sum,
          ['TASKS'],
          2,
          ['CLIQUE'>>collection(tasks1,tasks2)],
          [tasks1^end    =< tasks2^end                 ,
           tasks2^origin - tasks1^end < 'WINDOW_SIZE' - 1],
          [],
          [],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^npoint)])]],
          [sum_ctr(variables,=<,'LIMIT')]).

ctr_example(sliding_time_window_sum,
            sliding_time_window_sum(9,16,
                                    [[origin-10, end-13, npoint-2],
                                     [origin-5 , end-6 , npoint-3],
                                     [origin-6 , end-8 , npoint-4],
                                     [origin-14, end-16, npoint-5],
                                     [origin-2 , end-4 , npoint-6]])).

ctr_draw_example(sliding_time_window_sum,
                 ['TASKS'],
                 [[[origin-10, end-13, npoint-2],
                   [origin-5 , end-6 , npoint-3],
                   [origin-6 , end-8 , npoint-4],
                   [origin-14, end-16, npoint-5],
                   [origin-2 , end-4 , npoint-6]]],
                 ['CLIQUE'],
                 [1-[1,4],
                  2-[1,2,3],
                  3-[1,3,4],
                  4-4,
                  5-[1,2,3,5]],
                 [],
                 '','',
                 [2.145,2.3,2.4,2.3]).

ctr_see_also(sliding_time_window_sum,
 [link('related', sliding_time_window,
       'sum of the points of intersecting tasks with sliding time window replaced by sum of intersections of tasks with sliding time window', []),
  link('used in graph description', sum_ctr, '', [])]).

ctr_key_words(sliding_time_window_sum,['sliding sequence constraint',
                                       'temporal constraint'        ,
                                       'time window'                ,
                                       'sum'                        ]).

ctr_application(sliding_time_window_sum, [3]).

ctr_eval(sliding_time_window_sum, [reformulation(sliding_time_window_sum_r)]).

sliding_time_window_sum_r(WINDOW_SIZE, LIMIT, TASKS) :-
    integer(WINDOW_SIZE),
    WINDOW_SIZE > 0,
    integer(LIMIT),
    LIMIT >= 0,
    collection(TASKS,[dvar,dvar,dvar_gteq(0)]),
    get_attr1(TASKS, ORIGINS),
    get_attr2(TASKS, ENDS),
    get_attr3(TASKS, NPOINTS),
    sliding_time_window_sum1(ORIGINS, ENDS, NPOINTS, 1, ORIGINS, ENDS, NPOINTS, WINDOW_SIZE, LIMIT).

sliding_time_window_sum1([], [], [], _, _, _, _, _, _).
sliding_time_window_sum1([Oi|RO], [Ei|RE], [Pi|RP], I, ORIGINS, ENDS, NPOINTS, WINDOW_SIZE, LIMIT) :-
    Oi #=< Ei,
    sliding_time_window_sum2(ORIGINS, ENDS, NPOINTS, 1, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, SUM_NPOINTS),
    call(SUM_NPOINTS #=< LIMIT),
    I1 is I+1,
    sliding_time_window_sum1(RO, RE, RP, I1, ORIGINS, ENDS, NPOINTS, WINDOW_SIZE, LIMIT).

sliding_time_window_sum2([], [], [], _, _, _, _, _, _, _, 0) :-
    !.
sliding_time_window_sum2([_|RO], [_|RE], [_|RP], J, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, Pi+SUM) :-
    I = J,
    !,
    J1 is J+1,
    sliding_time_window_sum2(RO, RE, RP, J1, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, SUM).
sliding_time_window_sum2([_|RO], [Ej|RE], [_|RP], J, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, SUM) :-
    I =\= J,
    fd_max(Ej, MaxEj),
    fd_min(Oi, MinOi),
    MaxEj < MinOi,
    !,
    J1 is J+1,
    sliding_time_window_sum2(RO, RE, RP, J1, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, SUM).
sliding_time_window_sum2([Oj|RO], [_|RE], [_|RP], J, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, SUM) :-
    I =\= J,
    fd_min(Oj,MinOj),
    fd_max(Oi,MaxOi),
    E is MaxOi+WINDOW_SIZE-1,
    MinOj > E,
    !,
    J1 is J+1,
    sliding_time_window_sum2(RO, RE, RP, J1, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, SUM).
sliding_time_window_sum2([Oj|RO], [Ej|RE], [Pj|RP], J, Oi, Ei, Pi, I, WINDOW_SIZE,
                         LIMIT, min(1,max(0,min(Oi+WINDOW_SIZE,Ej)-max(Oi,Oj)))*Pj+SUM) :-
    J1 is J+1,
    sliding_time_window_sum2(RO, RE, RP, J1, Oi, Ei, Pi, I, WINDOW_SIZE, LIMIT, SUM).
