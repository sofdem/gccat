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

ctr_date(disjoint_tasks,['20030820','20060808']).

ctr_origin(disjoint_tasks, 'Derived from %c.', [disjoint]).

ctr_arguments(disjoint_tasks,
              ['TASKS1'-collection(origin-dvar, duration-dvar, end-dvar),
               'TASKS2'-collection(origin-dvar, duration-dvar, end-dvar)]).

ctr_exchangeable(disjoint_tasks,
                 [args([['TASKS1','TASKS2']]),
                  items('TASKS1',all),
                  items('TASKS2',all),
                  translate(['TASKS1'^origin,'TASKS1'^end,'TASKS2'^origin,'TASKS2'^end])]).

ctr_restrictions(disjoint_tasks,
                 [require_at_least(2,'TASKS1',[origin,duration,end]),
                  'TASKS1'^duration >= 0                            ,
                  'TASKS1'^origin   =< 'TASKS1'^end                 ,
                  require_at_least(2,'TASKS2',[origin,duration,end]),
                  'TASKS2'^duration >= 0                            ,
                  'TASKS2'^origin   =< 'TASKS2'^end                 ]).

ctr_typical(disjoint_tasks,
            [size('TASKS1')    > 1,
             'TASKS1'^duration > 0,
             size('TASKS2')    > 1,
             'TASKS2'^duration > 0]).

ctr_contractible(disjoint_tasks, [], 'TASKS1', any).
ctr_contractible(disjoint_tasks, [], 'TASKS2', any).

ctr_graph(disjoint_tasks,
          ['TASKS1'],
          1,
          ['SELF'>>collection(tasks1)],
          [tasks1^origin+tasks1^duration = tasks1^end],
          ['NARC' = size('TASKS1')],
          []).

ctr_graph(disjoint_tasks,
          ['TASKS2'],
          1,
          ['SELF'>>collection(tasks2)],
          [tasks2^origin+tasks2^duration = tasks2^end],
          ['NARC' = size('TASKS2')],
          []).

ctr_graph(disjoint_tasks,
          ['TASKS1','TASKS2'],
          2,
          ['PRODUCT'>>collection(tasks1,tasks2)],
          [tasks1^duration > 0         ,
           tasks2^duration > 0         ,
           tasks1^origin   < tasks2^end,
           tasks2^origin   < tasks1^end],
          ['NARC' = 0],
          []).

ctr_example(disjoint_tasks,
            disjoint_tasks([[origin-6 , duration-5, end-11],
                            [origin-8 , duration-2, end-10]],
                           [[origin-2 , duration-2, end-4 ],
                            [origin-3 , duration-3, end-6 ],
                            [origin-12, duration-1, end-13]])).

ctr_draw_example(disjoint_tasks,
                  ['TASKS1','TASKS2'],
                  [[[origin-6 , duration-5, end-11],
                    [origin-8 , duration-2, end-10]],
                   [[origin-2 , duration-2, end-4 ],
                    [origin-3 , duration-3, end-6 ],
                    [origin-12, duration-1, end-13]]],
                  ['PRODUCT'],
                  [],
                 ['NARC'],
                 '','NARC=0',
                 [1.6,1.6,1.6,1.6]).

ctr_see_also(disjoint_tasks,
 [link(generalisation, coloured_cumulative, 'tasks colours and limit on maximum number of colours in parallel are explicitly given', []),
  link(specialisation, disjoint,            '%e replaced by %e',                                                                     [task,variable])]).

ctr_key_words(disjoint_tasks,['scheduling constraint',
                              'temporal constraint'  ,
                              'non-overlapping'      ]).

ctr_application(disjoint_tasks, [1,2]).

ctr_eval(disjoint_tasks, [reformulation(disjoint_tasks_r)]).

disjoint_tasks_r(TASKS1, TASKS2) :-
    collection(TASKS1, [dvar,dvar_gteq(0),dvar]),
    collection(TASKS2, [dvar,dvar_gteq(0),dvar]),
    get_attr1(TASKS1, ORIGINS1  ),
    get_attr2(TASKS1, DURATIONS1),
    get_attr3(TASKS1, ENDS1     ),
    ori_dur_end(ORIGINS1, DURATIONS1, ENDS1),
    get_attr1(TASKS2, ORIGINS2  ),
    get_attr2(TASKS2, DURATIONS2),
    get_attr3(TASKS2, ENDS2     ),
    ori_dur_end(ORIGINS2, DURATIONS2, ENDS2),
    disjoint_tasks1(ORIGINS1, ENDS1, ORIGINS2, ENDS2).

disjoint_tasks1([], [], _, _).
disjoint_tasks1([O|R], [E|S], ORIGINS2, ENDS2) :-
    disjoint_tasks2(ORIGINS2, ENDS2, O, E),
    disjoint_tasks1(R, S, ORIGINS2, ENDS2).

disjoint_tasks2([], [], _, _).
disjoint_tasks2([Oj|R], [Ej|S], Oi, Ei) :-
    Ei #=< Oj #\/ Ej #=< Oi,
    disjoint_tasks2(R, S, Oi, Ei).
