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

ctr_date(cumulatives,['20000128','20030820','20040530','20060807']).

ctr_origin(cumulatives, '\\cite{BeldiceanuCarlsson02a}', []).

ctr_arguments(cumulatives,
              ['TASKS'-collection(machine-dvar,
                                  origin-dvar , duration-dvar, end-dvar,
                                  height-dvar                          ),
               'MACHINES'-collection(id-int, capacity-int)              ,
               'CTR'-atom                                               ]).

ctr_exchangeable(cumulatives,
                 [items('TASKS',all),
                  items('MACHINES',all),
                  vals(['TASKS'^machine,'MACHINES'^id],int,=\=,all,dontcare)]).

ctr_restrictions(cumulatives,
                 [required('TASKS',[machine,height])               ,
                  require_at_least(2,'TASKS',[origin,duration,end]),
                  in_attr('TASKS',machine,'MACHINES',id)           ,
                  'TASKS'^duration >= 0                            ,
                  'TASKS'^origin   =< 'TASKS'^end                  ,
                  size('MACHINES') >  0                            ,
                  required('MACHINES',[id,capacity])               ,
                  distinct('MACHINES',id)                          ,
                  in_list('CTR',[=<,>=])                           ]).

ctr_typical(cumulatives,
            [size('TASKS')           >   1                  ,
             range('TASKS'^machine)  >   1                  ,
             range('TASKS'^origin)   >   1                  ,
             range('TASKS'^duration) >   1                  ,
             range('TASKS'^end)      >   1                  ,
             range('TASKS'^height)   >   1                  ,
             'TASKS'^duration        >   0                  ,
             'TASKS'^height          =\= 0                  ,
             size('MACHINES')        >   1                  ,
             'MACHINES'^capacity     <   sum('TASKS'^height),
             size('TASKS')           >   size('MACHINES')   ]).

ctr_contractible(cumulatives, [in_list('RELOP',[=<]),minval('TASKS'^height)>=0], 'TASKS', any).

ctr_derived_collections(cumulatives,
    [col('TIME_POINTS'-collection(idm-int, duration-dvar, point-dvar),
         [item(idm-'TASKS'^machine, duration-'TASKS'^duration, point-'TASKS'^origin),
          item(idm-'TASKS'^machine, duration-'TASKS'^duration, point-'TASKS'^end   )])]).

ctr_graph(cumulatives,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin+tasks^duration = tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(cumulatives,
          ['TIME_POINTS','TASKS'],
          2,
          foreach('MACHINES',['PRODUCT'>>collection(time_points,tasks)]),
          [time_points^idm     =  'MACHINES'^id    ,
           time_points^idm     =  tasks^machine    ,
           time_points^duration > 0                ,
           tasks^origin        =< time_points^point,
           time_points^point    < tasks^end        ],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^height)])]],
          [sum_ctr(variables,'CTR','MACHINES'^capacity)]).

ctr_example(cumulatives,
            cumulatives(
              [[machine-1, origin-2 , duration-2, end-4 , height-(-2)],
               [machine-1, origin-1 , duration-4, end-5 , height-1   ],
               [machine-1, origin-4 , duration-2, end-6 , height-(-1)],
               [machine-1, origin-2 , duration-3, end-5 , height-2   ],
               [machine-1, origin-5 , duration-2, end-7 , height-2   ],
               [machine-2, origin-3 , duration-2, end-5 , height-(-1)],
               [machine-2, origin-1 , duration-4, end-5 , height-1   ]],
              [[id-1, capacity-0],
               [id-2, capacity-0]],
              >=)).

ctr_draw_example(cumulatives,
                 ['TIME_POINTS','TASKS'],
                 [[[idm-1, duration-2, point-2],
                   [idm-1, duration-2, point-4],
                   [idm-1, duration-4, point-1],
                   [idm-1, duration-4, point-5],
                   [idm-1, duration-2, point-4],
                   [idm-1, duration-2, point-6],
                   [idm-1, duration-3, point-2],
                   [idm-1, duration-3, point-5],
                   [idm-1, duration-2, point-5],
                   [idm-1, duration-2, point-7],
                   [idm-2, duration-2, point-3],
                   [idm-2, duration-2, point-5],
                   [idm-2, duration-4, point-1],
                   [idm-2, duration-4, point-5]],
                  [[machine-1, origin-2 , duration-2, end-4 , height-(-2)],
                   [machine-1, origin-1 , duration-4, end-5 , height-1   ],
                   [machine-1, origin-4 , duration-2, end-6 , height-(-1)],
                   [machine-1, origin-2 , duration-3, end-5 , height-2   ],
                   [machine-1, origin-5 , duration-2, end-7 , height-2   ],
                   [machine-2, origin-3 , duration-2, end-5 , height-(-1)],
                   [machine-2, origin-1 , duration-4, end-5 , height-1   ]]],
                 ['PRODUCT'],
                 [ 1-[1,2,4],
                   2-[2,3,4],
                   3-2,
                   4-[3,5],
                   5-[2,3,4],
                   6-5,
                   7-[1,2,4],
                   8-[3,5],
                   9-[3,5],
                  11-[6,7],
                  13-7],
                 ['FOREACH'('MACHINES',[1-[1,2,3,4,5,6,7,8,9,15,16,17,18,19],
                                        2-[11,13,20,21]])],
                 '','',
                 [5,2.145,5,2.145]).

ctr_see_also(cumulatives,
 [link('assignment dimension removed', cumulative,           'negative %e not allowed',                        [heights]),
  link('generalisation',               diffn,                '%e with %e %e and %e attributes replaced by %k', [task,machine,assignment,origin,orthotope]),
  link('common keyword',               calendar,             '%k',                                             ['scheduling constraint']),
  link('common keyword',               coloured_cumulatives, '%k',                                             ['resource constraint']),
  link('used in graph description',    sum_ctr,              '',                                               [])]).

ctr_key_words(cumulatives,['scheduling constraint'                                   ,
                           'resource constraint'                                     ,
                           'temporal constraint'                                     ,
                           'timetabling constraint'                                  ,
                           'producer-consumer'                                       ,
                           'workload covering'                                       ,
                           'demand profile'                                          ,
                           'assignment dimension'                                    ,
                           'assignment to the same set of values'                    ,
                           'derived collection'                                      ,
                           'compulsory part'                                         ,
                           'sweep'                                                   ,
                           'sequencing with release times and deadlines'             ,
                           'scheduling with machine choice, calendars and preemption',
                           'zero-duration task'                                      ]).

ctr_persons(cumulatives,['Beldiceanu N.',
                         'Carlsson M.'  ]).

ctr_application(cumulatives, [1]).

ctr_eval(cumulatives, [builtin(cumulatives_b)]).

cumulatives_b(TASKS, MACHINES, =<) :-
    !,
    collection(TASKS, [dvar,dvar,dvar_gteq(0),dvar,dvar]),
    get_attr1(TASKS, VMACHINES),
    get_attr2(TASKS, ORIGINS  ),
    get_attr3(TASKS, DURATIONS),
    get_attr4(TASKS, ENDS     ),
    get_attr5(TASKS, HEIGHTS  ),
    collection(MACHINES, [int,int]),
    get_attr1(MACHINES, IDS       ),
    get_attr2(MACHINES, CAPACITIES),
    all_different(IDS),
    cumulatives1(VMACHINES, ORIGINS, DURATIONS, ENDS, HEIGHTS, Tasks),
    cumulatives2(IDS, CAPACITIES, Machines),
    cumulatives(Tasks, Machines, [bound(upper)]).
cumulatives_b(TASKS, MACHINES, >=) :-
    collection(TASKS, [dvar,dvar,dvar_gteq(0),dvar,dvar]),
    get_attr1(TASKS, VMACHINES),
    get_attr2(TASKS, ORIGINS  ),
    get_attr3(TASKS, DURATIONS),
    get_attr4(TASKS, ENDS     ),
    get_attr5(TASKS, HEIGHTS  ),
    collection(MACHINES, [int,int]),
    get_attr1(MACHINES, IDS       ),
    get_attr2(MACHINES, CAPACITIES),
    all_different(IDS),
    cumulatives1(VMACHINES, ORIGINS, DURATIONS, ENDS, HEIGHTS, Tasks),
    cumulatives2(IDS, CAPACITIES, Machines),
    cumulatives(Tasks, Machines, [bound(lower)]).

cumulatives1([], [], [], [], [], []).
cumulatives1([M|RM], [O|RO], [D|RD], [E|RE], [H|RH], [task(O,D,E,H,M)|R]) :-
    cumulatives1(RM, RO, RD, RE, RH, R).

cumulatives2([], [], []).
cumulatives2([I|RI], [C|RC], [machine(I,C)|R]) :-
    cumulatives2(RI, RC, R).
