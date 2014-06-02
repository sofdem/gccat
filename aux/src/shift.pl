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

ctr_date(shift,['20030820','20060814','20090531']).

ctr_origin(shift, 'N.~Beldiceanu', []).

ctr_arguments(shift,
              ['MIN_BREAK'-int                          ,
               'MAX_RANGE'-int                          ,
               'TASKS'-collection(origin-dvar, end-dvar)]).

ctr_exchangeable(shift,
                 [items('TASKS',all),
                  translate(['TASKS'^origin])]).

ctr_restrictions(shift,
                 ['MIN_BREAK' > 0               ,
                  'MAX_RANGE' > 0               ,
                  required('TASKS',[origin,end]),
                  'TASKS'^origin < 'TASKS'^end  ]).

ctr_typical(shift,
            ['MIN_BREAK'   > 1          ,
             'MAX_RANGE'   > 1          ,
             'MIN_BREAK'   < 'MAX_RANGE',
             size('TASKS') > 2          ]).

ctr_graph(shift,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^end >= tasks^origin              ,
           tasks^end - tasks^origin =< 'MAX_RANGE'],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(shift,
          ['TASKS'],
          2,
          ['CLIQUE'>>collection(tasks1,tasks2)],
          [(tasks2^origin >= tasks1^end #/\ tasks2^origin - tasks1^end =< 'MIN_BREAK' ) #\/
           (tasks1^origin >= tasks2^end #/\ tasks1^origin - tasks2^end =< 'MIN_BREAK' ) #\/
           (tasks2^origin <  tasks1^end #/\ tasks1^origin               < tasks2^end  )    ],
          [],
          [],
          ['CC'>>[variables-col('VARIABLES'-collection(var-dvar),
                                [item(var-'TASKS'^origin),
                                 item(var-'TASKS'^end   )])]],
          [range_ctr(variables,=<,'MAX_RANGE')]).

ctr_example(shift,
            shift(6,8,[[origin-17, end-20],
                       [origin-7 , end-10],
                       [origin-2 , end-4 ],
                       [origin-21, end-22],
                       [origin-5 , end-6 ]])).

ctr_draw_example(shift,
                 ['TASKS'],
                 [[[origin-17, end-20],
                   [origin-7 , end-10],
                   [origin-2 , end-4 ],
                   [origin-21, end-22],
                   [origin-5 , end-6 ]]],
                 ['CLIQUE'],
                 [1-[1,4],
                  2-[2,3,5],
                  3-[2,3,5],
                  4-[1,4],
                  5-[2,3,5]],
                 ['SET'([[1,4],[2,3,5]])],
                 '','',
                 [2.3,2.145,2.145,2.148]).

ctr_see_also(shift,
 [link('common keyword',            sliding_time_window, '%k', ['temporal constraint']),
  link('used in graph description', range_ctr,           '',   [])]).

ctr_key_words(shift,['scheduling constraint' ,
                     'timetabling constraint',
                     'temporal constraint'   ]).

ctr_persons(shift,['Beldiceanu N.']).

ctr_application(shift, [3]).

ctr_eval(shift, [reformulation(shift_r)]).

shift_r(MIN_BREAK, MAX_RANGE, TASKS) :-
    integer(MIN_BREAK),
    MIN_BREAK > 0,
    integer(MAX_RANGE),
    MAX_RANGE > 0,
    collection(TASKS, [dvar,dvar]),
    get_attr1(TASKS, ORIGINS),
    get_attr2(TASKS, ENDS),
    get_minimum(ORIGINS, MINO),
    get_maximum(ORIGINS, MAXO),
    get_minimum(ENDS, MINE),
    get_maximum(ENDS, MAXE),
    shift1(ORIGINS, ENDS, ORIGINS, ENDS, MINO, MAXO, MINE, MAXE, MIN_BREAK, MAX_RANGE).

shift1([], [], _, _, _, _, _, _, _, _).
shift1([O|RO], [E|RE], ORIGINS, ENDS, MINO, MAXO, MINE, MAXE, MIN_BREAK, MAX_RANGE) :-
    shift2(ORIGINS, ENDS, O, E, MIN_BREAK, MAX_RANGE, ORIBOOLS, ENDBOOLS),
    MIN in MINO..MAXO,
    MAX in MINE..MAXE,
    eval(open_minimum(MIN, ORIBOOLS)),
    eval(open_maximum(MAX, ENDBOOLS)),
    MAX-MIN #=< MAX_RANGE,
    shift1(RO, RE, ORIGINS, ENDS, MINO, MAXO, MINE, MAXE, MIN_BREAK, MAX_RANGE).

shift2([], [], _, _, _, _, [], []).
shift2([Oj|RO], [Ej|RE], Oi, Ei, MIN_BREAK, MAX_RANGE, [[var-Oj,bool-Bij]|ROB], [[var-Ej,bool-Bij]|REB]) :-
    Oi #< Ei,
    Bij #<=> (Oj #>= Ei #/\ Oj-Ei #=< MIN_BREAK) #\/
             (Oi #>= Ej #/\ Oi-Ej #=< MIN_BREAK) #\/
             (Oj #< Ei #/\ Oi #< Ej),
    shift2(RO, RE, Oi, Ei, MIN_BREAK, MAX_RANGE, ROB, REB).
