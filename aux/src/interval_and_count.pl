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

ctr_date(interval_and_count,['20000128','20030820','20040530','20060810']).

ctr_origin(interval_and_count, '\\cite{Cousin93}', []).

ctr_arguments(interval_and_count,
              ['ATMOST'-int                                ,
               'COLOURS'-collection(val-int)               , 
               'TASKS'-collection(origin-dvar, colour-dvar),
               'SIZE_INTERVAL'-int                         ]).

ctr_exchangeable(interval_and_count,
                 [vals(['ATMOST'],int,<,dontcare,dontcare),
                  items('COLOURS',all),
                  items('TASKS',all),
                  translate(['TASKS'^origin]),
                  vals(['TASKS'^origin],intervals('SIZE_INTERVAL'),=,dontcare,dontcare),
                  vals(['TASKS'^colour],comp('COLOURS'^val),=,dontcare,dontcare)]).

ctr_restrictions(interval_and_count,
                 ['ATMOST'        >= 0             ,
                  required('COLOURS',val)          ,
                  distinct('COLOURS',val)          ,
                  required('TASKS',[origin,colour]),
		  'TASKS'^origin  >= 0             ,
                  'SIZE_INTERVAL' >  0             ]).

ctr_typical(interval_and_count,
            ['ATMOST'              > 0            ,
             'ATMOST'              < size('TASKS'),
             size('COLOURS')       > 0            ,
             size('TASKS')         > 1            ,
             range('TASKS'^origin) > 1            ,
             range('TASKS'^colour) > 1            ,
             'SIZE_INTERVAL'       > 1            ]).

ctr_contractible(interval_and_count, [], 'COLOURS', any).
ctr_contractible(interval_and_count, [], 'TASKS', any).

ctr_graph(interval_and_count,
          ['TASKS','TASKS'],
          2,
          ['PRODUCT'>>collection(tasks1,tasks2)],
          [tasks1^origin / 'SIZE_INTERVAL' = tasks2^origin / 'SIZE_INTERVAL'],
          [],
          [],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^colour)])]],
          [among_low_up(0,'ATMOST',variables,'COLOURS')]).

ctr_example(interval_and_count,
            interval_and_count(2,
                               [[val-4]],
                               [[origin-1 , colour-4],
                                [origin-0 , colour-9],
                                [origin-10, colour-4],
                                [origin-4 , colour-4]],
                               5)).

ctr_draw_example(interval_and_count,
                 ['TASKS','TASKS'],
                 [[[origin-1 , colour-4],
                   [origin-0 , colour-9],
                   [origin-10, colour-4],
                   [origin-4 , colour-4]]],
                 ['PRODUCT'],
                 [1-[1,2,4],2-[1,2,4],3-3,4-[1,2,4]],
                 ['COLLECTIONS'(['TASKS'-[1,2,3,4],'TASKS'-[5,6,7,8]])],
                 '','',
                 []).

ctr_see_also(interval_and_count,
 [link('assignment dimension removed', among_low_up,     '%k corresponding to intervals is removed', ['assignment dimension']),
  link('used in graph description',    among_low_up,     '',                                         []),
  link('related',                      interval_and_sum, '%c constraint replaced by %c',             [among_low_up, sum_ctr])]).

ctr_key_words(interval_and_count,['timetabling constraint'          ,
                                  'resource constraint'             ,
                                  'temporal constraint'             ,
                                  'assignment'                      ,
                                  'assignment dimension'            ,
                                  'interval'                        ,
                                  'coloured'                        ,
                                  'automaton'                       ,
                                  'automaton with array of counters']).

ctr_persons(interval_and_count,['Cousin X.']).

ctr_application(interval_and_count, [3]).

ctr_eval(interval_and_count, [reformulation(interval_and_count_r)]).

interval_and_count_r(ATMOST, COLOURS, TASKS, SIZE_INTERVAL) :-
    check_type(int_gteq(0), ATMOST),
    collection(COLOURS, [int]),
    get_attr1(COLOURS, COLS),
    all_different(COLS),
    collection(TASKS, [dvar_gteq(0),dvar]),
    check_type(int_gteq(1), SIZE_INTERVAL),
    (COLOURS = [] -> true ;
     TASKS   = [] -> true ;
                     get_attr1(TASKS, TORIS),
                     get_attr2(TASKS, TCOLS),
                     interval_and_count1(TCOLS, COLS, LB),
                     get_maximum(TORIS, MAX),
                     MAXK is (MAX+SIZE_INTERVAL-1)//SIZE_INTERVAL,
                     interval_and_count2(0, MAXK, SIZE_INTERVAL, ATMOST, LB, TORIS)
    ).

interval_and_count1([], _, []).
interval_and_count1([TC|R], COLS, [B|S]) :-
    build_or_var_in_values(COLS, TC, TERM),
    call(B #<=> TERM),
    interval_and_count1(R, COLS, S).

interval_and_count2(K, MAXK, _, _, _, _) :-
    K > MAXK, !.
interval_and_count2(K, MAXK, SIZE_INTERVAL, ATMOST, LB, TORIS) :-
    K =< MAXK,
    interval_and_count3(LB, TORIS, K, SIZE_INTERVAL, SUMB),
    call(SUMB #=< ATMOST),
    K1 is K+1,
    interval_and_count2(K1, MAXK, SIZE_INTERVAL, ATMOST, LB, TORIS).

interval_and_count3([], [], _, _, 0).
interval_and_count3([B|R], [O|S], K, SIZE_INTERVAL, BK+T) :-
    SK is K*SIZE_INTERVAL,
    TK is SK+SIZE_INTERVAL-1,
    BK #<=> B #/\ O #>= SK #/\ O #=< TK,
    interval_and_count3(R, S, K, SIZE_INTERVAL, T).
