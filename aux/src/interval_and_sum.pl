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

ctr_date(interval_and_sum,['20000128','20030820','20060810']).

ctr_origin(interval_and_sum, 'Derived from %c.', [cumulative]).

ctr_arguments(interval_and_sum,
              ['SIZE_INTERVAL'-int                         ,
               'TASKS'-collection(origin-dvar, height-dvar),
               'LIMIT'-int                                 ]).

ctr_exchangeable(interval_and_sum,
                 [items('TASKS',all),
                  translate(['TASKS'^origin]),
                  vals(['TASKS'^origin],intervals('SIZE_INTERVAL'),=,dontcare,dontcare),
                  vals(['TASKS'^height],int(>=(0)),>,dontcare,dontcare),
                  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_restrictions(interval_and_sum,
                 ['SIZE_INTERVAL' > 0              ,
                  required('TASKS',[origin,height]),
		  'TASKS'^origin  >= 0             ,
                  'TASKS'^height  >= 0             ,    
                  'LIMIT'         >= 0             ]).

ctr_typical(interval_and_sum,
            ['SIZE_INTERVAL'       > 1                  ,
             size('TASKS')         > 1                  ,
             range('TASKS'^origin) > 1                  ,
             range('TASKS'^height) > 1                  ,
             'LIMIT'               < sum('TASKS'^height)]).

ctr_contractible(interval_and_sum, [], 'TASKS', any).

ctr_graph(interval_and_sum,
          ['TASKS','TASKS'],
          2,
          ['PRODUCT'>>collection(tasks1,tasks2)],
          [tasks1^origin / 'SIZE_INTERVAL' = tasks2^origin / 'SIZE_INTERVAL'],
          [],
          [],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^height)])]],
          [sum_ctr(variables,=<,'LIMIT')]).

ctr_example(interval_and_sum,
            interval_and_sum(5,
                             [[origin-1 , height-2],
                              [origin-10, height-2],
                              [origin-10, height-3],
                              [origin-4 , height-1]],
                             5)).

ctr_draw_example(interval_and_sum,
                 ['TASKS','TASKS'],
                 [[[origin-1 , height-2],
                   [origin-10, height-2],
                   [origin-10, height-3],
                   [origin-4 , height-1]]],
                 ['PRODUCT'],
                 [1-[1,4],2-[2,3],3-[2,3],4-[1,4]],
                 ['COLLECTIONS'(['TASKS'-[1,2,3,4],'TASKS'-[5,6,7,8]])],
                 '','',
                 []).

ctr_see_also(interval_and_sum,
 [link('assignment dimension removed', sum_ctr,            '%k corresponding to intervals is removed', ['assignment dimension']),
  link('used in graph description',    sum_ctr,            '',                                         []),
  link('related',                      interval_and_count, '%c constraint replaced by %c',             [sum_ctr, among_low_up])]).

ctr_key_words(interval_and_sum,['timetabling constraint'          ,
                                'resource constraint'             ,
                                'temporal constraint'             ,
                                'assignment'                      ,
                                'assignment dimension'            ,
                                'interval'                        ,
                                'automaton'                       ,
                                'automaton with array of counters']).

ctr_application(interval_and_sum, [2]).

ctr_eval(interval_and_sum, [reformulation(interval_and_sum_r)]).

interval_and_sum_r(SIZE_INTERVAL, TASKS, LIMIT) :-
    check_type(int_gteq(1), SIZE_INTERVAL),
    collection(TASKS, [dvar_gteq(0),dvar_gteq(0)]),
    check_type(int_gteq(0), LIMIT),
    (TASKS = [] -> true ;
                   get_attr1(TASKS, ORIS),
                   get_attr2(TASKS, HEIGHTS),
                   get_maximum(ORIS, MAX),
                   MAXK is (MAX+SIZE_INTERVAL-1)//SIZE_INTERVAL,
                   interval_and_sum1(0, MAXK, SIZE_INTERVAL, LIMIT, ORIS, HEIGHTS)
    ).

interval_and_sum1(K, MAXK, _, _, _, _) :-
    K > MAXK, !.
interval_and_sum1(K, MAXK, SIZE_INTERVAL, LIMIT, ORIS, HEIGHTS) :-
    K =< MAXK,
    interval_and_sum2(ORIS, HEIGHTS, K, SIZE_INTERVAL, SUM),
    call(SUM #=< LIMIT),
    K1 is K+1,
    interval_and_sum1(K1, MAXK, SIZE_INTERVAL, LIMIT, ORIS, HEIGHTS).

interval_and_sum2([], [], _, _, 0).
interval_and_sum2([O|R], [H|S], K, SIZE_INTERVAL, H*B+T) :-
    SK is K*SIZE_INTERVAL,
    TK is SK+SIZE_INTERVAL-1,
    B #<=> O #>= SK #/\ O #=< TK,
    interval_and_sum2(R, S, K, SIZE_INTERVAL, T).
