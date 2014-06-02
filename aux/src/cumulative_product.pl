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

ctr_date(cumulative_product,['20030820','20060807','20081227']).

ctr_origin(cumulative_product, 'Derived from %c.', [cumulative]).

ctr_arguments(cumulative_product,
              ['TASKS'-collection(origin-dvar, duration-dvar, end-dvar,
                                  height-dvar                         ),
               'LIMIT'-int                                             ]).

ctr_exchangeable(cumulative_product,
                 [items('TASKS',all),
                  vals(['TASKS'^height],int(>=(0)),>,dontcare,dontcare),
                  translate(['TASKS'^origin,'TASKS'^end]),
                  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_restrictions(cumulative_product,
                 [require_at_least(2,'TASKS',[origin,duration,end]),
                  required('TASKS',height)                         ,
                  'TASKS'^duration >= 0                            ,
                  'TASKS'^origin   =< 'TASKS'^end                  ,
                  'TASKS'^height   >= 1                            ,
                  'LIMIT'          >= 0                            ]).

ctr_typical(cumulative_product,
            [size('TASKS')           > 1                   ,
             range('TASKS'^origin)   > 1                   ,
             range('TASKS'^duration) > 1                   ,
             range('TASKS'^end)      > 1                   ,
             range('TASKS'^height)   > 1                   ,
             'TASKS'^duration        > 0                   ,
             'LIMIT'                 < prod('TASKS'^height)]).

ctr_contractible(cumulative_product, [], 'TASKS', any).

ctr_graph(cumulative_product,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin+tasks^duration = tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(cumulative_product,
          ['TASKS','TASKS'],
          2,
          ['PRODUCT'>>collection(tasks1,tasks2)],
          [tasks1^duration > 0             ,
           tasks2^origin   =< tasks1^origin,
           tasks1^origin   <  tasks2^end   ],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'ITEMS'^height)])]],
          [product_ctr(variables,=<,'LIMIT')]).

ctr_example(cumulative_product,
            cumulative_product([[origin-1, duration-3 , end-4 , height-1],
                                [origin-2, duration-9 , end-11, height-2],
                                [origin-3, duration-10, end-13, height-1],
                                [origin-6, duration-6 , end-12, height-1],
                                [origin-7, duration-2 , end-9 , height-3]],
                               6)).

ctr_draw_example(cumulative_product,
                 ['TASKS','TASKS'],
                 [[[origin-1, duration-3 , end-4 , height-1],
                   [origin-2, duration-9 , end-11, height-2],
                   [origin-3, duration-10, end-13, height-1],
                   [origin-6, duration-6 , end-12, height-1],
                   [origin-7, duration-2 , end-9 , height-3]]],
                 ['PRODUCT'],
                 [1-1,
                  2-[1,2],
                  3-[1,2,3],
                  4-[2,3,4],
                  5-[2,3,4,5]],
                 ['COLLECTIONS'(['TASKS'-[1,2,3,4,5],'TASKS'-[6,7,8,9,10]])],
                 '','',
                 [2.3,3,4.2,3]).

ctr_see_also(cumulative_product,
 [link('common keyword',            cumulative,  '%k', ['resource constraint']),
  link('used in graph description', product_ctr, '',   [])]).

ctr_key_words(cumulative_product,['scheduling constraint',
                                  'resource constraint'  ,
                                  'temporal constraint'  ,
                                  'product'              ,
                                  'compulsory part'      ,
                                  'zero-duration task'   ]).

ctr_application(cumulative_product, [1]).

ctr_eval(cumulative_product, [reformulation(cumulative_product_r)]).

cumulative_product_r(TASKS, LIMIT) :-
    integer(LIMIT),
    LIMIT >= 1,
    collection(TASKS, [dvar,dvar_gteq(0),dvar,dvar(1,LIMIT)]),
    get_attr1(TASKS, ORIGINS  ),
    get_attr2(TASKS, DURATIONS),
    get_attr3(TASKS, ENDS     ),
    get_attr4(TASKS, HEIGHTS  ),
    ori_dur_end(ORIGINS, DURATIONS, ENDS),
    cumulative_product1(ORIGINS, ENDS, HEIGHTS, 1, ORIGINS, ENDS, HEIGHTS, LIMIT).

cumulative_product1([], [], [], _, _, _, _, _).
cumulative_product1([Oi|RO], [Ei|RE], [Hi|RH], I, ORIGINS, ENDS, HEIGHTS, LIMIT) :-
    cumulative_product2(ORIGINS, ENDS, HEIGHTS, 1, I, Oi, Ei, Hi, PRODi),
    call(PRODi #=< LIMIT),
    I1 is I+1,
    cumulative_product1(RO, RE, RH, I1, ORIGINS, ENDS, HEIGHTS, LIMIT).    

cumulative_product2([], [], [], _, _, _, _, _, 1).
cumulative_product2([_|RO], [_|RE], [_|RH], J, I, Oi, Ei, Hi, Hi*R) :-
    I = J,
    !,
    J1 is J+1,
    cumulative_product2(RO, RE, RH, J1, I, Oi, Ei, Hi, R).
cumulative_product2([Oj|RO], [Ej|RE], [Hj|RH], J, I, Oi, Ei, Hi, Hij*R) :-
    I =\= J,
    Hij in 1..Hj,
    ((Oj #=< Oi #/\ Ej #>  Oi) #/\ Hij #= Hj) #\/
    ((Oj #>  Oi #\/ Ej #=< Oi) #/\ Hij #= 1 ),
    J1 is J+1,
    cumulative_product2(RO, RE, RH, J1, I, Oi, Ei, Hi, R).
