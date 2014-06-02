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

ctr_date(coloured_cumulative,['20000128','20030820','20060805']).

ctr_origin(coloured_cumulative, 'Derived from %c and %c.', [cumulative,nvalues]).

ctr_arguments(coloured_cumulative,
              ['TASKS'-collection(origin-dvar, duration-dvar, end-dvar, colour-dvar),
               'LIMIT'-int                                                          ]).

ctr_exchangeable(coloured_cumulative,
                 [items('TASKS',all),
                  translate(['TASKS'^origin,'TASKS'^end]),
                  vals(['TASKS'^colour],int,=\=,all,dontcare),
                  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_synonyms(coloured_cumulative,[colored_cumulative]).

ctr_restrictions(coloured_cumulative,
                 [require_at_least(2,'TASKS',[origin,duration,end]),
                  required('TASKS',colour)                         ,
                  'TASKS'^duration >= 0                            ,
                  'TASKS'^origin   =< 'TASKS'^end                  ,
                  'LIMIT'          >= 0                            ]).

ctr_typical(coloured_cumulative,
            [size('TASKS') > 1,
             range('TASKS'^origin)   > 1                   ,
             range('TASKS'^duration) > 1                   ,
             range('TASKS'^end)      > 1                   ,
             range('TASKS'^colour)   > 1                   ,
             'LIMIT'                 < nval('TASKS'^colour)]).

ctr_contractible(coloured_cumulative, [], 'TASKS', any).

ctr_graph(coloured_cumulative,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin + tasks^duration = tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(coloured_cumulative,
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
                                  [item(var-'TASKS'^colour)])]],
          [nvalues(variables,=<,'LIMIT')]).

ctr_example(coloured_cumulative,
            coloured_cumulative([[origin-1, duration-2 , end-3 , colour-1],
                                 [origin-2, duration-9 , end-11, colour-2],
                                 [origin-3, duration-10, end-13, colour-3],
                                 [origin-6, duration-6 , end-12, colour-2],
                                 [origin-7, duration-2 , end-9 , colour-3]],2)).

ctr_draw_example(coloured_cumulative,
                 ['TASKS','TASKS'],
                 [[[origin-1, duration-2 , end-3 , colour-1],
                   [origin-2, duration-9 , end-11, colour-2],
                   [origin-3, duration-10, end-13, colour-3],
                   [origin-6, duration-6 , end-12, colour-2],
                   [origin-7, duration-2 , end-9 , colour-3]]],
                 ['PRODUCT'],
                 [1-1,
                  2-[1,2],
                  3-[2,3],
                  4-[2,3,4],
                  5-[2,3,4,5]],
                 ['COLLECTIONS'(['TASKS'-[1,2,3,4,5],'TASKS'-[6,7,8,9,10]])],
                 '','',
                 [2.3,3,4.2,3]).

ctr_see_also(coloured_cumulative,
 [link('assignment dimension added', coloured_cumulatives, '',                                                                                                               []),
  link('specialisation',             disjoint_tasks,       'a colour is assigned to each collection of tasks of constraint %c and a limit of one single colour is enforced', [disjoint_tasks]),
  link('common keyword',             cumulative,           '%k',                                                                                                             ['resource constraint']),
  link('common keyword',             track,                '%k',                                                                                                             ['resource constraint']),
  link('related',                    nvalue,               '',                                                                                                               []),
  link('used in graph description',  nvalues,              '',                                                                                                               [])]).

ctr_key_words(coloured_cumulative,['scheduling constraint'    ,
                                   'resource constraint'      ,
                                   'temporal constraint'      ,
                                   'coloured'                 ,
                                   'number of distinct values',
                                   'compulsory part'          ,
                                   'zero-duration task'       ]).

ctr_application(coloured_cumulative, [1]).

ctr_eval(coloured_cumulative, [reformulation(coloured_cumulative_r)]).

coloured_cumulative_r(TASKS, LIMIT) :-
    collection(TASKS, [dvar,dvar_gteq(0),dvar,dvar]),
    integer(LIMIT),
    LIMIT >= 0,
    get_attr1(TASKS, ORIGINS  ),
    get_attr2(TASKS, DURATIONS),
    get_attr3(TASKS, ENDS     ),
    get_attr4(TASKS, COLOURS  ),
    ori_dur_end(ORIGINS, DURATIONS, ENDS),
    coloured_cumulative1(ORIGINS, ENDS, COLOURS, 1, ORIGINS, ENDS, COLOURS, LIMIT).

coloured_cumulative1([], [], [], _, _, _, _, _).
coloured_cumulative1([Oi|RO], [Ei|RE], [Ci|RC], I, ORIGINS, ENDS, COLOURS, LIMIT) :-
    coloured_cumulative2(ORIGINS, ENDS, COLOURS, 1, I, Oi, Ei, Ci, COLi),
    Ni in 1..LIMIT,
    nvalue(Ni, COLi),
    I1 is I+1,
    coloured_cumulative1(RO, RE, RC, I1, ORIGINS, ENDS, COLOURS, LIMIT).

coloured_cumulative2([], [], [], _, _, _, _, _, []).
coloured_cumulative2([_|RO], [_|RE], [_|RC], J, I, Oi, Ei, Ci, [Ci|R]) :-
    I = J,
    !,
    J1 is J+1,
    coloured_cumulative2(RO, RE, RC, J1, I, Oi, Ei, Ci, R).
coloured_cumulative2([Oj|RO], [Ej|RE], [Cj|RC], J, I, Oi, Ei, Ci, [Cij|R]) :-
    I =\= J,
    K in 1..2,
%   Min is min(Ci,Cj),
%   Max is max(Ci,Cj),
    fd_min(Ci, Ci_min),
    fd_max(Ci, Ci_max),
    fd_min(Cj, Cj_min),
    fd_max(Cj, Cj_max),
    Min is min(Ci_min, Cj_min),
    Max is max(Ci_max, Cj_max),
    Cij in Min..Max,
    element(K, [Ci,Cj], Cij),
    ((Oj #=< Oi #/\ Ej #>  Oi) #/\ Cij #= Cj) #\/
    ((Oj #>  Oi #\/ Ej #=< Oi) #/\ Cij #= Ci),
    J1 is J+1,
    coloured_cumulative2(RO, RE, RC, J1, I, Oi, Ei, Ci, R).