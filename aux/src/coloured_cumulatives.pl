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

ctr_date(coloured_cumulatives,['20000128','20030820','20060805']).

ctr_origin(coloured_cumulatives, 'Derived from %c and %c.', [cumulatives,nvalues]).

ctr_arguments(coloured_cumulatives,
              ['TASKS'-collection(machine-dvar,
                                  origin-dvar , duration-dvar, end-dvar, 
                                  colour-dvar                          ),
               'MACHINES'-collection(id-int, capacity-int)              ]).

ctr_exchangeable(coloured_cumulatives,
                 [items('TASKS',all),
                  items('MACHINES',all),
                  vals(['MACHINES'^capacity],int,<,dontcare,dontcare),
                  vals(['TASKS'^machine,'MACHINES'^id],int,=\=,all,dontcare)]).

ctr_synonyms(coloured_cumulatives,[colored_cumulatives]).

ctr_restrictions(coloured_cumulatives,
                 [required('TASKS',[machine,colour])               ,
                  require_at_least(2,'TASKS',[origin,duration,end]),
                  'TASKS'^duration    >= 0                         ,
                  'TASKS'^origin      =< 'TASKS'^end               ,
                  required('MACHINES',[id,capacity])               ,
                  distinct('MACHINES',id)                          ,
                  'MACHINES'^capacity >= 0                         ]).

ctr_typical(coloured_cumulatives,
            [size('TASKS')           > 1                   ,
             range('TASKS'^machine)  > 1                   ,
             range('TASKS'^origin)   > 1                   ,
             range('TASKS'^duration) > 1                   ,
             range('TASKS'^end)      > 1                   ,
             range('TASKS'^colour)   > 1                   ,
             'TASKS'^duration        > 0                   ,
             size('MACHINES')        > 1                   ,
             'MACHINES'^capacity     > 0                   ,
             'MACHINES'^capacity     < nval('TASKS'^colour),
             size('TASKS')           > size('MACHINES')    ]).

ctr_contractible(coloured_cumulatives, [], 'TASKS', any).

ctr_graph(coloured_cumulatives,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin + tasks^duration = tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(coloured_cumulatives,
          ['TASKS','TASKS'],
          2,
          foreach('MACHINES',['PRODUCT'>>collection(tasks1,tasks2)]),
          [tasks1^machine  = 'MACHINES'^id  ,
           tasks1^machine  =  tasks2^machine,
           tasks1^duration >  0             ,
           tasks2^origin   =< tasks1^origin ,
           tasks1^origin   <  tasks2^end    ],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^colour)])]],
          [nvalues(variables,=<,'MACHINES'^capacity)]).

ctr_example(coloured_cumulatives,
            coloured_cumulatives(
               [[machine-1, origin-6, duration-6 , end-12, colour-2],
                [machine-1, origin-2, duration-9 , end-11, colour-3],
                [machine-2, origin-7, duration-3 , end-10, colour-3],
                [machine-1, origin-1, duration-2 , end-3 , colour-1],
                [machine-2, origin-4, duration-5 , end-9 , colour-3],
                [machine-1, origin-3, duration-10, end-13, colour-2]],
               [[id-1, capacity-2],
                [id-2, capacity-1]])).

ctr_draw_example(coloured_cumulatives,
                 ['TASKS','TASKS'],
                 [[[machine-1, origin-6, duration-6 , end-12, colour-2],
                   [machine-1, origin-2, duration-9 , end-11, colour-3],
                   [machine-2, origin-7, duration-3 , end-10, colour-3],
                   [machine-1, origin-1, duration-2 , end-3 , colour-1],
                   [machine-2, origin-4, duration-5 , end-9 , colour-3],
                   [machine-1, origin-3, duration-10, end-13, colour-2]]],
                 ['PRODUCT'],
                 [1-[1,2,6], 2-[2,4], 3-[3,5], 4-4, 5-5, 6-[2,6]],
                 ['FOREACH'('MACHINES',[1-[1,2,4,6,7,8,10,12],2-[3,5,9,11]])],
                 '','',
                 [2.3,3,4.2,3]).

ctr_see_also(coloured_cumulatives,
 [link('assignment dimension removed', cumulative,          '%e attribute removed and number of distinct %e replaced by sum of %e %e', [machine,colours,task,heights]),
  link('assignment dimension removed', coloured_cumulative, '%e attribute removed',                                                    [machine]),
  link('common keyword',               cumulative,          '%k',                                                                      ['resource constraint']),
  link('common keyword',               cumulatives,         '%k',                                                                      ['resource constraint']),
  link('related',                      nvalue,              '',                                                                        []),
  link('used in graph description',    nvalues,             '',                                                                        [])]).

ctr_key_words(coloured_cumulatives,['scheduling constraint'    ,
                                    'resource constraint'      ,
                                    'temporal constraint'      ,
                                    'coloured'                 ,
                                    'number of distinct values',
                                    'assignment dimension'     ,
                                    'compulsory part'          ,
                                    'zero-duration task'       ]).

ctr_application(coloured_cumulatives, [1]).

ctr_eval(coloured_cumulatives, [reformulation(coloured_cumulatives_r)]).

coloured_cumulatives_r(TASKS, MACHINES) :-
    collection(TASKS, [dvar,dvar,dvar_gteq(0),dvar,dvar]),
    get_attr1(TASKS, VMACHINES),
    get_attr2(TASKS, ORIGINS  ),
    get_attr3(TASKS, DURATIONS),
    get_attr4(TASKS, ENDS     ),
    get_attr5(TASKS, COLOURS  ),
    ori_dur_end(ORIGINS, DURATIONS, ENDS),
    collection(MACHINES, [int,int_gteq(0)]),
    get_attr1(MACHINES, IDS       ),
    get_attr2(MACHINES, CAPACITIES),
    all_different(IDS),
    get_maximum(CAPACITIES, CAPA_MAX),
    coloured_cumulatives1(VMACHINES, ORIGINS, ENDS, COLOURS, 1, VMACHINES, ORIGINS, ENDS, COLOURS, IDS, CAPACITIES, CAPA_MAX).

coloured_cumulatives1([], [], [], [], _, _, _, _, _, _, _, _).
coloured_cumulatives1([Mi|RM], [Oi|RO], [Ei|RE], [Ci|RC], I, VMACHINES, ORIGINS, ENDS, COLOURS, IDS, CAPACITIES, CAPA_MAX) :-
    coloured_cumulatives2(VMACHINES, ORIGINS, ENDS, COLOURS, 1, I, Mi, Oi, Ei, Ci, COLi),
    LIMIT in 0..CAPA_MAX,
    link_index_to_attribute(IDS, CAPACITIES, Mi, LIMIT),
    Ni in 0..CAPA_MAX,
    Ni #=< LIMIT,
    nvalue(Ni, COLi),
    I1 is I+1,
    coloured_cumulatives1(RM, RO, RE, RC, I1, VMACHINES, ORIGINS, ENDS, COLOURS, IDS, CAPACITIES, CAPA_MAX).

coloured_cumulatives2([], [], [], [], _, _, _, _, _, _, []).
coloured_cumulatives2([_|RM], [_|RO], [_|RE], [_|RC], J, I, Mi, Oi, Ei, Ci, [Ci|R]) :-
    I = J,
    !,
    J1 is J+1,
    coloured_cumulatives2(RM, RO, RE, RC, J1, I, Mi, Oi, Ei, Ci, R).
coloured_cumulatives2([Mj|RM], [Oj|RO], [Ej|RE], [Cj|RC], J, I, Mi, Oi, Ei, Ci, [Cij|R]) :-
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
    ((Mj #=  Mi #/\ Oj #=< Oi #/\ Ej #>  Oi) #/\ Cij #= Cj) #\/
    ((Mj #\= Mi #\/ Oj #>  Oi #\/ Ej #=< Oi) #/\ Cij #= Ci),
    J1 is J+1,
    coloured_cumulatives2(RM, RO, RE, RC, J1, I, Mi, Oi, Ei, Ci, R).
