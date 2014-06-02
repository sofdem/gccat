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

ctr_date(track,['20030820','20060819','20090510']).

ctr_origin(track, '\\cite{Marte01}', []).

ctr_arguments(track,
              ['NTRAIL'-int,
               'TASKS'-collection(trail-int, origin-dvar, end-dvar)]).

ctr_exchangeable(track,
                 [items('TASKS',all),
                  vals(['TASKS'^trail],int,=\=,all,dontcare),
                  translate(['TASKS'^origin,'TASKS'^end])]).

ctr_restrictions(track,
                 ['NTRAIL'       > 0                  ,
                  'NTRAIL'       =< size('TASKS')     ,
		  size('TASKS')  > 0                  ,
                  required('TASKS',[trail,origin,end]),
                  'TASKS'^origin =< 'TASKS'^end       ]).

ctr_typical(track,
            ['NTRAIL'             < size('TASKS'),
             size('TASKS')        > 1            ,
             range('TASKS'^trail) > 1            ,
             'TASKS'^origin       < 'TASKS'^end  ]).

ctr_derived_collections(track,
    [col('TIME_POINTS'-collection(origin-dvar, end-dvar, point-dvar),
         [item(origin-'TASKS'^origin, end-'TASKS'^end, point-'TASKS'^origin),
          item(origin-'TASKS'^origin, end-'TASKS'^end, point-'TASKS'^end-1 )])]).

ctr_graph(track,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin =< tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(track,
          ['TIME_POINTS','TASKS'],
          2,
          ['PRODUCT'>>collection(time_points,tasks)],
          [time_points^end > time_points^origin,
           tasks^origin =< time_points^point   ,
           time_points^point < tasks^end       ],
          [],
          [],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^trail)])]],
          [nvalue('NTRAIL',variables)]).

ctr_example(track,
            track(2,
                  [[trail-1, origin-1, end-2],
                   [trail-2, origin-1, end-2],
                   [trail-1, origin-2, end-4],
                   [trail-2, origin-2, end-3],
                   [trail-2, origin-3, end-4]])).

ctr_draw_example(track,
                 ['TIME_POINTS','TASKS'],
                 [[[origin-1, end-2, point-1],
                   [origin-1, end-2, point-1],
                   [origin-1, end-2, point-1],
                   [origin-1, end-2, point-1],
                   [origin-2, end-4, point-2],
                   [origin-2, end-4, point-3],
                   [origin-2, end-3, point-2],
                   [origin-2, end-3, point-2],
                   [origin-3, end-4, point-3],
                   [origin-3, end-4, point-3]],
                  [[trail-1, origin-1, end-2],
                   [trail-2, origin-1, end-2],
                   [trail-1, origin-2, end-4],
                   [trail-2, origin-2, end-3],
                   [trail-2, origin-3, end-4]]],
                 ['PRODUCT'],
                 [ 1-[1,2],
                   2-[1,2],
                   3-[1,2],
                   4-[1,2],
                   5-[3,4],
                   6-[3,5],
                   7-[3,4],
                   8-[3,4],
                   9-[3,5],
                  10-[3,5]],
                 ['COLLECTIONS'(['TIME_POINTS'-[1,2,3,4,5,6,7,8,9,10],'TASKS'-[11,12,13,14,15]])],
                 '','',
                 [4.2,2.145,5.2,2.145]).

ctr_see_also(track,
 [link('implies (items to collection)', atleast_nvector,     '',   []),
  link('common keyword',                coloured_cumulative, '%k', ['resource constraint']),
  link('used in graph description',     nvalue,              '',   [])]).

ctr_key_words(track,['timetabling constraint',
                     'resource constraint'   ,
                     'temporal constraint'   ,
                     'derived collection'    ]).

ctr_persons(track,['Marte M.']).

ctr_application(track, [2]).

ctr_eval(track, [reformulation(track_r)]).

track_r(NTRAIL, TASKS) :-
    length(TASKS, N),
    check_type(dvar(1,N), NTRAIL),
    collection(TASKS, [int(1,N),dvar,dvar]),
    get_attr1(TASKS, TRAILS ),
    get_attr2(TASKS, ORIGINS),
    get_attr3(TASKS, ENDS   ),
    ori_end(ORIGINS, ENDS   ),
    track1(ORIGINS, ENDS, TRAILS, 1, ORIGINS, ENDS, TRAILS, NTRAIL),
    track3(ORIGINS, ENDS, TRAILS, 1, ORIGINS, ENDS, TRAILS, NTRAIL).

track1([], [], [], _, _, _, _, _).
track1([Oi|RO], [Ei|RE], [Ti|TC], I, ORIGINS, ENDS, TRAILS, NTRAIL) :-
    track2(ORIGINS, ENDS, TRAILS, 1, I, Oi, Ei, Ti, COLi),
    nvalue(NTRAIL, COLi),
    I1 is I+1,
    track1(RO, RE, TC, I1, ORIGINS, ENDS, TRAILS, NTRAIL).

track2([], [], [], _, _, _, _, _, []).
track2([_|RO], [_|RE], [_|RT], J, I, Oi, Ei, Ti, [Ti|R]) :-
    I = J,
    !,
    J1 is J+1,
    track2(RO, RE, RT, J1, I, Oi, Ei, Ti, R).
track2([Oj|RO], [Ej|RE], [Tj|RT], J, I, Oi, Ei, Ti, [Tij|R]) :-
    I =\= J,
    K in 1..2,
    Min is min(Ti,Tj),
    Max is max(Ti,Tj),
    Tij in Min..Max,
    element(K, [Ti,Tj], Tij),
    ((Oj #=< Oi #/\ Ej #>  Oi) #/\ Tij #= Tj) #\/
    ((Oj #>  Oi #\/ Ej #=< Oi) #/\ Tij #= Ti),
    J1 is J+1,
    track2(RO, RE, RT, J1, I, Oi, Ei, Ti, R).

track3([], [], [], _, _, _, _, _).
track3([Oi|RO], [Ei|RE], [Ti|TC], I, ORIGINS, ENDS, TRAILS, NTRAIL) :-
    track4(ORIGINS, ENDS, TRAILS, 1, I, Oi, Ei, Ti, COLi),
    nvalue(NTRAIL, COLi),
    I1 is I+1,
    track3(RO, RE, TC, I1, ORIGINS, ENDS, TRAILS, NTRAIL).

track4([], [], [], _, _, _, _, _, []).
track4([_|RO], [_|RE], [_|RT], J, I, Oi, Ei, Ti, [Ti|R]) :-
    I = J,
    !,
    J1 is J+1,
    track4(RO, RE, RT, J1, I, Oi, Ei, Ti, R).
track4([Oj|RO], [Ej|RE], [Tj|RT], J, I, Oi, Ei, Ti, [Tij|R]) :-
    I =\= J,
    K in 1..2,
    Min is min(Ti,Tj),
    Max is max(Ti,Tj),
    Tij in Min..Max,
    element(K, [Ti,Tj], Tij),
    ((Oj #=< Ei-1 #/\ Ej #>  Ei-1) #/\ Tij #= Tj) #\/
    ((Oj #>  Ei-1 #\/ Ej #=< Ei-1) #/\ Tij #= Ti),
    J1 is J+1,
    track4(RO, RE, RT, J1, I, Oi, Ei, Ti, R).
