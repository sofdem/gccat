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

ctr_date(stretch_circuit,['20030820','20060817','20090716']).

ctr_origin(stretch_circuit, '\\cite{Pesant01}', []).

ctr_usual_name(stretch_circuit, stretch).

ctr_arguments(stretch_circuit,
              ['VARIABLES'-collection(var-dvar)                ,
               'VALUES'-collection(val-int, lmin-int, lmax-int)]).

ctr_exchangeable(stretch_circuit,
                 [items('VARIABLES',shift),
                  items('VALUES',all),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(stretch_circuit,
                 [size('VARIABLES')   > 0                ,
                  required('VARIABLES',var)              ,
                  size('VALUES')      > 0                ,
                  required('VALUES',[val,lmin,lmax])     ,
                  distinct('VALUES',val)                 ,
                  'VALUES'^lmin      =< 'VALUES'^lmax    ,
                  'VALUES'^lmin      =< size('VARIABLES'),
                  sum('VALUES'^lmin) =< size('VARIABLES')]).

ctr_typical(stretch_circuit,
            [size('VARIABLES')      > 1                ,
             range('VARIABLES'^var) > 1                ,
             size('VARIABLES')      > size('VALUES')   ,
             size('VALUES')         > 1                ,
             'VALUES'^lmax         =< size('VARIABLES')]).

ctr_graph(stretch_circuit,
          ['VARIABLES'],
          2,
          foreach('VALUES',['CIRCUIT'>>collection(variables1,variables2),
                            'LOOP'>>collection(variables1,variables2)]),
          [variables1^var = 'VALUES'^val,
           variables2^var = 'VALUES'^val],
          [not_in('MIN_NCC',1,'VALUES'^lmin-1),
           'MAX_NCC' =< 'VALUES'^lmax         ],
          []).

ctr_example(stretch_circuit,
            stretch_circuit([[var-6],[var-6],[var-3],[var-1],
                             [var-1],[var-1],[var-6],[var-6]],
                            [[val-1, lmin-2, lmax-4],
                             [val-2, lmin-2, lmax-3],
                             [val-3, lmin-1, lmax-6],
                             [val-6, lmin-2, lmax-4]])).

ctr_draw_example(stretch_circuit,
                 ['VARIABLES'],
                 [[[var-6],[var-6],[var-3],[var-1],
                   [var-1],[var-1],[var-6],[var-6]]],
                 ['CIRCUIT','LOOP'],
                 [1-[1,2],
                  2-2,
                  3-3,
                  4-[4,5],
                  5-[5,6],
                  6-6,
                  7-[7,8],
                  8-[1,8]],
                 ['FOREACH'('VALUES',[1-[4,5,6],3-[3],6-[1,2,7,8]])],
                 '','1:MIN_NCC=3,MAX_NCC=3\\n3:MIN_NCC=1,MAX_NCC=1\\n6:MIN_NCC=4,MAX_NCC=4',
                 [2.4,2.5,2.6,2.525]).

ctr_see_also(stretch_circuit,
 [link('common keyword',        stretch_path,         '%k,%k', ['sliding sequence constraint', 'timetabling constraint']),
  link('common keyword',        pattern,              '%k,%k', ['sliding sequence constraint', 'timetabling constraint'], '\\\\ '),
  link('common keyword',        sliding_distribution, '%k',    ['sliding sequence constraint']),
  link('common keyword',        group,                '%k',    ['timetabling constraint'], '\\\\ '),
  link('used in reformulation', stretch_path,         '',      [])]).

ctr_key_words(stretch_circuit,['timetabling constraint'     ,
                               'sliding sequence constraint',
                               'cyclic'                     ,
                               'dynamic programming'        ,
                               'arc-consistency'            ,
                               'duplicated variables'       ]).

ctr_persons(stretch_circuit,['Pesant G.'  ,
                             'Rochart G.' ,
                             'Jussien N.' ,
                             'Hellsten L.',
                             'van Beek P.']).

ctr_eval(stretch_circuit, [reformulation(stretch_circuit_r)]).

stretch_circuit_r(VARIABLES, VALUES) :-
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int,int,int]),
    length(VARIABLES, N),
    stretch_circuit1(VALUES, 0, N, DELTA),
    prefix_length(VARIABLES, VARS_DELTA, DELTA),
    append(VARIABLES, VARS_DELTA, VARS),
    ND is N+DELTA,
    stretch_circuit2(VALUES, N, ND, VALS),
    eval(stretch_path(VARS, VALS)).

stretch_circuit1([], C, N, DELTA) :-
    DELTA is min(C,N).
stretch_circuit1([[_,_,_-L]|R], C, N, DELTA) :-
    M is max(L,C),
    stretch_circuit1(R, M, N, DELTA).

stretch_circuit2([], _, _, []).
stretch_circuit2([[A,B,lmax-L]|R], N, ND, [[A,B,lmax-LL]|S]) :-
    (L >= N -> LL = ND ; LL = L),
    stretch_circuit2(R, N, ND, S).
