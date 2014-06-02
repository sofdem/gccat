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

ctr_date(temporal_path,['20000128','20030820','20060818','20090511']).

ctr_origin(temporal_path, 'ILOG', []).

ctr_arguments(temporal_path,
              ['NPATH'-dvar                                                  ,
               'NODES'-collection(index-int, succ-dvar, start-dvar, end-dvar)]).

ctr_exchangeable(temporal_path,
                 [items('NODES',all),
                  translate(['NODES'^start,'NODES'^end])]).

ctr_restrictions(temporal_path,
                 ['NPATH'       >= 1                      ,
                  'NPATH'       =< size('NODES')          ,
                  required('NODES',[index,succ,start,end]),
                  size('NODES') >  0                      ,
                  'NODES'^index >= 1                      ,
                  'NODES'^index =< size('NODES')          ,
                  distinct('NODES',index)                 ,
                  'NODES'^succ  >= 1                      ,
                  'NODES'^succ  =< size('NODES')          ,
                  'NODES'^start =< 'NODES'^end            ]).

ctr_typical(temporal_path,
            ['NPATH'       < size('NODES'),
             size('NODES') > 1            ,
            'NODES'^start  < 'NODES'^end  ]).

ctr_functional_dependency(temporal_path, 1, [2]).

ctr_graph(temporal_path,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ  =  nodes2^index                               ,
           nodes1^succ  =  nodes1^index #\/ nodes1^end =< nodes2^start,
           nodes1^start =< nodes1^end                                 ,
           nodes2^start =< nodes2^end                                 ],
          ['MAX_ID'  =< 1            ,
           'NCC'     =  'NPATH'      ,
           'NVERTEX' =  size('NODES')],
          []).

ctr_example(temporal_path,
            temporal_path(2,[[index-1, succ-2, start-0, end-1 ],
                             [index-2, succ-6, start-3, end-5 ],
                             [index-3, succ-4, start-0, end-3 ],
                             [index-4, succ-5, start-4, end-6 ],
                             [index-5, succ-7, start-7, end-8 ],
                             [index-6, succ-6, start-7, end-9 ],
                             [index-7, succ-7, start-9, end-10]])).

ctr_draw_example(temporal_path,
                 ['NODES'],
                 [[[index-1, succ-2, start-0, end-1 ],
                   [index-2, succ-6, start-3, end-5 ],
                   [index-3, succ-4, start-0, end-3 ],
                   [index-4, succ-5, start-4, end-6 ],
                   [index-5, succ-7, start-7, end-8 ],
                   [index-6, succ-6, start-7, end-9 ],
                   [index-7, succ-7, start-9, end-10]]],
                 ['CLIQUE'],
                 [1-2,2-6,3-4,4-5,5-7,6-6,7-7],
                 ['MAX_ID'([2]),
                  'NCC'([[1,2,6],[3,4,5,7]]),
                  'NVERTEX'],
                 '','MAX_ID=1,NCC=2,NVERTEX=7',
                 [2.2,2.8,2.6,2.8]).

ctr_see_also(temporal_path,
 [link('implies (items to collection)', atleast_nvector, '',                       []),
  link('specialisation',                path,            'time dimension removed', []),
  link('common keyword',                path_from_to,    '%k',                     ['path'])]).

ctr_key_words(temporal_path,['graph constraint'             ,
                             'graph partitioning constraint',
                             'path'                         ,
                             'connected component'          ,
                             'sequence dependent set-up'    ,
                             'functional dependency'        ]).

ctr_persons(temporal_path,['Beldiceanu N.'                      ,
                           'Contejean E.'                       ,
                           'Labb\\\'e M.'                       ,
                           'Laporte G.'                         ,
                           'Rodr{\\\'\\i}guez-Mart{\\\'\\i}n I.']).

ctr_application(temporal_path, [2]).

ctr_eval(temporal_path, [reformulation(temporal_path_r)]).

temporal_path_r(NPATH, NODES) :-
    temporal_path0(NODES, SNODES),
    length(SNODES, N),
    N > 0,
    check_type(dvar(1,N), NPATH),
    collection(SNODES, [int(1,N),dvar(1,N),dvar,dvar]),
    get_attr1(SNODES, INDEXES),
    get_attr2(SNODES, SUCCS),
    get_attr3(SNODES, STARTS),
    get_attr4(SNODES, ENDS),
    all_different(INDEXES),
    ori_end(STARTS, ENDS),
    temporal_path1(INDEXES, SUCCS, TNODES),
    eval(path(NPATH,TNODES)),
    temporal_path2(SUCCS, ENDS, [], STARTS).

temporal_path0(NODES, SNODES) :-
    temporal_path0a(NODES, L),
    sort(L, S),
    temporal_path0a(SNODES, S), !.

temporal_path0a([], []).
temporal_path0a([[index-INDEX,succ-SUCC,start-START,end-END]|R],
                [INDEX-(SUCC,START,END)|T]) :-
    temporal_path0a(R, T).

temporal_path1([], [], []).
temporal_path1([INDEX|RINDEX], [SUCC|RSUCC], [[index-INDEX,succ-SUCC]|RNODES]) :-
    temporal_path1(RINDEX, RSUCC, RNODES).

temporal_path2([], [], _, _).
temporal_path2([SUCCi|RSUCC], [ENDi|REND], PREV_STARTS, [_STARTi|RSTART]) :-
    append(PREV_STARTS, [ENDi], NEW_PREV_STARTS),
    append(NEW_PREV_STARTS, RSTART, TABLE),
    element(SUCCi, TABLE, START_SUCCi),
    ENDi #=< START_SUCCi,
    temporal_path2(RSUCC, REND, NEW_PREV_STARTS, RSTART).
