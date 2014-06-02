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

ctr_date(derangement,['20000128','20030820','20040530','20060808']).

ctr_origin(derangement, 'Derived from %c.', [cycle]).

ctr_arguments(derangement,
              ['NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(derangement,
                 [items('NODES',all),
                  attrs_sync('NODES',[[index, succ]])]).

ctr_restrictions(derangement,
                 [size('NODES') > 1             ,
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(derangement,
            [size('NODES') > 2]).

ctr_graph(derangement,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ  =  nodes2^index,
           nodes1^succ =\= nodes1^index],
          ['NTREE' = 0],
          ['ONE_SUCC']).

ctr_example(derangement,
            derangement([[index-1, succ-2],
                         [index-2, succ-1],
                         [index-3, succ-5],
                         [index-4, succ-3],
                         [index-5, succ-4]])).

ctr_draw_example(derangement,
                 ['NODES'],
                 [[[index-1, succ-2],
                   [index-2, succ-1],
                   [index-3, succ-5],
                   [index-4, succ-3],
                   [index-5, succ-4]]],
                 ['CLIQUE'],
                 [1-2,2-1,3-5,4-3,5-4],
                 [],
                 '','NTREE=0',
                 [2.145,2.145,2,1.6]).

ctr_cond_imply(derangement, permutation, [], [], index_to_col).

ctr_see_also(derangement,
 [link('implies',                       twin,                   '',   []),
  link('implies (items to collection)', k_alldifferent,         '',   []),
  link('implies (items to collection)', lex_alldifferent,       '',   []),
  link('implied by',                    symmetric_alldifferent, '',   []),
  link('common keyword',                alldifferent,           '%k', [permutation]),
  link('common keyword',                cycle,                  '%k', [permutation])]).

ctr_key_words(derangement,['graph constraint'        ,
                           'permutation'             ,
                           'sort based reformulation',
                           'arc-consistency'         ,
			   'DFS-bottleneck'          ,
                           'one\\_succ'              ]).

ctr_persons(derangement,['Beldiceanu N.',
                         'Contejean E.' ]).

ctr_eval(derangement, [checker(derangement_c),
		       reformulation(derangement_r)]).

ctr_sol(derangement,2,0,2,1,-).
ctr_sol(derangement,3,0,3,2,-).
ctr_sol(derangement,4,0,4,9,-).
ctr_sol(derangement,5,0,5,44,-).
ctr_sol(derangement,6,0,6,265,-).
ctr_sol(derangement,7,0,7,1854,-).
ctr_sol(derangement,8,0,8,14833,-).
ctr_sol(derangement,9,0,9,133496,-).
ctr_sol(derangement,10,0,10,1334961,-).

derangement_r(NODES) :-
    length(NODES, N),
    collection(NODES, [int(1,N),dvar(1,N)]),
    get_attr1(NODES, INDEXES),
    get_attr2(NODES, SUCCS),
    all_different(INDEXES),
    derangement1(SUCCS, INDEXES),
    all_different(SUCCS).

derangement_c([[_,succ-V],[_,succ-V]|_]) :-
    !,
    fail.
derangement_c(NODES) :-
    length(NODES, N),
    collection(NODES, [int(1,N),int(1,N)]),
    get_attr1(NODES, INDEXES),
    get_attr2(NODES, SUCCS),
    derangement1_fix(SUCCS, INDEXES),
    sort(SUCCS, SSUCCS),
    length(SSUCCS, N),
    sort(INDEXES, SINDEXES),
    length(SINDEXES, N).
