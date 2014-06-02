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

ctr_date(symmetric_alldifferent_loop,['20120221']).

ctr_origin(symmetric_alldifferent_loop, 'Derived from %c', [symmetric_alldifferent]).

ctr_arguments(symmetric_alldifferent_loop,
              ['NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(symmetric_alldifferent_loop,
                 [items('NODES',all)]).

ctr_synonyms(symmetric_alldifferent_loop,[symmetric_alldiff_loop    ,
                                          symmetric_alldistinct_loop,
                                          symm_alldifferent_loop    ,
                                          symm_alldiff_loop         ,
                                          symm_alldistinct_loop     ]).

ctr_restrictions(symmetric_alldifferent_loop,
                 [required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(symmetric_alldifferent_loop,
            [size('NODES') >= 4]).

ctr_graph(symmetric_alldifferent_loop,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index,
           nodes2^succ = nodes1^index],
          ['NARC' = size('NODES')],
          []).

ctr_example(symmetric_alldifferent_loop,
            [symmetric_alldifferent_loop([[index-1, succ-1],
                                          [index-2, succ-4],
                                          [index-3, succ-3],
                                          [index-4, succ-2]])]).

ctr_draw_example(symmetric_alldifferent_loop,
                 ['NODES'],
                 [[[index-1, succ-1],
                   [index-2, succ-4],
                   [index-3, succ-3],
                   [index-4, succ-2]]],
                 ['CLIQUE'],
                 [1-1,2-4,3-3,4-2],
                 ['NARC'],
                 '','NARC=4',
                 [2.145,2.145,2.145,2.145]).

ctr_cond_imply(symmetric_alldifferent_loop, permutation, [], [], index_to_col).

ctr_see_also(symmetric_alldifferent_loop,
 [link('implies (items to collection)', lex_alldifferent,       '', []),
  link('implies',                       twin,                   '', []),
  link('implied by',                    symmetric_alldifferent, '', [])]).

ctr_key_words(symmetric_alldifferent_loop,['graph constraint'             ,
                                           'circuit'                      ,
                                           'cycle'                        ,
                                           'permutation'                  ,
				           'involution'                   ,
                                           'all different'                ,
                                           'disequality'                  ,
					   'matching'                     ,
                                           'graph partitioning constraint']).

ctr_persons(symmetric_alldifferent_loop,['Cymer R.']).

ctr_eval(symmetric_alldifferent_loop, [checker(symmetric_alldifferent_loop_c),
				       reformulation(symmetric_alldifferent_loop_r1),
				       reformulation(symmetric_alldifferent_loop_r2)]).

ctr_sol(symmetric_alldifferent_loop,2,0,2,2,-).
ctr_sol(symmetric_alldifferent_loop,3,0,3,4,-).
ctr_sol(symmetric_alldifferent_loop,4,0,4,10,-).
ctr_sol(symmetric_alldifferent_loop,5,0,5,26,-).
ctr_sol(symmetric_alldifferent_loop,6,0,6,76,-).
ctr_sol(symmetric_alldifferent_loop,7,0,7,232,-).
ctr_sol(symmetric_alldifferent_loop,8,0,8,764,-).
ctr_sol(symmetric_alldifferent_loop,9,0,9,2620,-).
ctr_sol(symmetric_alldifferent_loop,10,0,10,9496,-).

symmetric_alldifferent_loop_r1(NODES) :-
    symmetric_alldifferent_loop_r1a(NODES, INODES),
    eval(inverse(INODES)).

symmetric_alldifferent_loop_r1a([], []).
symmetric_alldifferent_loop_r1a([[index-INDEX,succ-SUCC]|R], [[index-INDEX,succ-SUCC,pred-SUCC]|S]) :-
    symmetric_alldifferent_loop_r1a(R, S).

symmetric_alldifferent_loop_r2([]) :- !.
symmetric_alldifferent_loop_r2(NODES) :-
    symmetric_alldifferent0(NODES, SNODES),
    length(SNODES, N),
    collection(SNODES, [int(1,N),dvar(1,N)]),
    get_attr1(SNODES, INDEXES),
    get_attr2(SNODES, SUCCS),
    all_different(INDEXES),
    symmetric_alldifferent1(SUCCS, 1, SUCCS).

symmetric_alldifferent_loop_c([]) :- !.
symmetric_alldifferent_loop_c(NODES) :-
    length(NODES, N),
    symmetric_alldifferent0(NODES, SNODES),
    length(SNODES, N),
    collection(SNODES, [int(1,N),int(1,N)]),
    get_attr1(SNODES, INDEXES),
    get_attr2(SNODES, SUCCS),
    sort(INDEXES, SINDEXES),
    length(SINDEXES, N),
    sym_pairs_skip_loops(INDEXES, SUCCS, PAIRS),
    keysort(PAIRS, SPAIRS),
    symmetric_alldifferent_check(SPAIRS).

sym_pairs_skip_loops([], [], []) :- !.
sym_pairs_skip_loops([I|R], [I|S], T) :- !,
    sym_pairs_skip_loops(R, S, T).
sym_pairs_skip_loops([I|R], [J|S], [I-a(J),J-b(I)|T]) :-
    sym_pairs_skip_loops(R, S, T).
