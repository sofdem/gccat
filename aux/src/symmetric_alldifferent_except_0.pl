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

ctr_date(symmetric_alldifferent_except_0,['20120208']).

ctr_origin(symmetric_alldifferent_except_0, 'Derived from %c', [symmetric_alldifferent]).

ctr_arguments(symmetric_alldifferent_except_0,
              ['NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(symmetric_alldifferent_except_0,
                 [items('NODES',all)]).

ctr_synonyms(symmetric_alldifferent_except_0,[symmetric_alldiff_except_0    ,
                                              symmetric_alldistinct_except_0,
                                              symm_alldifferent_except_0    ,
                                              symm_alldiff_except_0         ,
                                              symm_alldistinct_except_0     ]).

ctr_restrictions(symmetric_alldifferent_except_0,
                 [required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 0            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(symmetric_alldifferent_except_0,
            [size('NODES')       >= 4,
	     minval('NODES'^succ) = 0,
	     maxval('NODES'^succ) > 0]).

ctr_predefined(symmetric_alldifferent_except_0).

ctr_example(symmetric_alldifferent_except_0,
            [symmetric_alldifferent_except_0([[index-1, succ-3],
                                              [index-2, succ-0],
                                              [index-3, succ-1],
                                              [index-4, succ-0]])]).

ctr_cond_imply(symmetric_alldifferent_except_0, alldifferent_except_0, [], [], index_to_col).

ctr_see_also(symmetric_alldifferent_except_0,
 [link('implies (items to collection)', k_alldifferent,         '', []),
  link('implies (items to collection)', lex_alldifferent,       '', []),
  link('implied by'                   , symmetric_alldifferent, '', [])]).

ctr_key_words(symmetric_alldifferent_except_0,['predefined constraint' ,
					       'timetabling constraint',
                                               'sport timetabling'     ,
                                               'joker value'           ,
					       'matching'              ]).

ctr_persons(symmetric_alldifferent_except_0,['Cymer R.']).

ctr_eval(symmetric_alldifferent_except_0, [checker(symmetric_alldifferent_except_0_c)      ,
					   reformulation(symmetric_alldifferent_except_0_r),
					   density(symmetric_alldifferent_except_0_d)      ]).

ctr_sol(symmetric_alldifferent_except_0,2,0,2,2,-).
ctr_sol(symmetric_alldifferent_except_0,3,0,3,4,-).
ctr_sol(symmetric_alldifferent_except_0,4,0,4,10,-).
ctr_sol(symmetric_alldifferent_except_0,5,0,5,26,-).
ctr_sol(symmetric_alldifferent_except_0,6,0,6,76,-).
ctr_sol(symmetric_alldifferent_except_0,7,0,7,232,-).
ctr_sol(symmetric_alldifferent_except_0,8,0,8,764,-).

symmetric_alldifferent_except_0_r([]) :- !.
symmetric_alldifferent_except_0_r(NODES) :-
    symmetric_alldifferent0(NODES, SNODES),
    length(SNODES, N),
    collection(SNODES, [int(1,N),dvar(0,N)]),
    get_attr1(SNODES, INDEXES),
    get_attr2(SNODES, SUCCS),
    all_different(INDEXES),
    derangement1(SUCCS, INDEXES),
    symmetric_alldifferent1(SUCCS, 1, SUCCS).

symmetric_alldifferent_except_0_c([]) :- !.
symmetric_alldifferent_except_0_c(NODES) :-
    length(NODES, N),
    symmetric_alldifferent0(NODES, SNODES),
    length(SNODES, N),
    collection(SNODES, [int(1,N),int(0,N)]),
    get_attr1(SNODES, INDEXES),
    get_attr2(SNODES, SUCCS),
    sort(INDEXES, SINDEXES),
    length(SINDEXES, N),
    sym_pairs_skip_zeros(INDEXES, SUCCS, PAIRS),
    keysort(PAIRS, SPAIRS),
    symmetric_alldifferent_check(SPAIRS).

sym_pairs_skip_zeros([], [], []) :- !.
sym_pairs_skip_zeros([_I|R], [0|S], T) :- !,
    sym_pairs_skip_zeros(R, S, T).
sym_pairs_skip_zeros([I|R], [J|S], [I-a(J),J-b(I)|T]) :-
    sym_pairs_skip_zeros(R, S, T).

symmetric_alldifferent_except_0_d(Density, NODES) :-
    get_attr2(NODES, SUCCS),
    sort(SUCCS, SORTED),
    length(SUCCS, N),
    length(SORTED, S),
    Density is S / N.
