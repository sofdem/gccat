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

ctr_date(inverse,['20000128','20030820','20040530','20060810']).

ctr_origin(inverse, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(inverse,
              ['NODES'-collection(index-int, succ-dvar, pred-dvar)]).

ctr_exchangeable(inverse,
                 [items('NODES',all),
                  attrs_sync('NODES',[[index],[succ,pred]])]).

ctr_synonyms(inverse,[assignment,channel,inverse_channeling]).

ctr_restrictions(inverse,
                 [required('NODES',[index,succ,pred]),
                  'NODES'^index >= 1                 ,
                  'NODES'^index =< size('NODES')     ,
                  distinct('NODES',index)            ,
                  'NODES'^succ  >= 1                 ,
                  'NODES'^succ  =< size('NODES')     ,
                  'NODES'^pred  >= 1                 ,
                  'NODES'^pred  =< size('NODES')     ]).

ctr_typical(inverse,
            [size('NODES') > 1]).

ctr_pure_functional_dependency(inverse, []).
ctr_functional_dependency(inverse, 1-2, [1-1,1-3]).
ctr_functional_dependency(inverse, 1-3, [1-1,1-2]).

ctr_graph(inverse,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index,
           nodes2^pred = nodes1^index],
          ['NARC' = size('NODES')],
          []).

ctr_example(inverse,
            inverse([[index-1, succ-2, pred-2],
                     [index-2, succ-1, pred-1],
                     [index-3, succ-5, pred-4],
                     [index-4, succ-3, pred-5],
                     [index-5, succ-4, pred-3]])).

ctr_draw_example(inverse,
                 ['NODES'],
                 [[[index-1, succ-2, pred-2],
                   [index-2, succ-1, pred-1],
                   [index-3, succ-5, pred-4],
                   [index-4, succ-3, pred-5],
                   [index-5, succ-4, pred-3]]],
                 ['CLIQUE'],
                 [1-2,2-1,3-5,4-3,5-4],
                 ['NARC'],
                 '','NARC=5',
                 [2.145,2.145,1.7,1.7]).

ctr_see_also(inverse,
 [link('implies (items to collection)', lex_alldifferent,      '',                                                                                                     []),
  link('generalisation',                inverse_offset,        'do not assume anymore that the smallest value of the %e or %e attributes is equal to %e', [pred, succ, 1]),
  link('generalisation',                inverse_set,           '%e replaced by %e %e',                                                 ['domain~variable', set, variable]),
  link('generalisation',                inverse_within_range,  'partial mapping between two collections of distinct size',                                             []),
  link('common keyword',                cycle,                 '%k',                                                                                      ['permutation']),
  link('common keyword',                symmetric_alldifferent,'%k',                                                                                      ['permutation'])]).

ctr_key_words(inverse,['graph constraint'                ,
                       'channelling constraint'          ,
                       'permutation channel'             ,
                       'permutation'                     ,
		       'bipartite matching'              ,
                       'dual model'                      ,
		       'functional dependency'           ,
		       'pure functional dependency'      ,
                       'heuristics'                      ,
                       'n-Amazons'                       ,
                       'n-queens'                        ,
                       'zebra puzzle'                    ,
                       'automaton'                       ,
                       'automaton with array of counters',
                       'arc-consistency'                 ]).

ctr_persons(inverse,['Cymer R.']).

ctr_eval(inverse, [reformulation(inverse_r)]).

inverse_r([]) :- !.
inverse_r(NODES) :-
    length(NODES, N),
    collection(NODES, [int(1,N),dvar(1,N),dvar(1,N)]),
    get_attr1(NODES, INDEXES),
    get_attr2(NODES, SUCCS),
    get_attr3(NODES, PREDS),
    all_different(INDEXES),
    all_different(SUCCS),
    all_different(PREDS),
    inverse1(SUCCS, INDEXES, PREDS, INDEXES).

inverse1([], [], _, _).
inverse1([S_I|R], [I|S], PREDS, INDEXES) :-
    inverse2(PREDS, INDEXES, S_I, I),
    inverse1(R, S, PREDS, INDEXES).

inverse2([], [], _, _).
inverse2([P_J|R], [J|S], S_I, I) :-
    S_I #= J #<=> P_J #= I,
    inverse2(R, S, S_I, I).
