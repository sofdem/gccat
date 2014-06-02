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

ctr_date(circuit,['20030820','20040530','20060805']).

ctr_origin(circuit, '\\cite{Lauriere78}', []).

ctr_arguments(circuit,
              ['NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(circuit,
                 [items('NODES',all)]).

ctr_synonyms(circuit,[atour,cycle]).

ctr_restrictions(circuit,
                 [required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ >= 1             ,
                  'NODES'^succ =< size('NODES') ]).

ctr_typical(circuit,
            [size('NODES') > 2]).

ctr_graph(circuit,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['MIN_NSCC' =  size('NODES'),
           'MAX_ID'   =< 1            ],
          ['ONE_SUCC']).

ctr_example(circuit,
            circuit([[index-1, succ-2],
                     [index-2, succ-3],
                     [index-3, succ-4],
                     [index-4, succ-1]])).

ctr_draw_example(circuit,
                 ['NODES'],
                 [[[index-1, succ-2],
                   [index-2, succ-3],
                   [index-3, succ-4],
                   [index-4, succ-1]]],
                 ['CLIQUE'],
                 [1-2,2-3,3-4,4-1],
                 ['MIN_NSCC'([1,2,3,4])],
                 '','MIN_NSCC=4,MAX_ID=1',
                 [2.145,2.145,2.8,2.4]).

ctr_cond_imply(circuit, cycle,          [],                  ['NCYCLE' = 1], [none,'NODES']).
ctr_cond_imply(circuit, derangement,    [size('NODES') > 1], [],             id            ).
ctr_cond_imply(circuit, k_alldifferent, [size('NODES') > 1], [],             same          ).
ctr_cond_imply(circuit, permutation,    [],                  [],             index_to_col  ).

ctr_see_also(circuit,
 [link('implies',                       alldifferent,       '',                                                []),
  link('implies',                       proper_circuit,     '',                                                []),
  link('implies',                       twin,               '',                                                []),
  link('implies (items to collection)', lex_alldifferent,   '',                                                []),
  link('generalisation',                cycle,              'introduce a variable for the number of circuits', []),
  link('common keyword',                circuit_cluster,    '%k, %k',                                          ['graph constraint', 'one\\_succ']),
  link('common keyword',                tour,               '%k, %k',                                          ['graph partitioning constraint', 'Hamiltonian']),
  link('common keyword',                path,               '%k, %k',                                          ['graph partitioning constraint', 'one\\_succ']),
  link('common keyword',                alldifferent,       '%k',                                              ['permutation']),
  link('common keyword',                proper_circuit,     '%k, %k',                                          ['permutation', 'one\\_succ']),
  link('related',                       strongly_connected, '',                                                [])]).

ctr_key_words(circuit,['graph constraint'             ,
                       'graph partitioning constraint',
                       'circuit'                      ,
                       'permutation'                  ,
                       'Hamiltonian'                  ,
                       'linear programming'           ,
                       'one\\_succ'                   ,
                       'planarity test'               ,
                       'strong bridge'                ,
                       'DFS-bottleneck'               ]).

ctr_persons(circuit,['Lauri\\`ere J.-L.',
                     'Althaus E.'       ,
                     'Bockmayr A.'      ,
                     'Elf M.'           ,
                     'Kasper T.'        ,
                     'J\\"unger M.'     ,
                     'Mehlhorn K.'      ,
                     'Shufet J. A.'     ,
                     'Berliner H. J.'   ,
                     'Hopcroft J.'      ,
                     'Tarjan R. E.'     ,
                     'Deo N.'           ,
                     'Grinberg E. Ya.'  ,
                     'Chv\\\'atal V.'   ,
                     'Kaya L. G.'       ,
                     'Hooker J. N.'     ]).

ctr_application(circuit, [1]).

ctr_eval(circuit, [builtin(circuit_b)]).

ctr_sol(circuit,2,0,2,1,-).
ctr_sol(circuit,3,0,3,2,-).
ctr_sol(circuit,4,0,4,6,-).
ctr_sol(circuit,5,0,5,24,-).
ctr_sol(circuit,6,0,6,120,-).
ctr_sol(circuit,7,0,7,720,-).
ctr_sol(circuit,8,0,8,5040,-).
ctr_sol(circuit,9,0,9,40320,-).
ctr_sol(circuit,10,0,10,362880,-).

circuit_b(NODES) :-
    length(NODES, N),
    collection(NODES, [int(1,N),dvar(1,N)]),
    get_attr1(NODES, INDEX),
    all_different(INDEX),
    sort_collection(NODES, index, SORTED_NODES),
    get_attr2(SORTED_NODES, SUCC),
    circuit(SUCC).
