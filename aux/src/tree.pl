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

ctr_date(tree,['20000128','20030820','20060819']).

ctr_origin(tree, 'N.~Beldiceanu', []).

ctr_arguments(tree,
              ['NTREES'-dvar                            ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(tree,
                 [items('NODES',all)]).

ctr_restrictions(tree,
                 ['NTREES'      >= 1            ,
                  'NTREES'      =< size('NODES'),
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(tree,
            ['NTREES'      < size('NODES'),
             size('NODES') > 2            ]).

ctr_functional_dependency(tree, 1, [2]).

ctr_graph(tree,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['MAX_NSCC' =< 1      ,
           'NCC'      =  'NTREES'],
          []).

ctr_example(tree,
            [tree(2,
                 [[index-1, succ-1],
                  [index-2, succ-5],
                  [index-3, succ-5],
                  [index-4, succ-7],
                  [index-5, succ-1],
                  [index-6, succ-1],
                  [index-7, succ-7],
                  [index-8, succ-5]]),
               tree(8,
                 [[index-1, succ-1],
                  [index-2, succ-2],
                  [index-3, succ-3],
                  [index-4, succ-4],
                  [index-5, succ-5],
                  [index-6, succ-6],
                  [index-7, succ-7],
                  [index-8, succ-8]]),
               tree(7,
                 [[index-1, succ-6],
                  [index-2, succ-2],
                  [index-3, succ-3],
                  [index-4, succ-4],
                  [index-5, succ-5],
                  [index-6, succ-6],
                  [index-7, succ-7],
                  [index-8, succ-8]])
	     ]).

ctr_draw_example(tree,
                 ['NODES'],
                 [[[index-1, succ-1],
                   [index-2, succ-5],
                   [index-3, succ-5],
                   [index-4, succ-7],
                   [index-5, succ-1],
                   [index-6, succ-1],
                   [index-7, succ-7],
                   [index-8, succ-5]]],
                 ['CLIQUE'],
                 [1-1,2-5,3-5,4-7,5-1,6-1,7-7,8-5],
                 ['NCC'([[1,2,3,5,6,8],[4,7]])],
                 '','MAX_NSCC=1,NCC=2',
                 [2.145,2.43,2.4,2.3]).

ctr_see_also(tree,
 [link('implied by',                    binary_tree,                       '',                                                                    []),
  link('implies (items to collection)', atleast_nvector,                   '',                                                                    []),
  link('specialisation',                binary_tree,                       'no limit on the number of children replaced by at most two children', []),
  link('specialisation',                path,                              'no limit on the number of children replaced by at most one child',    []),
  link('common keyword',                cycle,                             '%k',                                   ['graph partitioning constraint']),
  link('common keyword',                map,                               '%k',                                   ['graph partitioning constraint']),
  link('common keyword',                graph_crossing,                    '%k',                                   ['graph partitioning constraint']),
  link('common keyword',                proper_forest,                     '%k,%k',                                ['connected component', 'tree']),
  link('shift of concept',              tree_range,                        '',                                     []),
  link('shift of concept',              tree_resource,                     '',                                     []),
  link('shift of concept',              stable_compatibility,              '',                                     []),
  link('uses in its reformulation',     tree_range,                        '',                                     []),
  link('uses in its reformulation',     tree_resource,                     '',                                     []),
  link('related',                       global_cardinality_low_up_no_loop, 'can be used for restricting number of children since discard loops associated with tree roots', []),
  link('related',                       global_cardinality_no_loop,        'can be used for restricting number of children since discard loops associated with tree roots', []),
  link('related',                       balance_tree,                      'counting number of trees versus controlling how balanced the trees are',                        [])]).

ctr_key_words(tree,['graph constraint'             ,
                    'graph partitioning constraint',
                    'connected component'          ,
		    'DFS-bottleneck'               ,
                    'tree'                         ,
                    'one\\_succ'                   ,
                    'strong articulation point'    ,
                    'arc-consistency'              ,
                    'functional dependency'        ]).

ctr_persons(tree,['Beldiceanu N.'  ,
                  'Flener P.'      ,
                  'Lorca X.'       ,
                  'Dooms G.'       ,
                  'Katriel I.'     ,
                  'R\\\'egin J.-C.',
                  'Rousseau L.-M.' ,
                  'Rueher M.'      ,
                  'Sloane N. J. A.',
                  'van Hoeve W.-J.',
                  'Italiano G. F.' ,
                  'Laura L.'       ,
                  'Santaroni F.'   ,
                  'Fages J.-G.'    ]).

ctr_application(tree, [2]).

ctr_eval(tree, [reformulation(tree_r)]).

ctr_sol(tree,2,0,2,3,[1-2,2-1]).
ctr_sol(tree,3,0,3,16,[1-9,2-6,3-1]).
ctr_sol(tree,4,0,4,125,[1-64,2-48,3-12,4-1]).
ctr_sol(tree,5,0,5,1296,[1-625,2-500,3-150,4-20,5-1]).
ctr_sol(tree,6,0,6,16807,[1-7776,2-6480,3-2160,4-360,5-30,6-1]).
ctr_sol(tree,7,0,7,262144,[1-117649,2-100842,3-36015,4-6860,5-735,6-42,7-1]).
ctr_sol(tree,8,0,8,4782969,[1-2097152,2-1835008,3-688128,4-143360,5-17920,6-1344,7-56,8-1]).

tree_r(NTREES, NODES) :-
    length(NODES, N),
    check_type(dvar(1,N), NTREES),
    collection(NODES, [int(1,N),dvar(1,N)]),
    get_attr1(NODES, INDEXES),
    get_attr2(NODES, SUCCS),
    all_different(INDEXES),
    length(RANKS, N),
    domain(RANKS, 1, N),
    tree1(SUCCS, RANKS, INDEXES, RANKS, INDEXES, Term),
    call(NTREES #= Term).

tree1([], [], _, _, _, 0).
tree1([S|U], [R|P], [I|K], RANKS, INDEXES, B+T) :-
    S #= I #<=> B,
    tree2(S, R, I, RANKS, INDEXES),
    tree1(U, P, K, RANKS, INDEXES, T).

tree2(_, _, _, _, []) :- !.
tree2(S_I, R_I, I, [R_J|P], [J|K]) :-
    S_I #= J #/\ I #\= J #=> R_I #< R_J,
    tree2(S_I, R_I, I, P, K).
