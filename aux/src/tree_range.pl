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

ctr_date(tree_range,['20030820','20040727','20060819','20090923']).

ctr_origin(tree_range, 'Derived from %c.', [tree]).

ctr_arguments(tree_range,
              ['NTREES'-dvar                           ,
               'R'-dvar                                ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(tree_range,
                 [items('NODES',all)]).

ctr_restrictions(tree_range,
                 ['NTREES'  >= 0                ,
                  'R'       >= 0                ,
                  'R'       <  size('NODES')    ,
                  size('NODES') > 0             ,
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(tree_range,
            ['NTREES'      < size('NODES'),
             size('NODES') > 2            ]).

ctr_functional_dependency(tree_range, 1, [3]).
ctr_functional_dependency(tree_range, 2, [3]).

ctr_graph(tree_range,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['MAX_NSCC' =< 1       ,
           'NCC'      =  'NTREES',
           'RANGE_DRG'=  'R'     ],
          []).

ctr_example(tree_range,
            tree_range(2,1,
                       [[index-1, succ-1],
                        [index-2, succ-5],
                        [index-3, succ-5],
                        [index-4, succ-7],
                        [index-5, succ-1],
                        [index-6, succ-1],
                        [index-7, succ-7],
                        [index-8, succ-5]])).

ctr_draw_example(tree_range,
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
                 [ 1-1,2-5,3-5,4-7,5-1,6-1,7-7,8-5],
                 ['NCC'([[1,2,3,5,6,8],[4,7]]),
                  'RANGE_DRG'([4-7],[3-5,5-1])],
                 '','MAX_NSCC=1,NCC=2\\nRANGE_DRG=2-1=1',
                 [2.145,2.43,2.4,2.3]).

ctr_see_also(tree_range,
 [link('root concept',          tree,               '',                                         []),
  link('used in reformulation', tree,               '',                                         []),
  link('used in reformulation', domain,             '',                                         []),
  link('used in reformulation', element,            '',                                         []),
  link('used in reformulation', global_cardinality, '',                                         []),
  link('used in reformulation', open_minimum,       '',                                         []),
  link('used in reformulation', maximum,            '',                                         []),
  link('related',               balance,            'balanced tree versus balanced assignment', [])]).

ctr_key_words(tree_range,['graph constraint'             ,
                          'graph partitioning constraint',
                          'connected component'          ,
                          'tree'                         ,
                          'balanced tree'                ,
                          'functional dependency'        ]).

ctr_application(tree_range, [3]).

ctr_eval(tree_range, [reformulation(tree_range_r)]).

tree_range_r(NTREES, R, NODES) :-
    tree_range0(NODES, SNODES),
	length(SNODES, N),
	N > 0,
	N1 is N-1,
	check_type(dvar(1,N), NTREES),
	check_type(dvar(0,N1), R),
	collection(SNODES, [int(1,N),dvar(1,N)]),
	get_attr1(SNODES, INDEXES),
	get_attr2(SNODES, SUCCS),
	all_different(INDEXES),
    eval(tree(NTREES, SNODES)),
    tree_range1(INDEXES, SUCCS, DISTS1, DISTS2, OCCS1, OCCS2, SUCCS1, LS, OLS),
    eval(domain(DISTS1, 0, N)),
    tree_range2(INDEXES, SUCCS, N, [], DISTS2),
    eval(domain(OCCS1, 0, N)),
    eval(global_cardinality(SUCCS1, OCCS2)),
    eval(domain(LS, 0, 1)),
    tree_range3(OLS),
    eval(in_interval(MIN, 0, N)),
    eval(open_minimum(MIN, OLS)),
    eval(in_interval(MAX, 0, N)),
    eval(maximum(MAX,DISTS1)),
    eval(scalar_product([[coeff-1,var-MAX],[coeff-(-1),var-MIN]], =, R)).

tree_range0(NODES, SNODES) :-
    tree_range0a(NODES, L),
    sort(L, S),
    tree_range0a(SNODES, S), !.

tree_range0a([], []).
tree_range0a([[index-I,succ-S]|R], [I-S|T]) :-
    tree_range0a(R, T).

tree_range1([], [], [], [], [], [], [], [], []).
tree_range1([I|RI], [S|RS], [[var-V]|RV1], [[value-V]|RV2],
            [[var-O]|RO], [[val-I,noccurrence-O]|RIO],
            [[var-S]|RSS], [[var-L]|RL], [[var-O,bool-L]|ROL]) :-
    tree_range1(RI, RS, RV1, RV2, RO, RIO, RSS, RL, ROL).

tree_range2([], [], _, _, _).
tree_range2([_IND|RIND], [SUCC|RSUCC], N, DISTS_BEFORE, DISTS_AFTER) :-
    append(DISTS_BEFORE, [[value-0]], TD),
    DISTS_AFTER = [[value-D]|RDISTS_AFTER],
    append(TD, RDISTS_AFTER, TABLE),
    eval(in_interval(DS, 0, N)),
    eval(element(SUCC, TABLE, DS)),    
    eval(scalar_product([[coeff-1,var-D],[coeff-(-1),var-DS]], =, 1)),
    append(DISTS_BEFORE, [[value-D]], DISTS_BEFORE1),
    tree_range2(RIND, RSUCC, N, DISTS_BEFORE1, RDISTS_AFTER).

tree_range3([]).
tree_range3([[var-O,bool-L]|ROL]) :-
    L #<=> O #> 0,
    tree_range3(ROL).
