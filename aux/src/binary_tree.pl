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

ctr_date(binary_tree,['20000128','20030820','20060804']).

ctr_origin(binary_tree, 'Derived from %c.', [tree]).

ctr_arguments(binary_tree,
              ['NTREES'-dvar                            ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(binary_tree,
                 [items('NODES',all)]).

ctr_restrictions(binary_tree,
                 ['NTREES'      >= 0            ,
                  'NTREES'      =< size('NODES'),
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(binary_tree,
            ['NTREES'      > 0            ,
             'NTREES'      < size('NODES'),
             size('NODES') > 2            ]).

ctr_functional_dependency(binary_tree, 1, [2]).

ctr_graph(binary_tree,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['MAX_NSCC'      =< 1       ,
           'NCC'           =  'NTREES',
           'MAX_ID'        =< 2       ],
          ['ONE_SUCC']).

ctr_example(binary_tree,
            [binary_tree(2,[[index-1, succ-1],[index-2, succ-3],[index-3, succ-5],[index-4, succ-7],
                            [index-5, succ-1],[index-6, succ-1],[index-7, succ-7],[index-8, succ-5]]),
	     binary_tree(8,[[index-1, succ-1],[index-2, succ-2],[index-3, succ-3],[index-4, succ-4],
                            [index-5, succ-5],[index-6, succ-6],[index-7, succ-7],[index-8, succ-8]]),
	     binary_tree(7,[[index-1, succ-8],[index-2, succ-2],[index-3, succ-3],[index-4, succ-4],
                            [index-5, succ-5],[index-6, succ-6],[index-7, succ-7],[index-8, succ-8]])]).

ctr_draw_example(binary_tree,
                 ['NODES'],
                 [[[index-1, succ-1],
                   [index-2, succ-3],
                   [index-3, succ-5],
                   [index-4, succ-7],
                   [index-5, succ-1],
                   [index-6, succ-1],
                   [index-7, succ-7],
                   [index-8, succ-5]]],
                 ['CLIQUE'],
                 [1-1,2-3,3-5,4-7,5-1,6-1,7-7,8-5],
                 ['NCC'([[1,2,3,5,6,8],[4,7]]), 'MAX_ID'([5])],
                 '','MAX_NSCC=1, NCC=2\\nMAX_ID=2',
                 [2.5,4,2.5,2.5]).

ctr_see_also(binary_tree,
 [link('implies',                       tree,            '',                                                                     []),
  link('implies (items to collection)', atleast_nvector, '',                                                                     []),
  link('implied by',                    path,            '',                                                                     []),
  link(generalisation,                  tree, 'at most two childrens replaced by no restriction on maximum number of childrens', []),
  link(specialisation,                  path, 'at most two childrens replaced by at most one child',                             [])]).

ctr_key_words(binary_tree,['graph constraint'             ,
                           'graph partitioning constraint',
                           'connected component'          ,
                           'tree'                         ,
                           'one\\_succ'                   ,
                           'functional dependency'        ]).

ctr_application(binary_tree, [2]).

ctr_eval(binary_tree, [reformulation(binary_tree_r)]).

ctr_sol(binary_tree,2,0,2,3,[1-2,2-1]).
ctr_sol(binary_tree,3,0,3,16,[1-9,2-6,3-1]).
ctr_sol(binary_tree,4,0,4,121,[1-60,2-48,3-12,4-1]).
ctr_sol(binary_tree,5,0,5,1191,[1-540,2-480,3-150,4-20,5-1]).
ctr_sol(binary_tree,6,0,6,14461,[1-6120,2-5850,3-2100,4-360,5-30,6-1]).
ctr_sol(binary_tree,7,0,7,209098,[1-83790,2-84420,3-33390,4-6720,5-735,6-42,7-1]).
ctr_sol(binary_tree,8,0,8,3510921,[1-1345680,2-1411200,3-599760,4-135240,5-17640,6-1344,7-56,8-1]).

binary_tree_r(NTREES, NODES) :-
    eval(tree(NTREES, NODES)),
    get_attr1(NODES, INDEXES),
    get_attr2(NODES, SUCCS),
    k_ary_tree(INDEXES, INDEXES, SUCCS, 2).
