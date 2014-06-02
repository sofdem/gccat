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

ctr_date(path,['20090101','20120219']).

ctr_origin(path, 'Derived from %c.', [binary_tree]).

ctr_arguments(path,
              ['NPATH'-dvar                            ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_restrictions(path,
                 ['NPATH'       >= 1            ,
                  'NPATH'       =< size('NODES'),
                  required('NODES',[index,succ]),
                  size('NODES') >  0            ,
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(path,
            ['NPATH'       < size('NODES'),
             size('NODES') > 1            ]).

ctr_functional_dependency(path, 1, [2]).

ctr_exchangeable(path,
                 [items('NODES',all)]).

ctr_graph(path,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['MAX_NSCC'      =< 1       ,
           'NCC'           =  'NPATH' ,
           'MAX_ID'        =< 1       ],
          ['ONE_SUCC']).

ctr_example(path,
            [path(3,[[index-1, succ-1], [index-2, succ-3], [index-3, succ-5], [index-4, succ-7],
                     [index-5, succ-1], [index-6, succ-6], [index-7, succ-7], [index-8, succ-6]]),
             path(1,[[index-1, succ-8], [index-2, succ-7], [index-3, succ-6], [index-4, succ-5],
                     [index-5, succ-5], [index-6, succ-4], [index-7, succ-3], [index-8, succ-2]]),
             path(8,[[index-1, succ-1], [index-2, succ-2], [index-3, succ-3], [index-4, succ-4],
                     [index-5, succ-5], [index-6, succ-6], [index-7, succ-7], [index-8, succ-8]])
	    ]).

ctr_draw_example(path,
                 ['NODES'],
                 [[[index-1, succ-1],
                   [index-2, succ-3],
                   [index-3, succ-5],
                   [index-4, succ-7],
                   [index-5, succ-1],
                   [index-6, succ-6],
                   [index-7, succ-7],
                   [index-8, succ-6]]],
                 ['CLIQUE'],
                 [1-1,2-3,3-5,4-7,5-1,6-6,7-7,8-6],
                 ['NCC'([[1,2,3,5],[4,7],[6,8]]), 'MAX_ID'([1])],
                 '','MAX_NSCC=1, NCC=3\\nMAX_ID=1',
                 [2.5,4,2.5,2.5]).

ctr_see_also(path,
 [link('implies',        binary_tree,      '',                                                                                                    []),
  link('generalisation', binary_tree,      'at most one child replaced by at most two children',                                                  []),
  link('generalisation', tree,             'at most one child replaced by no limit on the number of children',                                    []),
  link('generalisation', temporal_path,    'vertices are located in time, and to each arc corresponds a precedence constraint',                   []),
  link('common keyword', circuit,          '%k, %k',                                                                                              ['graph partitioning constraint', 'one\\_succ']),
  link('common keyword', proper_circuit,   '%k, %k',                                                                                              ['graph partitioning constraint', 'one\\_succ']),
  link('common keyword', dom_reachability, '%k',                                                                                                  ['path']),
  link('common keyword', path_from_to,     '%k, select an induced subgraph so that there is a path from a given vertex to an other given vertex', ['path']),
  link('related',        balance_path,     'counting number of paths versus controlling how balanced the paths are',                              [])]).

ctr_key_words(path,['graph constraint'             ,
                    'graph partitioning constraint',
                    'connected component'          ,
                    'path'                         ,
                    'tree'                         ,
                    'one\\_succ'                   ,
                    'DFS-bottleneck'               ,
                    'functional dependency'        ]).

ctr_application(path, [2]).

ctr_eval(path, [reformulation(path_r),checker(path_c)]).

ctr_sol(path,2,0,2,3,[1-2,2-1]).
ctr_sol(path,3,0,3,13,[1-6,2-6,3-1]).
ctr_sol(path,4,0,4,73,[1-24,2-36,3-12,4-1]).
ctr_sol(path,5,0,5,501,[1-120,2-240,3-120,4-20,5-1]).
ctr_sol(path,6,0,6,4051,[1-720,2-1800,3-1200,4-300,5-30,6-1]).
ctr_sol(path,7,0,7,37633,[1-5040,2-15120,3-12600,4-4200,5-630,6-42,7-1]).
ctr_sol(path,8,0,8,394353,[1-40320,2-141120,3-141120,4-58800,5-11760,6-1176,7-56,8-1]).

path_r(NPATH, NODES) :-
    eval(tree(NPATH, NODES)),
    get_attr1(NODES, INDEXES),
    get_attr2(NODES, SUCCS),
    k_ary_tree(INDEXES, INDEXES, SUCCS, 1).

path_c(NPATH, NODES) :-
    length(NODES, N),
    check_type(dvar(1,N), NPATH),
    collection(NODES, [int(1,N),dvar(1,N)]),
    get_attr1(NODES, IND),
    sort(IND, SIND),
    length(SIND, N),
    get_attr12(NODES, IND_SUCC),
    keysort(IND_SUCC, SIND_SUCC),
    remove_key_from_collection(SIND_SUCC, Succ),	
   (   foreach(S,Succ),
	foreach(S-P,L1),
	foreach(_,Sink),	% each element will get its sink
	count(P,1,_)
   do  true
   ),
   keysort(L1, L2),
   keyclumped(L2, L3),
   % L3 is a list of pairs (successor-predecessors)
   (   foreach(Su-Ps,L3),
	param(Sink)
   do  (   Ps = [Su]
	->  nth1(Su, Sink, Su)
	;   Ps = [Other]
	->  nth1(Su, Sink, X),
	    nth1(Other, Sink, X)
	;   Ps = [Su,Other]
	->  nth1(Su, Sink, Su),
	    nth1(Other, Sink, Su)
	;   Ps = [Other,Su]
	->  nth1(Su, Sink, Su),
	    nth1(Other, Sink, Su)
	)
   ),
   ground(Sink),
   sort(Sink, Sinks),
   length(Sinks, NPATH).
