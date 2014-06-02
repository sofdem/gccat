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

ctr_date(cycle,['20000128','20030820','20060807','20111223']).

ctr_origin(cycle, '\\cite{BeldiceanuContejean94}', []).

ctr_arguments(cycle,
              ['NCYCLE'-dvar                           ,
               'NODES'-collection(index-int, succ-dvar)]).

ctr_exchangeable(cycle,
                 [items('NODES',all),
		  attrs_sync('NODES',[[index,succ]])]).

ctr_restrictions(cycle,
                 ['NCYCLE'      >= 1            ,
                  'NCYCLE'      =< size('NODES'),
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(cycle,
            ['NCYCLE'      < size('NODES'),
             size('NODES') > 2            ]).

ctr_functional_dependency(cycle, 1, [2]).

ctr_graph(cycle,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ = nodes2^index],
          ['NTREE' = 0       ,
           'NCC'   = 'NCYCLE'],
          ['ONE_SUCC']).

ctr_example(cycle,
            [cycle(2,[[index-1, succ-2],[index-2, succ-1],[index-3, succ-5],[index-4, succ-3],[index-5, succ-4]]),
	     cycle(1,[[index-1, succ-2],[index-2, succ-5],[index-3, succ-1],[index-4, succ-3],[index-5, succ-4]]),
	     cycle(5,[[index-1, succ-1],[index-2, succ-2],[index-3, succ-3],[index-4, succ-4],[index-5, succ-5]])]).

ctr_draw_example(cycle,
                 ['NODES'],
                 [[[index-1, succ-2],
                   [index-2, succ-1],
                   [index-3, succ-5],
                   [index-4, succ-3],
                   [index-5, succ-4]]],
                 ['CLIQUE'],
                 [1-2,2-1,3-5,4-3,5-4],
                 ['NCC'([[1,2],[3,4,5]])],
                 '','NTREE=0,NCC=2',
                 [2.145,2.145,2.145,2.145]).

ctr_cond_imply(cycle, balance_cycle, ['NCYCLE' = 1], ['BALANCE' = 0], [none,'NODES']         ).
ctr_cond_imply(cycle, permutation,   [],             [],              [index_to_col('NODES')]).

ctr_see_also(cycle,
 [link(specialisation,                  circuit,                '%e set to %e', ['NCYCLE',1]),
  link(implies,                         alldifferent,           '',             []),
  link('implies (items to collection)', atleast_nvector,        '',             []),
  link('common keyword',                cycle_or_accessibility, '%k',           ['graph constraint'], '\\\\ '),
  link('common keyword',                circuit_cluster,        '%k, %k',       ['graph constraint', 'one\\_succ'], '\\\\ '),
  link('common keyword',                alldifferent,           '%k',           [permutation], '\\\\ '),
  link('common keyword',                derangement,            '%k',           [permutation], '\\\\ '),
  link('common keyword',                inverse,                '%k',           [permutation], '\\\\ '),
  link('common keyword',                symmetric_alldifferent, '%k',           [permutation], '\\\\ '),
  link('common keyword',                tour,                   '%k',           ['graph constraint'], '\\\\ '),
  link('common keyword',                tree,                   '%k',           ['graph partitioning constraint']),
  link('common keyword',                map,                    '%k',           ['graph partitioning constraint'], '\\\\ '),
  link('common keyword',                cycle_resource,         '%k',           ['graph partitioning constraint'], '\\\\ '),
  link('common keyword',                cycle_card_on_path,     '%k,%k',        [permutation, 'graph partitioning constraint'], '\\\\ '),
  link('common keyword',                graph_crossing,         '%k,%k',        ['graph constraint', 'graph partitioning constraint'], '\\\\ '),
  link('used in reformulation',         alldifferent,           '',             []),
  link('used in reformulation',         element,                '',             []),
  link('used in reformulation',         minimum,                '',             []),
  link('used in reformulation',         nvalue,                 '',             []),
  link('related',                       balance_cycle,          'counting number of cycles versus controlling how balanced the cycles are', [])]).

ctr_key_words(cycle,['core'                         ,
                     'graph constraint'             ,
                     'circuit'                      ,
                     'cycle'                        ,
                     'permutation'                  ,
                     'graph partitioning constraint',
                     'business rules'               ,
                     'connected component'          ,
                     'strongly connected component' ,
                     'strong bridge'                ,
                     'Euler knight'                 ,
                     'pick-up delivery'             ,
                     'one\\_succ'                   ,
                     'DFS-bottleneck'               ,
                     'functional dependency'        ]).

ctr_persons(cycle,['Beldiceanu N.'    ,
                   'Contejean E.'     ,
                   'Bourreau \\\'E.'  ,
                   'Lauri\\`ere J.-L.',
                   'Sloane N. J. A.'  ]).

ctr_application(cycle, [2]).

ctr_eval(cycle, [checker(cycle_c),
		 reformulation(cycle_r)]).

ctr_sol(cycle,2,0,2,2,[1-1,2-1]).
ctr_sol(cycle,3,0,3,6,[1-2,2-3,3-1]).
ctr_sol(cycle,4,0,4,24,[1-6,2-11,3-6,4-1]).
ctr_sol(cycle,5,0,5,120,[1-24,2-50,3-35,4-10,5-1]).
ctr_sol(cycle,6,0,6,720,[1-120,2-274,3-225,4-85,5-15,6-1]).
ctr_sol(cycle,7,0,7,5040,[1-720,2-1764,3-1624,4-735,5-175,6-21,7-1]).
ctr_sol(cycle,8,0,8,40320,[1-5040,2-13068,3-13132,4-6769,5-1960,6-322,7-28,8-1]).
ctr_sol(cycle,9,0,9,362880,[1-40320,2-109584,3-118124,4-67284,5-22449,6-4536,7-546,8-36,9-1]).
ctr_sol(cycle,10,0,10,3628800,[1-362880,2-1026576,3-1172700,4-723680,5-269325,6-63273,7-9450,8-870,9-45,10-1]).

cycle_c(NCYCLE, NODES):-
        length(NODES, N),
        check_type(dvar(1,N), NCYCLE),
        collection(NODES, [int(1,N),dvar(1,N)]),
        get_attr1(NODES, IND),
	sort(IND, Js),
        length(Js, N),
	get_attr12(NODES, IND_SUCC),
	keysort(IND_SUCC, SIND_SUCC),
	remove_key_from_collection(SIND_SUCC, SUCCS),
	length(Term, N),
	list_to_tree(Term, Tree),
        (   for(J,1,N),
	    foreach(X,SUCCS),
	    foreach(Free,Term),
	    foreach(J,Js),
	    param(Tree)
	do  get_label(X, Tree, Free)
        ),
	sort(SUCCS, Js),
	sort(Term, Cs),
	length(Cs, NCYCLE).

cycle_r(NCYCLE, NODES):-
        length(NODES, N),
        check_type(dvar(1,N), NCYCLE),
        collection(NODES, [int(1,N),dvar(1,N)]),
	get_attr1(NODES, IND),
	sort(IND, SIND),
	length(SIND, N),
	get_attr12(NODES, IND_SUCC),
	keysort(IND_SUCC, SIND_SUCC),
	remove_key_from_collection(SIND_SUCC, Succ),
	all_distinct(Succ),
	(   for(I,1,N),
	    foreach(Min,Mins),
	    param(Succ,N)
	do  length([I|Ss], N),
	    minimum(Min, [I|Ss]),
	    (   foreach(S2,Ss),
		fromto(I,S1,S2,_),
		param(Succ)
	    do  element(S1, Succ, S2)
	    )
	),
	nvalue(NCYCLE, Mins).
