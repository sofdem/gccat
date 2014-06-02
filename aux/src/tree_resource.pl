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

ctr_date(tree_resource,['20030820','20060819']).

ctr_origin(tree_resource, 'Derived from %c.', [tree]).

ctr_arguments(tree_resource,
              ['RESOURCE'-collection(id-int, nb_task-dvar               ),
                   'TASK'-collection(id-int, father-dvar , resource-dvar)]).

ctr_exchangeable(tree_resource,
                 [items('RESOURCE',all),
                  items('TASK',all)]).

ctr_restrictions(tree_resource,
                 [size('RESOURCE') > 0                          ,
                  required('RESOURCE',[id,nb_task])             ,
                  'RESOURCE'^id >= 1                            ,
                  'RESOURCE'^id =< size('RESOURCE')             ,
                  distinct('RESOURCE',id)                       ,
                  'RESOURCE'^nb_task >= 0                       ,
                  'RESOURCE'^nb_task =< size('TASK')            ,
                  required('TASK',[id,father,resource])         ,
                  'TASK'^id > size('RESOURCE')                  ,
                  'TASK'^id =< size('RESOURCE')+size('TASK')    ,
                  distinct('TASK',id)                           ,
                  'TASK'^father >= 1                            ,
                  'TASK'^father =< size('RESOURCE')+size('TASK'),
                  'TASK'^resource >= 1                          ,
                  'TASK'^resource =< size('RESOURCE')           ]).

ctr_typical(tree_resource,
            [size('RESOURCE') > 0               ,
             size('TASK')     > size('RESOURCE')]).

ctr_derived_collections(tree_resource,
    [col('RESOURCE_TASK'-collection(index-int, succ-dvar, name-dvar),
         [item(index-'RESOURCE'^id, succ-'RESOURCE'^id, name-'RESOURCE'^id  ),
          item(index-'TASK'^id    , succ-'TASK'^father, name-'TASK'^resource)])
    ]).

ctr_graph(tree_resource,
          ['RESOURCE_TASK'],
          2,
          ['CLIQUE'>>collection(resource_task1,resource_task2)],
          [resource_task1^succ = resource_task2^index,
           resource_task1^name = resource_task2^name ],
          ['MAX_NSCC' =< 1                            ,
           'NCC'      =  size('RESOURCE')             ,
           'NVERTEX'  =  size('RESOURCE')+size('TASK')],
          []).

ctr_graph(tree_resource,
          ['RESOURCE_TASK'],
          2,
          foreach('RESOURCE',['CLIQUE'>>collection(resource_task1,resource_task2)]),
          [resource_task1^succ = resource_task2^index,
           resource_task1^name = resource_task2^name ,
           resource_task1^name = 'RESOURCE'^id       ],
          ['NVERTEX' = 'RESOURCE'^nb_task+1],
          []).

ctr_example(tree_resource,
            tree_resource([[id-1, nb_task-4],
                           [id-2, nb_task-0],
                           [id-3, nb_task-1]],
                          [[id-4,  father-8, resource-1],
                           [id-5,  father-3, resource-3],
                           [id-6,  father-8, resource-1],
                           [id-7,  father-1, resource-1],
                           [id-8,  father-1, resource-1]])).

ctr_draw_example(tree_resource,
                 ['RESOURCE_TASK'],
                 [[[index-1, succ-1, name-1],
                   [index-2, succ-2, name-2],
                   [index-3, succ-3, name-3],
                   [index-4, succ-8, name-1],
                   [index-5, succ-3, name-3],
                   [index-6, succ-8, name-1],
                   [index-7, succ-1, name-1],
                   [index-8, succ-1, name-1]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3,4-8,5-3,6-8,7-1,8-1],
                 ['NVERTEX',
                  'FOREACH'('RESOURCE',[1-[1,4,6,7,8],2-[2],3-[3,5]])],
                 '','1:NVERTEX=5\\n2:NVERTEX=1\\n3:NVERTEX=2',
                 [2,3.94,3.1,3.5]).

ctr_see_also(tree_resource,
 [link('root concept',          tree,               '', []),
  link('used in reformulation', element,            '', []),
  link('used in reformulation', global_cardinality, '', []),
  link('used in reformulation', tree,               '', [])]).

ctr_key_words(tree_resource,['graph constraint'             ,
                             'tree'                         ,
                             'resource constraint'          ,
                             'graph partitioning constraint',
                             'connected component'          ,
                             'derived collection'           ]).

ctr_application(tree_resource, [2]).

ctr_eval(tree_resource, [reformulation(tree_resource_r)]).

tree_resource_r(RESOURCE, TASK) :-
    length(RESOURCE, R),
    length(TASK, T),
    R > 0,
    collection(RESOURCE, [int(1,R),dvar(0,T)]),
    get_attr1(RESOURCE, RIDS),
    get_attr2(RESOURCE, RNBTASKS),
    all_different(RIDS),
    R1 is R+1,
    RT is R+T,
    collection(TASK, [int(R1,RT),dvar(1,RT),dvar(1,R)]),
    get_attr1(TASK, TIDS),
    get_attr2(TASK, TFATHERS),
    get_attr3(TASK, TRESOURCES),
    all_different(TIDS),
    tree_resource1(RIDS, CNODES1),
    tree_resource2(TIDS, TFATHERS, CNODES2),
    append(CNODES1, CNODES2, NODES),
    eval(tree(R, NODES)),
    tree_resource3(TIDS, TRESOURCES, TIR),
    sort(TIR, STIR),
    tree_resource4(1, R, INC),
    append(INC, STIR, TAB),
    tree_resource5(TAB, TABR),
    tree_resource6(TFATHERS, TRESOURCES, TABR),
    tree_resource7(STIR, GCVARS),
    tree_resource8(RIDS, RNBTASKS, GCVALS),
    eval(global_cardinality(GCVARS, GCVALS)).

tree_resource1([], []).
tree_resource1([I|R], [[index-I,succ-I]|S]) :-
    tree_resource1(R, S).

tree_resource2([], [], []).
tree_resource2([I|R], [F|S], [[index-I,succ-F]|T]) :-
    tree_resource2(R, S, T).

tree_resource3([], [], []).
tree_resource3([I|RI], [R|RR], [I-R|S]) :-
    tree_resource3(RI, RR, S).

tree_resource4(I, R, []) :-
    I > R, !.
tree_resource4(I, R, [I-I|S]) :-
    I =< R,
    I1 is I+1,
    tree_resource4(I1, R, S).

tree_resource5([], []).
tree_resource5([_-R|S], [[value-R]|T]) :-
    tree_resource5(S, T).

tree_resource6([], [], _).
tree_resource6([Fi|RF], [Ri|RR], TABR) :-
    eval(element(Fi, TABR, Ri)),
    tree_resource6(RF, RR, TABR).

tree_resource7([], []).
tree_resource7([_-V|R], [[var-V]|S]) :-
    tree_resource7(R, S).

tree_resource8([], [], []).
tree_resource8([V|R], [O|S], [[val-V,noccurrence-O]|T]) :-
    tree_resource8(R, S, T).
