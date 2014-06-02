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

ctr_date(global_cardinality_with_costs,['20030820','20040530','20060809','20090425']).

ctr_origin(global_cardinality_with_costs, '\\cite{Regin99a}', []).

ctr_synonyms(global_cardinality_with_costs, [gccc,cost_gcc]).

ctr_arguments(global_cardinality_with_costs,
              ['VARIABLES'-collection(var-dvar)              ,
               'VALUES'-collection(val-int, noccurrence-dvar),
               'MATRIX'-collection(i-int, j-int, c-int)      ,
               'COST'-dvar                                   ]).

ctr_restrictions(global_cardinality_with_costs,
                 [required('VARIABLES',var)                          ,
                  size('VALUES') > 0                                 ,
                  required('VALUES',[val,noccurrence])               ,
                  distinct('VALUES',val)                             ,
                  'VALUES'^noccurrence >= 0                          ,
                  'VALUES'^noccurrence =< size('VARIABLES')          ,
                  required('MATRIX',[i,j,c])                         ,
                  increasing_seq('MATRIX',[i,j])                     ,
                  'MATRIX'^i >= 1                                    ,
                  'MATRIX'^i =< size('VARIABLES')                    ,
                  'MATRIX'^j >= 1                                    ,
                  'MATRIX'^j =< size('VALUES')                       ,
                  size('MATRIX') = size('VARIABLES') * size('VALUES')]).

ctr_typical(global_cardinality_with_costs,
            [size('VARIABLES')           > 1             ,
             range('VARIABLES'^var)      > 1             ,
             size('VALUES')              > 1             ,
             range('VALUES'^noccurrence) > 1             ,
             range('MATRIX'^c)           > 1             ,
             size('VARIABLES')           > size('VALUES')]).

ctr_pure_functional_dependency(global_cardinality_with_costs, []).
ctr_functional_dependency(global_cardinality_with_costs, 2-2, [1]).
ctr_functional_dependency(global_cardinality_with_costs, 4, [1,2,3]).

ctr_graph(global_cardinality_with_costs,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var = 'VALUES'^val],
          ['NVERTEX' = 'VALUES'^noccurrence],
          []).

ctr_graph(global_cardinality_with_costs,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var = values^val],
          ['SUM_WEIGHT_ARC'('MATRIX'@((variables^key-1)*size('VALUES')+values^key)^c) = 'COST'],
          []).

ctr_example(global_cardinality_with_costs,
            global_cardinality_with_costs([[var-3],[var-3],[var-3],[var-6]],
                                          [[val-3, noccurrence-3],
                                           [val-5, noccurrence-0],
                                           [val-6, noccurrence-1]],
                                           [[i-1 ,j-1, c-4],
                                            [i-1 ,j-2, c-1],
                                            [i-1 ,j-3, c-7],
                                            [i-2 ,j-1, c-1],
                                            [i-2 ,j-2, c-0],
                                            [i-2 ,j-3, c-8],
                                            [i-3 ,j-1, c-3],
                                            [i-3 ,j-2, c-2],
                                            [i-3 ,j-3, c-1],
                                            [i-4 ,j-1, c-0],
                                            [i-4 ,j-2, c-0],
                                            [i-4 ,j-3, c-6]],
                                            14)).

ctr_draw_example(global_cardinality_with_costs,
                 ['VARIABLES','VALUES'],
                 [[[var-3],[var-3],[var-3],[var-6]],
                  [[val-3, noccurrence-3],
                   [val-5, noccurrence-0],
                   [val-6, noccurrence-1]]],
                 ['PRODUCT'],
                 [1-1,2-1,3-1,4-3],
                 ['SUM_WEIGHT_ARC'([4-[1-5],1-[2-5],3-[3-5],6-[4-7]])],
                 '','SUM_WEIGHT_ARC=4+1+3+6=14',
                 [2.145,2.145,2.4,2.145]).

ctr_see_also(global_cardinality_with_costs,
 [link('attached to cost variant', global_cardinality,                '%e associated with each %e,%e pair removed', [cost,variable,value]),
  link('implies',                  global_cardinality,                '',                                           []),
  link('common keyword',           minimum_weight_alldifferent,       '%k,%k',                                      ['cost filtering constraint','weighted assignment']),
  link('common keyword',           sum_of_weights_of_distinct_values, '%k',                                         ['weighted assignment']),
  link('common keyword',           weighted_partial_alldiff,          '%k',                                         ['weighted assignment'])]).

ctr_key_words(global_cardinality_with_costs,['cost filtering constraint'                 ,
                                             'assignment'                                ,
                                             'cost matrix'                               ,
                                             'weighted assignment'                       ,
                                             'scalar product'                            ,
                                             'magic square'                              ,
                                             'magic hexagon'                             ,
					     'pure functional dependency'                ,
                                             'functional dependency'                     ,
                                             'regret based heuristics'                   ,
                                             'regret based heuristics in matrix problems']).

ctr_persons(global_cardinality_with_costs,['R\\\'egin J.-C.']).

ctr_eval(global_cardinality_with_costs, [reformulation(global_cardinality_with_costs_r)]).

global_cardinality_with_costs_r(VARIABLES, VALUES, MATRIX, COST) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    collection(VALUES, [int,dvar(0,N)]),
    length(VALUES, M),
    M > 0,
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES   , VALS),
    all_different(VALS),
    collection(MATRIX, [int(1,N),int(1,M),int]),
    collection_increasing_seq(MATRIX,[1,2]),
    eval(global_cardinality(VARIABLES, VALUES)),
    get_attr3(MATRIX, CS),
    global_cardinality_with_costs1(VARS, VALS, M, CS, TERM),
    call(TERM #= COST).

global_cardinality_with_costs1([], _, _, _, 0).
global_cardinality_with_costs1([VAR|R], VALS, M, CMAT, C+S) :-
    global_cardinality_with_costs2(M, CMAT, ELEMTABLE, RESTCMAT),
    element(IVAL, VALS, VAR),
    element(IVAL, ELEMTABLE, C),
    global_cardinality_with_costs1(R, VALS, M, RESTCMAT, S).

global_cardinality_with_costs2(0, CMAT, [], CMAT) :- !.
global_cardinality_with_costs2(I, [C|R], [C|S], T) :-
    I > 0,
    I1 is I-1,
    global_cardinality_with_costs2(I1, R, S, T).
