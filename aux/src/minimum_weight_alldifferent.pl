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

ctr_date(minimum_weight_alldifferent,['20030820','20040530','20060812']).

ctr_origin(minimum_weight_alldifferent, '\\cite{FocacciLodiMilano99}', []).

ctr_arguments(minimum_weight_alldifferent,
              ['VARIABLES'-collection(var-dvar)        ,
               'MATRIX'-collection(i-int, j-int, c-int),
               'COST'-dvar                             ]).

ctr_synonyms(minimum_weight_alldifferent,[minimum_weight_alldiff    ,
                                          minimum_weight_alldistinct,
                                          min_weight_alldiff        ,
                                          min_weight_alldifferent   ,
                                          min_weight_alldistinct    ]).

ctr_restrictions(minimum_weight_alldifferent,
                 [size('VARIABLES') > 0                                 ,
                  required('VARIABLES',var)                             ,
                  'VARIABLES'^var >= 1                                  ,
                  'VARIABLES'^var =< size('VARIABLES')                  ,
                  required('MATRIX',[i,j,c])                            ,
                  increasing_seq('MATRIX',[i,j])                        ,
                  'MATRIX'^i >= 1                                       ,
                  'MATRIX'^i =< size('VARIABLES')                       ,
                  'MATRIX'^j >= 1                                       ,
                  'MATRIX'^j =< size('VARIABLES')                       ,
                  size('MATRIX') = size('VARIABLES') * size('VARIABLES')]).

ctr_typical(minimum_weight_alldifferent,
            [size('VARIABLES') > 1,
             range('MATRIX'^c) > 1,
             'MATRIX'^c        > 0]).

ctr_functional_dependency(minimum_weight_alldifferent, 3, [1,2]).

ctr_graph(minimum_weight_alldifferent,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^key],
          ['NTREE' = 0,
           'SUM_WEIGHT_ARC'('MATRIX'@((variables1^key-1)*size('VARIABLES')+variables1^var)^c) = 'COST'],
          []).

ctr_example(minimum_weight_alldifferent,
            minimum_weight_alldifferent([[var-2],[var-3],[var-1],[var-4]],
                                         [[i-1 ,j-1, c-4],
                                          [i-1 ,j-2, c-1],
                                          [i-1 ,j-3, c-7],
                                          [i-1 ,j-4, c-0],
                                          [i-2 ,j-1, c-1],
                                          [i-2 ,j-2, c-0],
                                          [i-2 ,j-3, c-8],
                                          [i-2 ,j-4, c-2],
                                          [i-3 ,j-1, c-3],
                                          [i-3 ,j-2, c-2],
                                          [i-3 ,j-3, c-1],
                                          [i-3 ,j-4, c-6],
                                          [i-4 ,j-1, c-0],
                                          [i-4 ,j-2, c-0],
                                          [i-4 ,j-3, c-6],
                                          [i-4 ,j-4, c-5]],
                                          17)).

ctr_draw_example(minimum_weight_alldifferent,
                 ['VARIABLES'],
                 [[[var-2],[var-3],[var-1],[var-4]]],
                 ['CLIQUE'],
                 [1-2,2-3,3-1,4-4],
                 ['NARC','SUM_WEIGHT_ARC'([1-[1-2],8-[2-3],3-[3-1],5-[4-4]])],
                 '','NARC=4\\nSUM_WEIGHT_ARC=1+8+3+5=17',
                 [2.145,2.2,2.5,2.145]).

ctr_see_also(minimum_weight_alldifferent,
 [link('attached to cost variant', alldifferent,                      '',      []),
  link('common keyword',           sum_of_weights_of_distinct_values, '%k',    ['weighted assignment']),
  link('common keyword',           global_cardinality_with_costs,     '%k,%k', ['cost filtering constraint', 'weighted assignment']),
  link('common keyword',           weighted_partial_alldiff,          '%k,%k', ['cost filtering constraint', 'weighted assignment'])]).

ctr_key_words(minimum_weight_alldifferent,['core'                                       ,
                                           'cost filtering constraint'                  ,
                                           'assignment'                                 ,
                                           'cost matrix'                                ,
                                           'weighted assignment'                        ,
                                           'one\\_succ'                                 ,
                                           'Hungarian method for the assignment problem',
                                           'functional dependency'                      ]).

ctr_persons(minimum_weight_alldifferent,['Focacci F.' ,
                                         'Lodi A.'    ,
                                         'Milano M.'  ,
                                         'Kuhn H. W.' ,
                                         'Sellmann M.']).
