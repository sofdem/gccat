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

ctr_date(sum,['20030820','20040726','20060817']).

ctr_origin(sum, '\\cite{Yunes02}.', []).

ctr_arguments(sum,
              ['INDEX'-dvar                        ,
               'SETS'-collection(ind-int, set-sint),
               'CONSTANTS'-collection(cst-int)     ,
               'S'-dvar                            ]).

ctr_exchangeable(sum,
                 [items('SETS',all)]).

ctr_synonyms(sum,[sum_pred]).

ctr_restrictions(sum,
                 [
                  size('SETS') >= 1         ,
                  required('SETS',[ind,set]),
                  distinct('SETS',ind)      ,
                  size('CONSTANTS') >= 1    ,
                  required('CONSTANTS',cst) ]).

ctr_typical(sum,
            [size('SETS')           > 1           ,
             size('CONSTANTS')      > size('SETS'),
             range('CONSTANTS'^cst) > 1           ]).

ctr_functional_dependency(sum, 4, [1,2,3]).

ctr_graph(sum,
          ['SETS','CONSTANTS'],
          2,
          ['PRODUCT'>>collection(sets,constants)],
          ['INDEX' = sets^ind            ,
           in_set(constants^key,sets^set)],
          ['SUM'('CONSTANTS',cst) = 'S'],
          []).

ctr_example(sum,
            sum(8,
                [[ind-8, set-{2,3}  ],
                 [ind-1, set-{3}    ],
                 [ind-3, set-{1,4,5}],
                 [ind-6, set-{2,4}  ]],
                [[cst-4],[cst-9],[cst-1],[cst-3],[cst-1]],
                10)).

ctr_draw_example(sum,
                 ['SETS','CONSTANTS'],
                 [[[ind-8, set-{2,3}  ],
                   [ind-1, set-{3}    ],
                   [ind-3, set-{1,4,5}],
                   [ind-6, set-{2,4}  ]],
                  [[cst-4],
                   [cst-9],
                   [cst-1],
                   [cst-3],
                   [cst-1]]],
                 ['PRODUCT'],
                 [1-[2,3]],
                 ['SUM'([6,7])],
                  '','SUM=9+1=10',
                 [2.6,1.9,1.3,1.3]).

ctr_see_also(sum,
 [link('common keyword',            element, '%k', ['data constraint']),
  link('common keyword',            sum_ctr, '%k', ['sum']),
  link('common keyword',            sum_set, '%k', ['sum']),
  link('used in graph description', in_set,  '',   [])]).

ctr_key_words(sum,['data constraint'       ,
                   'linear programming'    ,
                   'convex hull relaxation',
                   'sum'                   ,
                   'functional dependency' ]).

ctr_persons(sum,['Tallys H. Yunes']).
