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

ctr_date(nvalue_on_intersection,['20040530','20060812']).

ctr_origin(nvalue_on_intersection, 'Derived from %c and %c.', [common,nvalue]).

ctr_arguments(nvalue_on_intersection,
              ['NVAL'-dvar                      ,
               'VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(nvalue_on_intersection,
                 [args([['NVAL'],['VARIABLES1','VARIABLES2']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_restrictions(nvalue_on_intersection,
                 [required('VARIABLES1',var)       ,
                  required('VARIABLES2',var)       ,
                  'NVAL' >= 0                      ,
                  'NVAL' =< size('VARIABLES1')     ,
                  'NVAL' =< size('VARIABLES2')     ,
                  'NVAL' =< range('VARIABLES1'^var),
                  'NVAL' =< range('VARIABLES2'^var)]).

ctr_typical(nvalue_on_intersection,
            ['NVAL'             > 0                      ,
             'NVAL'             < size('VARIABLES1')     ,
             'NVAL'             < size('VARIABLES2')     ,
             'NVAL'             < range('VARIABLES1'^var),
             'NVAL'             < range('VARIABLES2'^var),
             size('VARIABLES1') > 1                      ,
             size('VARIABLES2') > 1                      ]).

ctr_pure_functional_dependency(nvalue_on_intersection, []).
ctr_functional_dependency(nvalue_on_intersection, 1, [2,3]).

ctr_contractible(nvalue_on_intersection, ['NVAL'=0], 'VARIABLES1', any).
ctr_contractible(nvalue_on_intersection, ['NVAL'=0], 'VARIABLES2', any).

ctr_graph(nvalue_on_intersection,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NCC' = 'NVAL'],
          []).

ctr_example(nvalue_on_intersection,
            nvalue_on_intersection(2,
                                   [[var-1],[var-9],[var-1],[var-5]],
                                   [[var-2],[var-1],[var-9],[var-9],[var-6],[var-9]])).

ctr_draw_example(nvalue_on_intersection,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5]],
                  [[var-2],[var-1],[var-9],[var-9],[var-6],[var-9]]],
                 ['PRODUCT'],
                 [1-2,
                  2-[3,4,6],
                  3-2],
                 ['NCC'([[1,3,6],[2,7,8,10]])],
                 '','NCC=2',
                 [2.4,2.145,2.4,1.9]).

ctr_see_also(nvalue_on_intersection,
 [link('root concept',   nvalue,                        '',     []),
  link('common keyword', alldifferent_on_intersection, '%k',    ['constraint on the intersection']),
  link('common keyword', common,                       '%k',    ['constraint on the intersection'],'\\\\ '),
  link('common keyword', same_intersection,            '%k',    ['constraint on the intersection'])]).

ctr_key_words(nvalue_on_intersection,['counting constraint'           ,
                                      'number of distinct values'     ,
                                      'connected component'           ,
                                      'constraint on the intersection',
                                      'functional dependency'         ,
		                      'pure functional dependency'    ]).
