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

ctr_date(common,['20000128','20030820','20060805']).

ctr_origin(common, 'N.~Beldiceanu', []).

ctr_arguments(common,
              ['NCOMMON1'-dvar                  ,
               'NCOMMON2'-dvar                  ,
               'VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(common,
                 [args([['NCOMMON1','NCOMMON2'],['VARIABLES1','VARIABLES2']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_restrictions(common,
                 ['NCOMMON1' >= 0                 ,
                  'NCOMMON1' =< size('VARIABLES1'),
                  'NCOMMON2' >= 0                 ,
                  'NCOMMON2' =< size('VARIABLES2'),
                  required('VARIABLES1',var)      ,
                  required('VARIABLES2',var)      ]).

ctr_typical(common,
            [size('VARIABLES1')      > 1,
             range('VARIABLES1'^var) > 1,
             size('VARIABLES2')      > 1,
             range('VARIABLES2'^var) > 1]).

ctr_pure_functional_dependency(common, []).
ctr_functional_dependency(common, 1, [3,4]).
ctr_functional_dependency(common, 2, [3,4]).

ctr_graph(common,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSOURCE' = 'NCOMMON1',
           'NSINK'   = 'NCOMMON2'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(common,
            common(3,4,
                   [[var-1],[var-9],[var-1],[var-5]],
                   [[var-2],[var-1],[var-9],[var-9],[var-6],[var-9]])).

ctr_draw_example(common,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5]],
                  [[var-2],[var-1],[var-9],[var-9],[var-6],[var-9]]],
                 ['PRODUCT'],
                 [1-2,
                  2-[3,4,6],
                  3-2],
                 ['NSOURCE'([1,2,3]), 'NSINK'([6,7,8,10])],
                 '','NSOURCE=3,NSINK=4',
                 [2.145,2.145,2.145,1.5]).

ctr_see_also(common,
 [link(generalisation,    common_interval ,             '%e replaced by %e', [variable,variable/constant]),
  link(generalisation,    common_modulo   ,             '%e replaced by %e', [variable,variable mod constant]),
  link(generalisation,    common_partition,             '%e replaced by %e', [variable,in_list(variable,partition)]),  
  link(specialisation,    uses,                         '%e$=$%e',           ['NCOMMON2',size('VARIABLES2')]),
  link('common keyword',  alldifferent_on_intersection, '%k',                ['constraint on the intersection']),
  link('common keyword',  nvalue_on_intersection,       '%k',                ['constraint on the intersection']),
  link('common keyword',  same_intersection,            '%k',                ['constraint on the intersection']),
  link('root concept',    among,                        '',                  []),
  link('related',         among_var,                    '',                  []),
  link('related',         roots,                        '',                  [])]).

ctr_key_words(common,['constraint between two collections of variables',
                      'constraint on the intersection'                 ,
                      '3-SAT'                                          ,
                      'acyclic'                                        ,
                      'bipartite'                                      ,
                      'no loop'                                        ,
                      'functional dependency'                          ,
		      'pure functional dependency'                     ]).

ctr_persons(common,['Beldiceanu N.' ,
                    'Bessi\\`ere C.',
                    'Hebrard E.'    ,
                    'Hnich B.'      ,
                    'Walsh T.'      ]).

ctr_eval(common, [reformulation(common_r)]).

common_r(NCOMMON1, NCOMMON2, VARIABLES1, VARIABLES2) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    length(VARIABLES1, N1),
    length(VARIABLES2, N2),
    check_type(dvar(0,N1), NCOMMON1),
    check_type(dvar(0,N2), NCOMMON2),
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    common1(VARS1, VARS2, _MAT12, SUM1),
    call(NCOMMON1 #= SUM1),
    common1(VARS2, VARS1, _MAT21, SUM2),
    call(NCOMMON2 #= SUM2).
