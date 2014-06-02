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

ctr_date(common_interval,['20030820','20060805']).

ctr_origin(common_interval, 'Derived from %c.', [common]).

ctr_arguments(common_interval,
              ['NCOMMON1'-dvar                  ,
               'NCOMMON2'-dvar                  ,
               'VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'SIZE_INTERVAL'-int              ]).

ctr_exchangeable(common_interval,
                 [args([['NCOMMON1','NCOMMON2'],['VARIABLES1','VARIABLES2'],['SIZE_INTERVAL']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare),
                  vals(['VARIABLES2'^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare)]).

ctr_restrictions(common_interval,
                 ['NCOMMON1' >= 0                 ,
                  'NCOMMON1' =< size('VARIABLES1'),
                  'NCOMMON2' >= 0                 ,
                  'NCOMMON2' =< size('VARIABLES2'),
                  required('VARIABLES1',var)      ,
                  required('VARIABLES2',var)      ,
                  'SIZE_INTERVAL' > 0             ]).

ctr_typical(common_interval,
            [size('VARIABLES1')      > 1                      ,
             range('VARIABLES1'^var) > 1                      ,
             size('VARIABLES2')      > 1                      ,
             range('VARIABLES2'^var) > 1                      ,
             'SIZE_INTERVAL'         > 1                      ,
             'SIZE_INTERVAL'         < range('VARIABLES1'^var),
             'SIZE_INTERVAL'         < range('VARIABLES2'^var)]).

ctr_pure_functional_dependency(common_interval, []).
ctr_functional_dependency(common_interval, 1, [3,4,5]).
ctr_functional_dependency(common_interval, 2, [3,4,5]).

ctr_graph(common_interval,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var / 'SIZE_INTERVAL' = variables2^var / 'SIZE_INTERVAL'],
          ['NSOURCE' = 'NCOMMON1',
           'NSINK'   = 'NCOMMON2'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(common_interval,
            common_interval(3,2,
                            [[var-8],[var-6],[var-6],[var-0]],
                            [[var-7],[var-3],[var-3],[var-3],[var-3],[var-7]],
                            3)).

ctr_draw_example(common_interval,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-8],[var-6],[var-6],[var-0]],
                  [[var-7],[var-3],[var-3],[var-3],[var-3],[var-7]]],
                 ['PRODUCT'],
                 [1-[1,6],
                  2-[1,6],
                  3-[1,6]],
                 ['NSOURCE'([1,2,3]), 'NSINK'([5,10])],
                 '','NSOURCE=3,NSINK=2',
                 [2.6,2.145,1.6,1.6]).

ctr_see_also(common_interval,
 [link(specialisation, common, '%e replaced by %e', [variable/constant,variable])]).

ctr_key_words(common_interval,['constraint between two collections of variables',
                               'interval'                                       ,
                               'acyclic'                                        ,
                               'bipartite'                                      ,
                               'no loop'                                        ,
                               'functional dependency'                          ,
		               'pure functional dependency'                     ]).

ctr_eval(common_interval, [reformulation(common_interval_r)]).

common_interval_r(NCOMMON1, NCOMMON2, VARIABLES1, VARIABLES2, SIZE_INTERVAL) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    length(VARIABLES1, N1),
    length(VARIABLES2, N2),
    check_type(dvar(0,N1), NCOMMON1),
    check_type(dvar(0,N2), NCOMMON2),
    integer(SIZE_INTERVAL),
    SIZE_INTERVAL > 0,
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    gen_quotient(VARS1, SIZE_INTERVAL, QUOTVARS1),
    gen_quotient(VARS2, SIZE_INTERVAL, QUOTVARS2),
    common1(QUOTVARS1, QUOTVARS2, _MAT12, SUM1),
    call(NCOMMON1 #= SUM1),
    common1(QUOTVARS2, QUOTVARS1, _MAT21, SUM2),
    call(NCOMMON2 #= SUM2).
