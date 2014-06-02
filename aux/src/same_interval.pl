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

ctr_date(same_interval,['20030820','20060814']).

ctr_origin(same_interval, 'Derived from %c.', [same]).

ctr_arguments(same_interval,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'SIZE_INTERVAL'-int              ]).

ctr_exchangeable(same_interval,
                 [args([['VARIABLES1','VARIABLES2'],['SIZE_INTERVAL']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES'^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare)]).

ctr_restrictions(same_interval,
                 [size('VARIABLES1') = size('VARIABLES2'),
                  required('VARIABLES1',var)             ,
                  required('VARIABLES2',var)             ,
                  'SIZE_INTERVAL' > 0                    ]).

ctr_typical(same_interval,
            [size('VARIABLES1')      > 1                      ,
             range('VARIABLES1'^var) > 1                      ,
             range('VARIABLES2'^var) > 1                      ,
             'SIZE_INTERVAL'         > 1                      ,
             'SIZE_INTERVAL'         < range('VARIABLES1'^var),
             'SIZE_INTERVAL'         < range('VARIABLES2'^var)]).

% same_interval('VARIABLES11', 'VARIABLES12', 'SIZE_INTERVAL') and
% same_interval('VARIABLES21', 'VARIABLES22', 'SIZE_INTERVAL') =>
% same_interval(union('VARIABLES11','VARIABLES21'), union('VARIABLES12','VARIABLES22'), 'SIZE_INTERVAL')
ctr_aggregate(same_interval, [], [union, union, id]).

ctr_graph(same_interval,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var / 'SIZE_INTERVAL' = variables2^var / 'SIZE_INTERVAL'],
          [for_all('CC','NSOURCE' = 'NSINK'),
           'NSOURCE' = size('VARIABLES1'),
           'NSINK'   = size('VARIABLES2')],
          []).

ctr_example(same_interval,
            same_interval([[var-1],[var-7],[var-6],[var-0],[var-1],[var-7]],
                          [[var-8],[var-8],[var-8],[var-0],[var-1],[var-2]],
                          3)).

ctr_draw_example(same_interval,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-7],[var-6],[var-0],[var-1],[var-7]],
                  [[var-8],[var-8],[var-8],[var-0],[var-1],[var-2]]],
                 ['PRODUCT'],
                 [1-[4,5,6],
                  2-[1,2,3],
                  3-[1,2,3],
                  4-[4,5,6],
                  5-[4,5,6],
                  6-[1,2,3]],
                 ['NSOURCE'([1,2,3,4,5,6]),
                  'NSINK'([7,8,9,10,11,12]),
                  'NCC'([[1,4,5,10,11,12],[2,3,6,7,8,9]])],
                 '','CC#1:NSOURCE=3,NSINK=3\\nCC#2:NSOURCE=3,NSINK=3',
                 [3,2.3,3.4,2.27]).

ctr_see_also(same_interval,
 [link('system of constraints', k_same_interval,        '',                  []),
  link('soft variant',          soft_same_interval_var, '%k',                ['variable-based violation measure']),
  link('implies',               used_by_interval,       '',                  []),
  link('specialisation',        same,                   '%e replaced by %e', [variable/constant,variable])]).

ctr_key_words(same_interval,['constraint between two collections of variables',
                             'permutation'                                    ,
                             'sort based reformulation'                       ,
                             'interval'                                       ]).

ctr_eval(same_interval, [reformulation(same_interval_r)]).

same_interval_r(VARIABLES1, VARIABLES2, SIZE_INTERVAL) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
    N1 = N2,
	integer(SIZE_INTERVAL),
	SIZE_INTERVAL > 0,
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
	gen_quotient(VARS1, SIZE_INTERVAL, QUOTVARS1),
	gen_quotient(VARS2, SIZE_INTERVAL, QUOTVARS2),
	same1(QUOTVARS1, QUOTVARS2).
