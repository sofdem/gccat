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

ctr_date(same_modulo,['20030820','20060814']).

ctr_origin(same_modulo, 'Derived from %c.', [same]).

ctr_arguments(same_modulo,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'M'-int                          ]).

ctr_exchangeable(same_modulo,
                 [args([['VARIABLES1','VARIABLES2'],['M']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES'^var],mod('M'),=,dontcare,dontcare)]).

ctr_restrictions(same_modulo,
                 [size('VARIABLES1') = size('VARIABLES2'),
                  required('VARIABLES1',var)             ,
                  required('VARIABLES2',var)             ,
                  'M' > 0                                ]).

ctr_typical(same_modulo,
            [size('VARIABLES1')      > 1                       ,
             range('VARIABLES1'^var) > 1                       ,
             range('VARIABLES2'^var) > 1                       ,
             'M'                     > 1                       ,
             'M'                     < maxval('VARIABLES1'^var),
             'M'                     < maxval('VARIABLES2'^var)]).

% same_modulo('VARIABLES11', 'VARIABLES12', 'M') and
% same_modulo('VARIABLES21', 'VARIABLES22', 'M') =>
% same_modulo(union('VARIABLES11','VARIABLES21'), union('VARIABLES12','VARIABLES22'), 'M')
ctr_aggregate(same_modulo, [], [union, union, id]).

ctr_graph(same_modulo,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var mod 'M' = variables2^var mod 'M'],
          [for_all('CC','NSOURCE' = 'NSINK'),
           'NSOURCE' = size('VARIABLES1'),
           'NSINK'   = size('VARIABLES2')],
          []).

ctr_example(same_modulo,
            same_modulo([[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                        [[var-6],[var-4],[var-1],[var-1],[var-5],[var-5]],
                        3)).

ctr_draw_example(same_modulo,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                  [[var-6],[var-4],[var-1],[var-1],[var-5],[var-5]]],
                 ['PRODUCT'],
                 [1-[2,3,4],
                  2-1,
                  3-[2,3,4],
                  4-[5,6],
                  5-[5,6],
                  6-[2,3,4]],
                 ['NSOURCE'([1,2,3,4,5,6]),
                  'NSINK'([7,8,9,10,11,12]),
                  'NCC'([[1,3,6,8,9,10],[2,7],[4,5,11,12]])],
                 '','CC#1:NSOURCE=3,NSINK=3\\nCC#2:NSOURCE=1,NSINK=1\\nCC#3:NSOURCE=2,NSINK=2',
                 [3,2.3,3.4,2.27]).

ctr_see_also(same_modulo,
 [link('system of constraints', k_same_modulo,        '',                  []),
  link('soft variant',          soft_same_modulo_var, '%k',                ['variable-based violation measure']),
  link('implies',               used_by_modulo,       '',                  []),
  link('specialisation',        same,                 '%e replaced by %e', [variable mod constant,variable])]).

ctr_key_words(same_modulo,['constraint between two collections of variables',
                           'permutation'                                    ,
                           'sort based reformulation'                       ,
                           'modulo'                                         ]).

ctr_eval(same_modulo, [reformulation(same_modulo_r)]).

same_modulo_r(VARIABLES1, VARIABLES2, M) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
    N1 = N2,
    integer(M),
	M > 0,
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
	gen_remainder(VARS1, M, REMVARS1),
	gen_remainder(VARS2, M, REMVARS2),
    same1(REMVARS1, REMVARS2).
