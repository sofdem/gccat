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

ctr_date(soft_used_by_interval_var,['20050507','20060816']).

ctr_origin(soft_used_by_interval_var, 'Derived from %c.', [used_by_interval]).

ctr_arguments(soft_used_by_interval_var,
              ['C'-dvar                         ,
               'VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'SIZE_INTERVAL'-int              ]).

ctr_exchangeable(soft_used_by_interval_var,
                 [items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare),
                  vals(['VARIABLES2'^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare)]).

ctr_synonyms(soft_used_by_interval_var,[soft_used_by_interval]).

ctr_restrictions(soft_used_by_interval_var,
                 ['C' >= 0                                ,
                  'C' =< size('VARIABLES2')               ,
                  size('VARIABLES1') >= size('VARIABLES2'),
                  required('VARIABLES1',var)              ,
                  required('VARIABLES2',var)              ,
                  'SIZE_INTERVAL' > 0                     ]).

ctr_typical(soft_used_by_interval_var,
            ['C'                     > 0                      ,
             size('VARIABLES1')      > 1                      ,
             size('VARIABLES2')      > 1                      ,
             range('VARIABLES1'^var) > 1                      ,
             range('VARIABLES2'^var) > 1                      ,
             'SIZE_INTERVAL'         > 1                      ,
             'SIZE_INTERVAL'         < range('VARIABLES1'^var),
             'SIZE_INTERVAL'         < range('VARIABLES2'^var)]).

ctr_graph(soft_used_by_interval_var,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var / 'SIZE_INTERVAL' = variables2^var / 'SIZE_INTERVAL'],
          ['NSINK_NSOURCE' = size('VARIABLES2')-'C'],
          []).

ctr_example(soft_used_by_interval_var,
            soft_used_by_interval_var(2,[[var-9],[var-1],[var-1],[var-8],[var-8]],
                                                [[var-9],[var-9],[var-9],[var-1]],3)).

ctr_draw_example(soft_used_by_interval_var,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-9],[var-1],[var-1],[var-8],[var-8]],
                  [[var-9],[var-9],[var-9],[var-1]]],
                 ['PRODUCT'],
                 [1-[1,2,3],
                  2-4,
                  3-4],
                 ['NSINK_NSOURCE'([1,2,3,6,7,8,9])],
                 '','NSINK_NSOURCE=min(1,3)+min(2,1)=2',
                 [2.145,2.145,2.8,2.145]).

ctr_see_also(soft_used_by_interval_var,
 [link('implied by',   soft_same_interval_var, '', []),
  link('hard version', used_by_interval,       '', [])]).

ctr_key_words(soft_used_by_interval_var,['soft constraint'                                ,
                                         'constraint between two collections of variables',
                                         'relaxation'                                     ,
                                         'variable-based violation measure'               ,
                                         'interval'                                       ]).

ctr_eval(soft_used_by_interval_var, [reformulation(soft_used_by_interval_var_r)]).

soft_used_by_interval_var_r(C, VARIABLES1, VARIABLES2, SIZE_INTERVAL) :-
    length(VARIABLES1, L1),
    length(VARIABLES2, L2),
    L1 >= L2,
    check_type(dvar(0,L2), C),
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
	integer(SIZE_INTERVAL),
	SIZE_INTERVAL > 0,
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
	gen_quotient(VARS1, SIZE_INTERVAL, QUOTVARS1),
	gen_quotient(VARS2, SIZE_INTERVAL, QUOTVARS2),
	gen_collection(QUOTVARS1, var, CVARS1),
	gen_collection(QUOTVARS2, var, CVARS2),
    eval(soft_used_by_var(C, CVARS1, CVARS2)).
