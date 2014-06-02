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

ctr_date(balance_interval,['20030820','20060804']).

ctr_origin(balance_interval, 'Derived from %c.', [balance]).

ctr_arguments(balance_interval,
              ['BALANCE'-dvar                  ,
               'VARIABLES'-collection(var-dvar),
               'SIZE_INTERVAL'-int             ]).

ctr_exchangeable(balance_interval,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare)]).

ctr_restrictions(balance_interval,
                 ['BALANCE' >= 0                         ,
                  'BALANCE' =< max(0,size('VARIABLES')-2),
                  required('VARIABLES',var)              ,
                  'SIZE_INTERVAL' > 0                    ]).

ctr_typical(balance_interval,
            [size('VARIABLES') > 2                     ,
             'SIZE_INTERVAL'   > 1                     ,
             'SIZE_INTERVAL'   < range('VARIABLES'^var)]).

ctr_pure_functional_dependency(balance_interval, []).
ctr_functional_dependency(balance_interval, 1, [2,3]).

ctr_graph(balance_interval,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var / 'SIZE_INTERVAL' = variables2^var / 'SIZE_INTERVAL'],
          ['RANGE_NSCC' = 'BALANCE'],
          ['EQUIVALENCE']).

ctr_example(balance_interval,
            balance_interval(3,[[var-6],[var-4],[var-3],[var-3],[var-4]],3)).

ctr_draw_example(balance_interval,
                 ['VARIABLES'],
                 [[[var-6],[var-4],[var-3],[var-3],[var-4]]],
                 ['CLIQUE'],
                 [1-1,
                  2-[2,3,4,5],
                  3-[2,3,4,5],
                  4-[2,3,4,5],
                  5-[2,3,4,5]],
                 ['RANGE_NSCC'([1],[2,3,4,5])],
                 '','RANGE_NSCC=4-1=3',
                 [2.145,2.145,2.145,2.2]).

ctr_see_also(balance_interval,
 [link(specialisation, balance, '%e replaced by %e', [variable/constant,variable])]).

ctr_key_words(balance_interval,['value constraint'                ,
                                'interval'                        ,
                                'assignment'                      ,
                                'balanced assignment'             ,
                                'automaton'                       ,
                                'automaton with array of counters',
                                'equivalence'                     ,
                                'functional dependency'           ,
		                'pure functional dependency'      ]).
