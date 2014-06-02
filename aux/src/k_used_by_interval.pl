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

ctr_date(k_used_by_interval,['20050814','20060811']).

ctr_origin(k_used_by_interval, 'Derived from %c and from %c.', [used_by_interval,k_used_by]).

ctr_types(k_used_by_interval, ['VARIABLES'-collection(var-dvar)]).

ctr_arguments(k_used_by_interval,
              ['SETS'-collection(set-'VARIABLES'),
               'SIZE_INTERVAL'-int               ]).

ctr_exchangeable(k_used_by_interval,
                 [items('SETS',all),
                  items('SETS'^set,all),
                  vals(['SETS'^set^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare)]).

ctr_restrictions(k_used_by_interval,
                 [required('VARIABLES',var)      ,
                  size('VARIABLES') >= 1         ,
                  required('SETS',set)           ,
                  size('SETS')      >  1         ,
                  non_increasing_size('SETS',set),
                  'SIZE_INTERVAL'   > 0          ]).

ctr_typical(k_used_by_interval,
            [size('VARIABLES') > 1,
             'SIZE_INTERVAL'   > 0]).

ctr_contractible(k_used_by_interval, [], 'SETS', any).

ctr_graph(k_used_by_interval,
          ['SETS'],
          2,
          ['PATH'>>collection(set1,set2)],
          [used_by_interval(set1^set,set2^set,'SIZE_INTERVAL')],
          ['NARC' = size('SETS')-1],
          []).

ctr_example(k_used_by_interval,
            k_used_by_interval([[set-[[var-1],[var-1],[var-1],[var-8],[var-6],[var-2]]],
                                [set-[[var-1],[var-0],[var-7],[var-7]]],
                                [set-[[var-1],[var-2]]]],3)).

ctr_draw_example(k_used_by_interval,
                 ['SETS'],
                 [[[set-[[var-1],[var-1],[var-1],[var-8],[var-6],[var-2]]],
                   [set-[[var-1],[var-0],[var-7],[var-7]]],
                   [set-[[var-1],[var-2]]]]],
                 ['PATH'],
                 [1-2,2-3],
                 ['NARC'],
                 '','NARC=2',
                 [1.5,2.145,3,3]).

ctr_see_also(k_used_by_interval,
 [link('part of system of constraints', used_by_interval, '',   []),
  link('used in graph description',     used_by_interval, '',   []),
  link('implied by',                    k_same_interval,  '',   []),
  link('common keyword',                k_used_by,        '%k', ['system of constraints'])]).

ctr_key_words(k_used_by_interval,['system of constraints'   ,
                                  'decomposition'           ,
                                  'inclusion'               ,
                                  'interval'                ,
                                  'sort based reformulation']).

ctr_eval(k_used_by_interval, [reformulation(k_used_by_interval_r)]).

k_used_by_interval_r(SETS, SIZE_INTERVAL) :-
    length(SETS, N),
    N > 1,
    collection(SETS, [non_empty_col([dvar])]),
	integer(SIZE_INTERVAL),
	SIZE_INTERVAL > 0,
    get_attr1(SETS, VARS),
    k_used_by_interval1(VARS, SIZE_INTERVAL).

k_used_by_interval1([_], _) :- !.
k_used_by_interval1([V1,V2|R], SIZE_INTERVAL) :-
    eval(used_by_interval(V1, V2, SIZE_INTERVAL)),
    k_used_by_interval1([V2|R], SIZE_INTERVAL).
