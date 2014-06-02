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

ctr_date(k_used_by,['20050814','20060811']).

ctr_origin(k_used_by, 'Derived from %c', [used_by]).

ctr_types(k_used_by, ['VARIABLES'-collection(var-dvar)]).

ctr_arguments(k_used_by,
              ['SETS'-collection(set-'VARIABLES')]).

ctr_exchangeable(k_used_by,
                 [items('SETS',all),
                  items('SETS'^set,all),
                  vals(['SETS'^set^var],int,=\=,all,dontcare)]).

ctr_restrictions(k_used_by,
                 [required('VARIABLES',var)      ,
                  size('VARIABLES') >= 1         ,
                  required('SETS',set)           ,
                  size('SETS')      >  1         ,
                  non_increasing_size('SETS',set)]).

ctr_typical(k_used_by,
            [size('VARIABLES') > 1]).

ctr_contractible(k_used_by, [], 'SETS', any).

ctr_graph(k_used_by,
          ['SETS'],
          2,
          ['PATH'>>collection(set1,set2)],
          [used_by(set1^set,set2^set)],
          ['NARC' = size('SETS')-1],
          []).

ctr_example(k_used_by,
            k_used_by([[set-[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]]],
                       [set-[[var-9],[var-1],[var-1],[var-1],[var-2],[var-5]]],
                       [set-[[var-1],[var-1],[var-2],[var-5]]]])).

ctr_draw_example(k_used_by,
                 ['SETS'],
                 [[[set-[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]]],
                   [set-[[var-9],[var-1],[var-1],[var-1],[var-2],[var-5]]],
                   [set-[[var-1],[var-1],[var-2],[var-5]]]]],
                 ['PATH'],
                 [1-2,2-3],
                 ['NARC'],
                 '','NARC=2',
                 [1.5,2.145,3,4]).

ctr_see_also(k_used_by,
 [link('part of system of constraints', used_by,             '',   []),
  link('used in graph description',     used_by,             '',   []),
  link('implied by',                    k_same,              '',   []),
  link('common keyword',                k_used_by_interval,  '%k', ['system of constraints']),
  link('common keyword',                k_used_by_modulo,    '%k', ['system of constraints']),
  link('common keyword',                k_used_by_partition, '%k', ['system of constraints'])]).

ctr_key_words(k_used_by,['system of constraints'   ,
                         'decomposition'           ,
                         'multiset'                ,
                         'inclusion'               ,
                         'sort based reformulation']).

ctr_persons(k_used_by,['Elbassioni K. M.',
                       'Katriel I.'      ,
                       'Kutz M.'         ,
                       'Mahajan M.'      ]).

ctr_eval(k_used_by, [reformulation(k_used_by_r)]).

k_used_by_r(SETS) :-
    length(SETS, N),
    N > 1,
    collection(SETS, [non_empty_col([dvar])]),
    get_attr1(SETS, VARS),
    k_used_by1(VARS).

k_used_by1([_]) :- !.
k_used_by1([V1,V2|R]) :-
    eval(used_by(V1, V2)),
    k_used_by1([V2|R]).
