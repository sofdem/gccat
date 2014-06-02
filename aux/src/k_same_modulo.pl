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

ctr_date(k_same_modulo,['20050810','20060811']).

ctr_origin(k_same_modulo, 'Derived from %c and from %c.', [same_modulo,k_same]).

ctr_types(k_same_modulo, ['VARIABLES'-collection(var-dvar)]).

ctr_arguments(k_same_modulo,
              ['SETS'-collection(set-'VARIABLES'),
               'M'-int                           ]).

ctr_exchangeable(k_same_modulo,
                 [items('SETS',all),
                  items('SETS'^set,all),
                  vals(['SETS'^set^var],mod('M'),=,dontcare,dontcare)]).

ctr_restrictions(k_same_modulo,
                 [required('VARIABLES',var),
                  size('VARIABLES') >= 1   ,
                  required('SETS',set)     ,
                  size('SETS')      >  1   ,
                  same_size('SETS',set)    ,
                  'M'               >  0   ]).

ctr_typical(k_same_modulo,
            [size('VARIABLES') > 1,
             'M'               > 1]).

ctr_contractible(k_same_modulo, [], 'SETS', any).

ctr_graph(k_same_modulo,
          ['SETS'],
          2,
          ['PATH'>>collection(set1,set2)],
          [same_modulo(set1^set,set2^set,'M')],
          ['NARC' = size('SETS')-1],
          []).

ctr_example(k_same_modulo,
            k_same_modulo([[set-[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]]],
                           [set-[[var-6],[var-4],[var-1],[var-1],[var-5],[var-5]]],
                           [set-[[var-1],[var-3],[var-4],[var-2],[var-8],[var-7]]]],3)).

ctr_draw_example(k_same_modulo,
                 ['SETS'],
                 [[[set-[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]]],
                   [set-[[var-6],[var-4],[var-1],[var-1],[var-5],[var-5]]],
                   [set-[[var-1],[var-3],[var-4],[var-2],[var-8],[var-7]]]]],
                 ['PATH'],
                 [1-2,2-3],
                 ['NARC'],
                 '','NARC=2',
                 [1.5,2.145,3,4]).

ctr_see_also(k_same_modulo,
 [link('part of system of constraints', same_modulo,      '',   []),
  link('used in graph description',     same_modulo,      '',   []),
  link('implies',                       k_used_by_modulo, '',   []),
  link('common keyword',                k_same,           '%k', ['system of constraints'])]).

ctr_key_words(k_same_modulo,['system of constraints'   ,
                             'decomposition'           ,
                             'permutation'             ,
                             'sort based reformulation',
                             'modulo'                  ]).

ctr_eval(k_same_modulo, [reformulation(k_same_modulo_r)]).

k_same_modulo_r(SETS, M) :-
    length(SETS, N),
    N > 1,
    collection(SETS, [non_empty_col([dvar])]),
	integer(M),
	M =\= 0,
    get_attr1(SETS, VARS),
    k_same_modulo1(VARS, M).

k_same_modulo1([_], _) :- !.
k_same_modulo1([V1,V2|R], M) :-
    eval(same_modulo(V1, V2, M)),
    k_same_modulo1([V2|R], M).
