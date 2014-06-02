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
    ctr_pure_functional_dependency/2,
    ctr_functional_dependency/3,
    ctr_typical/2,
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

ctr_date(k_disjoint,['20050816','20060811']).

ctr_origin(k_disjoint, 'Derived from %c', [disjoint]).

ctr_types(k_disjoint, ['VARIABLES'-collection(var-dvar)]).

ctr_arguments(k_disjoint,
              ['SETS'-collection(set-'VARIABLES')]).

ctr_exchangeable(k_disjoint,
                 [items('SETS',all),
                  items('SETS'^set,all),
                  vals(['VARIABLES'^var],int,=\=,dontcare,in),
                  vals(['SETS'^set^var],int,=\=,all,dontcare)]).

ctr_restrictions(k_disjoint,
                 [required('VARIABLES',var),
                  size('VARIABLES') >= 1   ,
                  required('SETS',set)     ,
                  size('SETS')      >  1   ]).

ctr_typical(k_disjoint,
            [size('VARIABLES') > 1]).

ctr_contractible(k_disjoint, [], 'SETS', any).

ctr_graph(k_disjoint,
          ['SETS'],
          2,
          ['CLIQUE'(<)>>collection(set1,set2)],
          [disjoint(set1^set,set2^set)],
          ['NARC' = (size('SETS')*(size('SETS')-1)) / 2],
          []).

ctr_example(k_disjoint,
            k_disjoint([[set-[[var-1],[var-9],[var-1],[var-5]]],
                        [set-[[var-2],[var-7],[var-7],[var-0],[var-6],[var-8]]],
                        [set-[[var-4],[var-4],[var-3]]]])).

ctr_draw_example(k_disjoint,
                 ['SETS'],
                 [[[set-[[var-1],[var-9],[var-1],[var-5]]],
                   [set-[[var-2],[var-7],[var-7],[var-0],[var-6],[var-8]]],
                   [set-[[var-4],[var-4],[var-3]]]]],
                 ['CLIQUE'(<)],
                 [1-[2,3],
                  2-3],
                 ['NARC'],
                 '','NARC=3',
                 [1.5,2.145,3,3]).

ctr_see_also(k_disjoint,
 [link('part of system of constraints', disjoint, '', []),
  link('used in graph description',     disjoint, '', [])]).

ctr_key_words(k_disjoint,['system of constraints',
                          'decomposition'        ,
                          'value constraint'     ,
                          'empty intersection'   ,
                          'disequality'          ]).

ctr_eval(k_disjoint, [reformulation(k_disjoint_r)]).

k_disjoint_r(SETS) :-
    length(SETS, N),
    N > 1,
    collection(SETS, [non_empty_col([dvar])]),
    get_attr1(SETS, VARS),
    k_disjoint1(VARS).

k_disjoint1([_]) :- !.
k_disjoint1([V1,V2|R]) :-
    k_disjoint2([V2|R], V1),
    k_disjoint1([V2|R]).

k_disjoint2([], _).
k_disjoint2([U|R], V) :-
    eval(disjoint(V, U)),
    k_disjoint2(R, V).
