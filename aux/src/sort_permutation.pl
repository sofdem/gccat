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

ctr_date(sort_permutation,['20030820','20060816','20111025']).

ctr_origin(sort_permutation, '\\cite{Zhou97}', []).

ctr_usual_name(sort_permutation, sort).

ctr_arguments(sort_permutation,
              ['FROM'-collection(var-dvar),
               'PERMUTATION'-collection(var-dvar),
               'TO'-collection(var-dvar)]).

ctr_exchangeable(sort_permutation,
                 [translate(['FROM'^var,'TO'^var])]).

ctr_synonyms(sort_permutation,[extended_sortedness, sortedness, sorted, sorting]).

ctr_restrictions(sort_permutation,
                 [size('PERMUTATION') = size('FROM')      ,
                  size('PERMUTATION') = size('TO')        ,
                  'PERMUTATION'^var >= 1                  ,
                  'PERMUTATION'^var =< size('PERMUTATION'),
                  alldifferent('PERMUTATION')             ,
                  required('FROM',var)                    ,
                  required('PERMUTATION',var)             ,
                  required('TO',var)                      ]).

ctr_typical(sort_permutation,
            [size('FROM')      > 1     ,
             range('FROM'^var) > 1     ,
             lex_different('FROM','TO')]).

ctr_functional_dependency(sort_permutation, 3, [1]).
ctr_functional_dependency(sort_permutation, 2, [1,3]).

ctr_derived_collections(sort_permutation,
                        [col('FROM_PERMUTATION'-collection(var-dvar, ind-dvar),
                             [item(var-'FROM'^var, ind-'PERMUTATION'^var)])]).

ctr_graph(sort_permutation,
          ['FROM_PERMUTATION','TO'],
          2,
          ['PRODUCT'>>collection(from_permutation,to)],
          [from_permutation^var = to^var,
           from_permutation^ind = to^key],
          ['NARC' = size('PERMUTATION')],
          []).

ctr_graph(sort_permutation,
          ['TO'],
          2,
          ['PATH'>>collection(to1,to2)],
          [to1^var =< to2^var],
          ['NARC' = size('TO')-1],
          []).

ctr_example(sort_permutation,
            sort_permutation([[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                             [[var-1],[var-6],[var-3],[var-5],[var-4],[var-2]],
                             [[var-1],[var-1],[var-1],[var-2],[var-5],[var-9]])).

ctr_draw_example(sort_permutation,
                 ['FROM_PERMUTATION','TO'],
                 [[[var-1,ind-1],[var-9,ind-6],[var-1,ind-3],
                   [var-5,ind-5],[var-2,ind-4],[var-1,ind-2]],
                  [[var-1],[var-1],[var-1],[var-2],[var-5],[var-9]]],
                 ['PRODUCT'],
                 [1-1,2-6,3-3,4-5,5-4,6-2],
                 ['NARC'],
                 '','NARC=6',
                 [2.145,2.145,2.8,1.9]).

ctr_see_also(sort_permutation,
 [link('implies',               correspondence, '',                     []),
  link('specialisation',        sort,           '%e parameter removed', ['PERMUTATION']),
  link('used in reformulation', element,        '',                     []),
  link('used in reformulation', alldifferent,   '',                     []),
  link('used in reformulation', increasing,     '',                     []),
  link('common keyword',        order,          '%k, %k',               [sort, permutation])]).

ctr_key_words(sort_permutation,['constraint between three collections of variables',
                                'sort'                                             ,
                                'permutation'                                      ,
                                'functional dependency'                            ,
                                'derived collection'                               ]).

ctr_persons(sort_permutation,['Zhou J.'  ,
                              'Schaus P.']).

ctr_eval(sort_permutation, [builtin(sort_permutation_b)]).

sort_permutation_b(FROM, PERMUTATION, TO) :-
    length(FROM,        F),
    length(PERMUTATION, P),
    length(TO,          T),
    F = P,
    P = T,
    collection(FROM,        [dvar     ]),
    collection(PERMUTATION, [dvar(1,P)]),
    collection(TO,          [dvar     ]),
    get_attr1(FROM       , FVARS),
    get_attr1(PERMUTATION, PVARS),
    get_attr1(TO         , TVARS),
    sorting(FVARS, PVARS, TVARS).
