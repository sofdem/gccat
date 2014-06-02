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

ctr_date(ordered_atmost_nvector,['20080921']).

ctr_origin(ordered_atmost_nvector, 'Conjoin %c and %c.', [atmost_nvector,lex_chain_lesseq]).

ctr_types(ordered_atmost_nvector, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(ordered_atmost_nvector,
              ['NVEC'-dvar                       ,
               'VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(ordered_atmost_nvector,
                 [vals(['NVEC'],int,<,dontcare,dontcare)]).

ctr_synonyms(ordered_atmost_nvector,[ordered_atmost_nvectors,
                                     ordered_atmost_npoint  ,
                                     ordered_atmost_npoints ]).

ctr_restrictions(ordered_atmost_nvector,
                 [size('VECTOR') >= 1                     ,
                  'NVEC'         >= min(1,size('VECTORS')),
                  required('VECTORS',vec)                 ,
                  same_size('VECTORS',vec)                ]).

ctr_typical(ordered_atmost_nvector,
            [size('VECTOR')  > 1              ,
             'NVEC'          > 1              ,
             'NVEC'          < size('VECTORS'),
             size('VECTORS') > 1              ]).

ctr_contractible(ordered_atmost_nvector, [], 'VECTORS', any).

ctr_graph(ordered_atmost_nvector,
          ['VECTORS'],
          2,
          ['PATH'>>collection(vectors1,vectors2)],
          [lex_lesseq(vectors1^vec,vectors2^vec)],
          ['NARC' = size('VECTORS')-1],
          []).

ctr_graph(ordered_atmost_nvector,
          ['VECTORS'],
          2,
          ['PATH'>>collection(vectors1,vectors2)],
          [lex_less(vectors1^vec,vectors2^vec)],
          ['NCC' =< 'NVEC'],
          []).

ctr_example(ordered_atmost_nvector,
            ordered_atmost_nvector(3, [[vec-[[var-5], [var-6]]],
                                       [vec-[[var-5], [var-6]]],
                                       [vec-[[var-5], [var-6]]],
                                       [vec-[[var-9], [var-3]]],
                                       [vec-[[var-9], [var-3]]]])).

ctr_draw_example(ordered_atmost_nvector,
                 ['VECTORS'],
                 [[[vec-[[var-5], [var-6]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-9], [var-3]]],
                   [vec-[[var-9], [var-3]]]]],
                 ['PATH'],
                 [1-[2],
                  2-[3],
                  3-[],
                  4-[5],
                  5-[]],
                 ['NCC'([[1,2,3],[4,5]])],
                 '','NCC=2',
                 [2.145,2.3,2.8,2.33]).

ctr_see_also(ordered_atmost_nvector,
 [link('comparison swapped',        ordered_atleast_nvector, '',                                                        []),
  link('implies',                   atmost_nvector,          '',                                                        []),
  link('implies',                   lex_chain_lesseq,        '$\\argument{NVEC}$ of constraint %c removed',             [ordered_atmost_nvector]),
  link('implied by',                ordered_nvector,         '$\\leq\\argument{NVEC}$ replaced by $=\\argument{NVEC}$', []),
  link('common keyword',            nvector,                 '%k',                                                      ['vector']),
  link('used in graph description', lex_lesseq,              '',                                                        []),
  link('used in graph description', lex_less,                '',                                                        [])]).

ctr_key_words(ordered_atmost_nvector,['counting constraint',
                                      'symmetry'           ,
                                      'vector'             ,
                                      'order constraint'   ]).

ctr_eval(ordered_atmost_nvector, [reformulation(ordered_atmost_nvector_r)]).

ordered_atmost_nvector_r(0, []) :-
    !.
ordered_atmost_nvector_r(NVEC, VECTORS) :-
    eval(atmost_nvector(NVEC, VECTORS)),
    eval(lex_chain_lesseq(VECTORS)).
