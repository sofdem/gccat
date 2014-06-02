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

ctr_date(ordered_atleast_nvector,['20080921']).

ctr_origin(ordered_atleast_nvector, 'Conjoin %c and %c.', [atleast_nvector,lex_chain_lesseq]).

ctr_types(ordered_atleast_nvector, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(ordered_atleast_nvector,
              ['NVEC'-dvar                       ,
               'VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(ordered_atleast_nvector,
                 [vals(['NVEC'],int(>=(0)),>,dontcare,dontcare)]).

ctr_synonyms(ordered_atleast_nvector,[ordered_atleast_nvectors,
                                      ordered_atleast_npoint  ,
                                      ordered_atleast_npoints ]).

ctr_restrictions(ordered_atleast_nvector,
                 [size('VECTOR') >= 1              ,
                  'NVEC'         >= 0              ,
                  'NVEC'         =< size('VECTORS'),
                  required('VECTORS',vec)          ,
                  same_size('VECTORS',vec)         ]).

ctr_typical(ordered_atleast_nvector,
            [size('VECTOR')  > 1              ,
             'NVEC'          > 0              ,
             'NVEC'          < size('VECTORS'),
             size('VECTORS') > 1              ]).

ctr_graph(ordered_atleast_nvector,
          ['VECTORS'],
          2,
          ['PATH'>>collection(vectors1,vectors2)],
          [lex_lesseq(vectors1^vec,vectors2^vec)],
          ['NARC' = size('VECTORS')-1],
          []).

ctr_graph(ordered_atleast_nvector,
          ['VECTORS'],
          2,
          ['PATH'>>collection(vectors1,vectors2)],
          [lex_less(vectors1^vec,vectors2^vec)],
          ['NCC' >= 'NVEC'],
          []).

ctr_example(ordered_atleast_nvector,
            ordered_atleast_nvector(2, [[vec-[[var-5], [var-6]]],
                                        [vec-[[var-5], [var-6]]],
                                        [vec-[[var-5], [var-6]]],
                                        [vec-[[var-9], [var-3]]],
                                        [vec-[[var-9], [var-4]]]])).

ctr_draw_example(ordered_atleast_nvector,
                 ['VECTORS'],
                 [[[vec-[[var-5], [var-6]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-9], [var-3]]],
                   [vec-[[var-9], [var-4]]]]],
                 ['PATH'],
                 [1-[2],
                  2-[3],
                  3-[],
                  4-[],
                  5-[]],
                 ['NCC'([[1,2,3],[4],[5]])],
                 '','NCC=3',
                 [2.145,2.3,2.8,2.33]).

ctr_see_also(ordered_atleast_nvector,
 [link('comparison swapped',        ordered_atmost_nvector, '',                                                        []),
  link('implies',                   atleast_nvector,        '',                                                        []),
  link('implies',                   lex_chain_lesseq,       '$\\argument{NVEC}$ of constraint %c removed',             [ordered_atleast_nvector]),
  link('implied by',                ordered_nvector,        '$\\geq\\argument{NVEC}$ replaced by $=\\argument{NVEC}$', []),
  link('common keyword',            nvector,                '%k',                                                      ['vector']),
  link('used in graph description', lex_lesseq,             '',                                                        []),
  link('used in graph description', lex_less,               '',                                                        [])]).

ctr_key_words(ordered_atleast_nvector,['counting constraint',
                                       'symmetry'           ,
                                       'vector'             ,
                                       'order constraint'   ]).

ctr_eval(ordered_atleast_nvector, [reformulation(ordered_atleast_nvector_r)]).

ordered_atleast_nvector_r(0, []) :-
    !.
ordered_atleast_nvector_r(NVEC, VECTORS) :-
    eval(atleast_nvector(NVEC, VECTORS)),
    eval(lex_chain_lesseq(VECTORS)).
