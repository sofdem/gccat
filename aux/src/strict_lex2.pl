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

ctr_date(strict_lex2,['20031016','20060817']).

ctr_origin(strict_lex2, '\\cite{FlenerFrischHnichKiziltanMiguelPearsonWalsh02}', []).

ctr_types(strict_lex2,
          ['VECTOR'-collection(var-dvar)]).

ctr_arguments(strict_lex2,
              ['MATRIX'-collection(vec-'VECTOR')]).

ctr_exchangeable(strict_lex2,
                 [translate(['MATRIX'^vec^var])]).

ctr_restrictions(strict_lex2,
                 [size('VECTOR') >= 1    ,
                  required('VECTOR',var) ,
                  required('MATRIX',vec) ,
                  same_size('MATRIX',vec)]).

ctr_typical(strict_lex2,
            [size('VECTOR') > 1,
             size('MATRIX') > 1]).

ctr_predefined(strict_lex2).

ctr_example(strict_lex2,
            strict_lex2([[vec-[[var-2], [var-2], [var-3]]],
                         [vec-[[var-2], [var-3], [var-1]]]])).

ctr_see_also(strict_lex2,
 [link('part of system of constraints', lex_chain_less, '',   []),
  link('implies',                       lex2,           '',   []),
  link('implies',                       lex_chain_less, '',   []),
  link('common keyword',                allperm,        '%k', ['lexicographic order']),
  link('common keyword',                lex_lesseq,     '%k', ['lexicographic order'])]).

ctr_key_words(strict_lex2,['predefined constraint',
                           'system of constraints',
                           'order constraint'     ,
                           'matrix'               ,
                           'matrix model'         ,
                           'symmetry'             ,
                           'matrix symmetry'      ,
                           'lexicographic order'  ]).

ctr_persons(strict_lex2,['Flener P.'          ,
                         'Frisch A. M.'       ,
                         'Hnich B.'           ,
                         'K{\\i}z{\\i}ltan Z.',
                         'Miguel I.'          ,
                         'Pearson J.'         ,
                         'Walsh T.'           ]).

ctr_eval(strict_lex2, [checker(strict_lex2_c)      ,
		       reformulation(strict_lex2_r)]).

strict_lex2_c(MATRIX) :-
    collection(MATRIX, [col([int])]),
    same_size(MATRIX),
    get_attr11(MATRIX, MAT),
    lex_chain_less_c1(MAT),
    transpose(MAT, TMAT),
    lex_chain_less_c1(TMAT).

strict_lex2_r(MATRIX) :-
    collection(MATRIX, [col([dvar])]),
    same_size(MATRIX),
    get_attr11(MATRIX, MAT),
    lex_chain(MAT, [op(#<)]),
    transpose(MAT, TMAT),
    lex_chain(TMAT, [op(#<)]).
