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

ctr_date(allperm,['20031008', '20070916']).

ctr_origin(allperm, '\\cite{FlenerFrischHnichKiziltanMiguelPearsonWalsh02}', []).

ctr_types(allperm,
          ['VECTOR'-collection(var-dvar)]).

ctr_arguments(allperm,
              ['MATRIX'-collection(vec-'VECTOR')]).

ctr_exchangeable(allperm,
                 [translate(['MATRIX'^vec^var])]).

ctr_synonyms(allperm,[all_perm,all_permutations]).

ctr_restrictions(allperm,
                 [size('VECTOR') >= 1    ,
                  required('VECTOR',var) ,
                  required('MATRIX',vec) ,
                  same_size('MATRIX',vec)]).

ctr_typical(allperm,
            [size('VECTOR') > 1,
             size('MATRIX') > 1]).

ctr_contractible(allperm, [], 'MATRIX'^vec, suffix).

ctr_graph(allperm,
          ['MATRIX'],
          2,
          ['CLIQUE'(<)>>collection(matrix1,matrix2)],
          [matrix1^key=1,
           matrix2^key>1,
           lex_lesseq_allperm(matrix1^vec,matrix2^vec)],
          ['NARC' = size('MATRIX')-1],
          ['ACYCLIC','BIPARTITE','NO_LOOP']).


ctr_example(allperm,
            allperm([[vec-[[var-1], [var-2], [var-3]]],
                     [vec-[[var-3], [var-1], [var-2]]]])).

ctr_draw_example(allperm,
                 ['MATRIX'],
                 [[[vec-[[var-1], [var-2], [var-3]]],
                   [vec-[[var-3], [var-1], [var-2]]]]],
                 ['CLIQUE'(<)],
                 [1-2],
                 ['NARC'],
                 '','NARC=1',
                 [1.7,1.7,1.7,1.7]).

ctr_see_also(allperm,
 [link('common keyword',                lex2,                '%k,%k', ['matrix symmetry', 'lexicographic order']),
  link('common keyword',                lex_lesseq_allperm,  '%k,%k', ['matrix symmetry', 'lexicographic order']),
  link('common keyword',                lex_chain_lesseq,    '%k,%k', ['matrix symmetry', 'lexicographic order']),
  link('common keyword',                strict_lex2,         '%k',    ['lexicographic order']),
  link('common keyword',                lex_lesseq,          '%k',    ['lexicographic order']),
  link('part of system of constraints', lex_lesseq_allperm,  '',      []),
  link('used in graph description',     lex_lesseq_allperm,  '',      [])]).

ctr_key_words(allperm,['order constraint'        ,
                       'system of constraints'   ,
                       'sort based reformulation',
                       'matrix'                  ,
                       'matrix model'            ,
                       'matrix symmetry'         ,
                       'vector'                  ,
                       'symmetry'                ,
                       'lexicographic order'     ,
                       'acyclic'                 ,
                       'bipartite'               ]).

ctr_persons(allperm,['Flener P.'          ,
                     'Frisch A. M.'       ,
                     'Hnich B.'           ,
                     'K{\\i}z{\\i}ltan Z.',
                     'Miguel I.'          ,
                     'Pearson J.'         ,
                     'Walsh T.'           ]).

ctr_eval(allperm, [checker(allperm_c)      ,
		   reformulation(allperm_r)]).

allperm_c(MATRIX) :-
    collection(MATRIX, [col([int])]),
    same_size(MATRIX),
    get_attr11(MATRIX, MAT),
    MAT = [FIRST_ROW|R],
    allperm_c1(R, FIRST_ROW).

allperm_c1([], _) :- !.
allperm_c1([CUR_ROW|R], FIRST_ROW) :-
    create_pairs(CUR_ROW, PAIRS),
    keysort(PAIRS, SORTED_ROW),
    remove_key_from_collection(SORTED_ROW, SORTED),
    lex_lesseq_c1(FIRST_ROW, SORTED),
    allperm_c1(R, FIRST_ROW).

allperm_r(MATRIX) :-
    collection(MATRIX, [col([dvar])]),
    same_size(MATRIX),
    MATRIX = [[vec-F]|R],
    allperm_sorted(R, S),
    allperm_order(S, F).

allperm_sorted([], []).
allperm_sorted([[vec-X]|R], [S|T]) :-
    get_attr1(X, L),
    get_minimum(L, MIN),
    get_maximum(L, MAX),
    length(X, LX),
    length(Y, LX),
    domain(Y, MIN, MAX),
    gen_collection(Y, var, S),
    eval(sort(X,S)),
    allperm_sorted(R, T).

allperm_order([], _).
allperm_order([X|R], F) :-
    eval(lex_lesseq(F,X)),
    allperm_order(R, F).
