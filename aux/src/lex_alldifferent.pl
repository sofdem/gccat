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

ctr_date(lex_alldifferent,['20030820','20040530','20051008','20060811','20111102']).

ctr_origin(lex_alldifferent, 'J.~Pearson', []).

ctr_types(lex_alldifferent,
          ['VECTOR'-collection(var-dvar)]).

ctr_synonyms(lex_alldifferent,[lex_alldiff           ,
                               lex_alldistinct       ,
                               alldiff_on_tuples     ,
                               alldifferent_on_tuples,
                               alldistinct_on_tuples ]).

ctr_arguments(lex_alldifferent,
              ['VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(lex_alldifferent,
                 [items('VECTORS',all),
                  items_sync('VECTORS'^vec,all),
                  vals(['VECTORS'^vec],int,=\=,all,dontcare)]).

ctr_restrictions(lex_alldifferent,
                 [size('VECTOR') >= 1     ,
                  required('VECTOR',var)  ,
                  required('VECTORS',vec) ,
                  same_size('VECTORS',vec)]).

ctr_typical(lex_alldifferent,
            [size('VECTOR')  > 1,
             size('VECTORS') > 1]).

ctr_contractible(lex_alldifferent, [], 'VECTORS', any).

ctr_extensible(lex_alldifferent, [], 'VECTORS'^vec, any).

ctr_graph(lex_alldifferent,
          ['VECTORS'],
          2,
          ['CLIQUE'(<)>>collection(vectors1,vectors2)],
          [lex_different(vectors1^vec,vectors2^vec)],
          ['NARC' = (size('VECTORS')*(size('VECTORS')-1)) / 2],
          []).

ctr_example(lex_alldifferent,
            lex_alldifferent([[vec-[[var-5], [var-2], [var-3]]],
                              [vec-[[var-5], [var-2], [var-6]]],
                              [vec-[[var-5], [var-3], [var-3]]]])).

ctr_draw_example(lex_alldifferent,
                 ['VECTORS'],
                 [[[vec-[[var-5], [var-2], [var-3]]],
                   [vec-[[var-5], [var-2], [var-6]]],
                   [vec-[[var-5], [var-3], [var-3]]]]],
                 ['CLIQUE'(<)],
                 [1-[2,3], 2-3],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.4,2.1]).

ctr_see_also(lex_alldifferent,
 [link('implied by',                    all_incomparable,          '',                  []),
  link('implied by',                    lex_chain_greater,         '',                  []),
  link('implied by',                    lex_chain_less,            '',                  []),
  link('implies',                       lex_alldifferent_except_0, '',                  []),
  link('generalisation',                diffn,                     '%e replaced by %e', [vector, orthotope]),
  link('generalisation',                geost,                     '%e replaced by %e', [vector, object]),
  link('specialisation',                alldifferent,              '%e replaced by %e', [vector, variable]),
  link('part of system of constraints', lex_different,             '',                  []),
  link('used in graph description',     lex_different,             '',                  [])]).

ctr_key_words(lex_alldifferent,['system of constraints'                ,
                                'decomposition'                        ,
                                'vector'                               ,
                                'bipartite matching'                   ,
                                'arc-consistency'                      ,
                                'difference between pairs of variables']).

ctr_persons(lex_alldifferent,['Pearson J.'   ,
                              'Quimper C.-G.',
                              'Walsh T.'     ,
                              'Beldiceanu N.',
                              'Carlsson M.'  ,
                              'Poder E.'     ,
                              'Sadek R.'     ,
                              'Truchet C.'   ]).

ctr_eval(lex_alldifferent, [checker(lex_alldifferent_c),
                            reformulation(lex_alldifferent_r),
			    density(lex_alldifferent_d)]).

% sort (which remove duplicates) and compare length
lex_alldifferent_c([]) :- !.
lex_alldifferent_c(VECTORS) :-
    collection(VECTORS, [col([int])]),
    same_size(VECTORS),
    length(VECTORS, L),
    sort(VECTORS, SVECTORS),
    length(SVECTORS, L).

lex_alldifferent_r([]) :- !.
lex_alldifferent_r(VECTORS) :-
    collection(VECTORS, [col([dvar])]),
    lex_alldifferent1(VECTORS).

lex_alldifferent1([]).
lex_alldifferent1([[_-VECTOR]|R]) :-
    lex_alldifferent2(R, VECTOR),
    lex_alldifferent1(R).

lex_alldifferent2([], _).
lex_alldifferent2([[_-VECTORi]|R], VECTOR) :-
    eval(lex_different(VECTOR, VECTORi)),
    lex_alldifferent2(R, VECTOR).

lex_alldifferent_d(Density, VECTORS) :-
    lex_alldifferent_density(Density, VECTORS).
