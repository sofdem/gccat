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

ctr_date(lex_chain_greatereq,['20130730']).

ctr_origin(lex_chain_greatereq, 'Derived from %c', [lex_chain_lesseq]).

ctr_usual_name(lex_chain_greatereq, lex_chain).

ctr_types(lex_chain_greatereq,
          ['VECTOR'-collection(var-dvar)]).

ctr_arguments(lex_chain_greatereq,
              ['VECTORS'-collection(vec-'VECTOR')]).

ctr_restrictions(lex_chain_greatereq,
                 [size('VECTOR') >= 1     ,
                  required('VECTOR',var)  ,
                  required('VECTORS',vec) ,
                  same_size('VECTORS',vec)]).

ctr_typical(lex_chain_greatereq,
            [size('VECTOR')  > 1,
             size('VECTORS') > 1]).

ctr_contractible(lex_chain_greatereq, [], 'VECTORS', any).
ctr_contractible(lex_chain_greatereq, [], 'VECTORS'^vec, suffix).

ctr_graph(lex_chain_greatereq,
          ['VECTORS'],
          2,
          ['PATH'>>collection(vectors1,vectors2)],
          [lex_lesseq(vectors1^vec,vectors2^vec)],
          ['NARC' = size('VECTORS')-1],
          []).

ctr_example(lex_chain_greatereq,
            lex_chain_greatereq([[vec-[[var-5], [var-2], [var-6], [var-2]]],
                                 [vec-[[var-5], [var-2], [var-6], [var-2]]],
				 [vec-[[var-5], [var-2], [var-3], [var-9]]]])).

ctr_draw_example(lex_chain_greatereq,
                 ['VECTORS'],
                 [[[vec-[[var-5], [var-2], [var-6], [var-2]]],
                   [vec-[[var-5], [var-2], [var-6], [var-2]]],
		   [vec-[[var-5], [var-2], [var-3], [var-9]]]]],
                 ['PATH'],
                 [1-2,2-3],
                 ['NARC'],
                 '','NARC=2',
                 [1.4,2.145,3,3]).

ctr_see_also(lex_chain_greatereq,
 [link('part of system of constraints', lex_greatereq,           '',                                         []),
  link('used in graph description',     lex_greatereq,           '',                                         []),
  link('implied by',                    lex_chain_greater,       'non-strict order implied by strict order', []),
  link('common keyword',                lex_between,             '%k',                                       ['lexicographic order']),
  link('common keyword',                lex_greater,             '%k',                                       ['lexicographic order']),
  link('common keyword',                lex_less,                '%k',                                       ['lexicographic order']),
  link('common keyword',                lex_lesseq,              '%k',                                       ['lexicographic order'])]).

ctr_key_words(lex_chain_greatereq,['system of constraints'                  ,
                                   'decomposition'                          ,
                                   'order constraint'                       ,
                                   'vector'                                 ,
                                   'symmetry'                               ,
                                   'matrix symmetry'                        ,
                                   'lexicographic order'                    ,
                                   'arc-consistency'                        ,
                                   'heuristics and lexicographical ordering']).

ctr_eval(lex_chain_greatereq, [checker(lex_chain_greatereq_c),
			       builtin(lex_chain_greatereq_b)]).

lex_chain_greatereq_c(VECTORS) :-
    collection(VECTORS, [col([int])]),
    same_size(VECTORS),
    get_attr11(VECTORS, VECTS),
    reverse(VECTS, RVECTS),
    lex_chain_lesseq_c1(RVECTS).

lex_chain_greatereq_b(VECTORS) :-
    collection(VECTORS, [col([dvar])]),
    same_size(VECTORS),
    get_attr11(VECTORS, VECTS),
    reverse(VECTS, RVECTS),
    lex_chain(RVECTS, [op(#=<)]).
