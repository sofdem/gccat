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

ctr_date(lex_lesseq_allperm,['20070916']).

ctr_origin(lex_lesseq_allperm, 'Inspired by \\cite{FlenerFrischHnichKiziltanMiguelPearsonWalsh02}', []).

ctr_arguments(lex_lesseq_allperm,
              ['VECTOR1'-collection(var-dvar),
               'VECTOR2'-collection(var-dvar)]).

ctr_exchangeable(lex_lesseq_allperm,
                 [vals(['VECTOR1'^var,'VECTOR2'^var],int,=\=,all,dontcare)]).

ctr_synonyms(lex_lesseq_allperm, [leximin]).

ctr_restrictions(lex_lesseq_allperm,
                 [required('VECTOR1',var)           ,
                  required('VECTOR2',var)           ,
                  size('VECTOR1') = size('VECTOR2')]).

ctr_typical(lex_lesseq_allperm,
            [size('VECTOR1') > 1]).

ctr_contractible(lex_lesseq_allperm, [], ['VECTOR1','VECTOR2'], suffix).

ctr_predefined(lex_lesseq_allperm).

ctr_example(lex_lesseq_allperm,
            lex_lesseq_allperm([[var-1], [var-2], [var-3]],
                               [[var-3], [var-1], [var-2]])).

ctr_see_also(lex_lesseq_allperm,
 [link('system of constraints', allperm,    '',      []),
  link('implies',               lex_lesseq, '',      []),
  link('common keyword',        allperm,    '%k,%k', ['matrix symmetry', 'lexicographic order'])]).

ctr_key_words(lex_lesseq_allperm,['predefined constraint',
                                  'order constraint'     ,
                                  'vector'               ,
                                  'symmetry'             ,
                                  'matrix symmetry'      ,
                                  'lexicographic order'  ]).

ctr_persons(lex_lesseq_allperm,['Flener P.'          ,
                                'Frisch A. M.'       ,
                                'Hnich B.'           ,
                                'K{\\i}z{\\i}ltan Z.',
                                'Miguel I.'          ,
                                'Pearson J.'         ,
                                'Walsh T.'           ]).
