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

ctr_date(cond_lex_greater,['20060430']).

ctr_origin(cond_lex_greater, 'Inspired by \\cite{WallaceWilson06}.', []).

ctr_types(cond_lex_greater, ['TUPLE_OF_VALS'-collection(val-int)]).

ctr_arguments(cond_lex_greater,
              ['VECTOR1'-collection(var-dvar)                      ,
               'VECTOR2'-collection(var-dvar)                      ,
               'PREFERENCE_TABLE'-collection(tuple-'TUPLE_OF_VALS')]).

ctr_exchangeable(cond_lex_greater,
                 [items_sync('VECTOR1','VECTOR2','PREFERENCE_TABLE'^tuple,all),
                  vals(['VECTOR1','VECTOR2','PREFERENCE_TABLE'^tuple],int,=\=,all,dontcare)]).

ctr_restrictions(cond_lex_greater,
                 [size('TUPLE_OF_VALS') >= 1               ,
                  required('TUPLE_OF_VALS',val)            ,
                  required('VECTOR1',var)                  ,
                  required('VECTOR2',var)                  ,
                  size('VECTOR1') = size('VECTOR2')        ,
                  size('VECTOR1') = size('TUPLE_OF_VALS')  ,
                  required('PREFERENCE_TABLE',tuple)       ,
                  same_size('PREFERENCE_TABLE',tuple)      ,
                  distinct('PREFERENCE_TABLE',[])          ,
                  in_relation('VECTOR1','PREFERENCE_TABLE'),
                  in_relation('VECTOR2','PREFERENCE_TABLE')]).

ctr_typical(cond_lex_greater,
            [size('TUPLE_OF_VALS')    > 1,
             size('VECTOR1')          > 1,
             size('VECTOR2')          > 1,
             size('PREFERENCE_TABLE') > 1]).

ctr_example(cond_lex_greater,
            cond_lex_greater([[var-0], [var-0]],
                             [[var-1], [var-0]],
                             [[tuple-[[val-1], [val-0]]],
                              [tuple-[[val-0], [val-1]]],
                              [tuple-[[val-0], [val-0]]],
                              [tuple-[[val-1], [val-1]]]])).

ctr_see_also(cond_lex_greater,
 [link('implies',        cond_lex_greatereq, '',   []),
  link('common keyword', cond_lex_less,      '%k', [preferences]),
  link('common keyword', cond_lex_lesseq,    '%k', [preferences]),
  link('common keyword', cond_lex_greatereq, '%k', [preferences]),
  link('common keyword', cond_lex_cost,      '%k', [preferences]),
  link('common keyword', lex_greater,        '%k', ['lexicographic order'])]).

ctr_key_words(cond_lex_greater,['order constraint'                ,
                                'vector'                          ,
                                'lexicographic order'             ,
                                'preferences'                     ,
                                'Berge-acyclic constraint network',
                                'automaton'                       ,
                                'arc-consistency'                 ]).

ctr_persons(cond_lex_greater,['Wallace R. J.',
                              'Wilson N.'    ]).

ctr_eval(cond_lex_greater, [automata(cond_lex_greater_a)]).

cond_lex_greater_a(VECTOR1, VECTOR2, PREFERENCE_TABLE) :-
    collection(VECTOR1, [dvar]),
    collection(VECTOR2, [dvar]),
    collection(PREFERENCE_TABLE, [col([dvar])]),
    same_size(PREFERENCE_TABLE),
    PREFERENCE_TABLE = [[_-L]|_R],
    length(VECTOR1, LV1),
    length(VECTOR2, LV2),
    length(L, N),
    N >= 1,
    LV1 = LV2,
    LV1 = N,    
    create_collection(PREFERENCE_TABLE, vec, var, PREF),
    eval(lex_alldifferent(PREF)),
    eval(in_relation(VECTOR1,PREFERENCE_TABLE)),
    eval(in_relation(VECTOR2,PREFERENCE_TABLE)),
    cond_lex(VECTOR1, VECTOR2, PREFERENCE_TABLE, I, J),
    I #> J.
