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

ctr_date(lex_different,['20030820','20040530']).

ctr_origin(lex_different, 'Used for defining %c.', [lex_alldifferent]).

ctr_arguments(lex_different,
              ['VECTOR1'-collection(var-dvar),
               'VECTOR2'-collection(var-dvar)]).

ctr_exchangeable(lex_different,
                 [args([['VECTOR1','VECTOR2']]),
                  items_sync('VECTOR1','VECTOR2',all)]).

ctr_synonyms(lex_different,[different, diff]).

ctr_restrictions(lex_different,
                 [required('VECTOR1',var)          ,
                  required('VECTOR2',var)          ,
                  size('VECTOR1') > 0              ,
                  size('VECTOR1') = size('VECTOR2')]).

ctr_typical(lex_different,
            [size('VECTOR1')      > 1,
             range('VECTOR1'^var) > 1,
             range('VECTOR2'^var) > 1]).

ctr_extensible(lex_different, [], ['VECTOR1','VECTOR2'], any).

ctr_graph(lex_different,
          ['VECTOR1','VECTOR2'],
          2,
          ['PRODUCT'(=)>>collection(vector1,vector2)],
          [vector1^var =\= vector2^var],
          ['NARC' >= 1],
          []).

ctr_example(lex_different,
            lex_different([[var-5], [var-2], [var-7], [var-1]],
                          [[var-5], [var-3], [var-7], [var-1]])).

ctr_draw_example(lex_different,
                 ['VECTOR1','VECTOR2'],
                 [[[var-5], [var-2], [var-7], [var-1]],
                  [[var-5], [var-3], [var-7], [var-1]]],
                 ['PRODUCT'(=)],
                 [2-2],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,2.145,1.4,1.1]).

ctr_see_also(lex_different,
 [link('negation',              lex_equal,         '',   []),
  link('implied by',            lex_less,          '',   []),
  link('implied by',            lex_greater,       '',   []),
  link('implied by',            disjoint,          '',   []),
  link('implied by',            incomparable,      '',   []),
  link('system of constraints', lex_alldifferent,  '',   []),
  link('common keyword',        lex_greatereq,     '%k', [vector]),
  link('common keyword',        lex_lesseq,        '%k', [vector])]).

ctr_key_words(lex_different,['vector'                          ,
                             'disequality'                     ,
                             'Berge-acyclic constraint network',
                             'automaton'                       ,
                             'automaton without counters'      ,
                             'reified automaton constraint'    ,
                             'arc-consistency'                 ]).

ctr_eval(lex_different, [reformulation(lex_different_r),
                         automaton(lex_different_a)]).

lex_different_r(VECTOR1, VECTOR2) :-
    collection(VECTOR1, [dvar]),
    collection(VECTOR2, [dvar]),
    length(VECTOR1, L1),
    L1 > 0,
    length(VECTOR2, L2),
    L1 = L2,
    get_attr1(VECTOR1, VECT1),
    get_attr1(VECTOR2, VECT2),
    lex_different1(VECT1, VECT2, Term),
    call(Term).

lex_different1([], [], 0).
lex_different1([V1|R1], [V2|R2], V1#\=V2 #\/ T) :-
    lex_different1(R1, R2, T).

% 0: VAR1=\=VAR2
% 1: VAR1=VAR2
lex_different_a(FLAG, VECTOR1, VECTOR2) :-
    collection(VECTOR1, [dvar]),
    collection(VECTOR2, [dvar]),
    length(VECTOR1, L1),
    L1 > 0,
    length(VECTOR2, L2),
    L1 = L2,
    lex_different_signature(VECTOR1, VECTOR2, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,1,s),
                           arc(s,0,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [], [], []),
    automaton_bool(FLAG, [0,1], AUTOMATON).

lex_different_signature([], [], []).
lex_different_signature([[var-VAR1]|Xs], [[var-VAR2]|Ys], [S|Ss]) :-
    VAR1 #= VAR2 #<=> S,
    lex_different_signature(Xs, Ys, Ss).
