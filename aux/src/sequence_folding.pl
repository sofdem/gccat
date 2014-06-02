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

ctr_date(sequence_folding,['20030820','20040530','20060814']).

ctr_origin(sequence_folding, 'J.~Pearson', []).

ctr_arguments(sequence_folding,
              ['LETTERS'-collection(index-int, next-dvar)]).

ctr_restrictions(sequence_folding,
                 [size('LETTERS') >= 1              ,
                  required('LETTERS',[index,next])  ,
                  'LETTERS'^index >= 1              ,
                  'LETTERS'^index =< size('LETTERS'),
                  increasing_seq('LETTERS',index)   ,
                  'LETTERS'^next >= 1               ,
                  'LETTERS'^next =< size('LETTERS') ]).

ctr_typical(sequence_folding,
            [size('LETTERS')       > 2,
             range('LETTERS'^next) > 1]).

ctr_graph(sequence_folding,
          ['LETTERS'],
          1,
          ['SELF'>>collection(letters)],
          [letters^next >= letters^index],
          ['NARC' = size('LETTERS')],
          []).

ctr_graph(sequence_folding,
          ['LETTERS'],
          2,
          ['CLIQUE'(<)>>collection(letters1,letters2)],
          [letters2^index >= letters1^next #\/ letters2^next =< letters1^next],
          ['NARC' = (size('LETTERS')*(size('LETTERS')-1)) / 2],
          []).

ctr_example(sequence_folding,
            sequence_folding([[index-1, next-1],
                              [index-2, next-8],
                              [index-3, next-3],
                              [index-4, next-5],
                              [index-5, next-5],
                              [index-6, next-7],
                              [index-7, next-7],
                              [index-8, next-8],
                              [index-9, next-9]])).

ctr_draw_example(sequence_folding,
                 ['LETTERS'],
                 [[[index-1, next-1],
                   [index-2, next-8],
                   [index-3, next-3],
                   [index-4, next-5],
                   [index-5, next-5],
                   [index-6, next-7],
                   [index-7, next-7],
                   [index-8, next-8],
                   [index-9, next-9]]],
                 ['CLIQUE'(<)],
                 [1-[2,3,4,5,6,7,8,9],
                  2-[3,4,5,6,7,8,9],
                  3-[4,5,6,7,8,9],
                  4-[5,6,7,8,9],
                  5-[6,7,8,9],
                  6-[7,8,9],
                  7-[8,9],
                  8-9],
                 ['NARC'],
                 '','NARC=36',
                 [1.4,3.3,3.3,3.3]).

ctr_see_also(sequence_folding,
 [link('implies (items to collection)', lex_alldifferent, '', []),
  link('implies (items to collection)', lex_chain_less,   '', [])]).

ctr_key_words(sequence_folding,['decomposition'               ,
                                'geometrical constraint'      ,
                                'sequence'                    ,
                                'bioinformatics'              ,
                                'automaton'                   ,
                                'automaton without counters'  ,
                                'reified automaton constraint']).

ctr_persons(sequence_folding,['Pearson J.'    ,
                              'Flamm C.'      ,
                              'Hofacker I. L.',
                              'Stadler P. F.' ]).

ctr_application(sequence_folding, [1]).

ctr_eval(sequence_folding, [automaton(sequence_folding_a)]).

% 0: INDEX1=<NEXT1 and INDEX2=<NEXT2 and NEXT1=<INDEX2
% 1: INDEX1=<NEXT1 and INDEX2=<NEXT2 and NEXT1 >INDEX2 and NEXT2=<NEXT1
% 2: otherwise
sequence_folding_a(FLAG, LETTERS) :-
    length(LETTERS, N),
    N >= 1,
    collection(LETTERS, [int(1,N), dvar(1,N)]),
    collection_increasing_seq(LETTERS,[1]),
    sequence_folding_signature(LETTERS, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,0,s),
                           arc(s,1,s)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

sequence_folding_signature([], []).
sequence_folding_signature([_], []) :- !.
sequence_folding_signature([L1,L2|R], S) :-
    sequence_folding_signature([L2|R], L1, S1),
    sequence_folding_signature([L2|R], S2),
    append(S1, S2, S).

sequence_folding_signature([], _, []).
sequence_folding_signature([L2|R], L1, [S|Ss]) :-
    L1 = [index-INDEX1,next-NEXT1],
    L2 = [index-INDEX2,next-NEXT2],
    INDEX1#=<NEXT1 #/\ INDEX2#=<NEXT2 #/\ NEXT1#=<INDEX2 #<=> S #= 0,
    INDEX1#=<NEXT1 #/\ INDEX2#=<NEXT2 #/\ NEXT1#>INDEX2 #/\ NEXT2#=<NEXT1 #<=> S #= 1,
    sequence_folding_signature(R, L1, Ss).
