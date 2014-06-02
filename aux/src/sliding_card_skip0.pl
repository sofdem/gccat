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

:-dynamic sliding_card_skip0_a/4.

ctr_date(sliding_card_skip0,['20000128','20030820','20040530','20060815']).

ctr_origin(sliding_card_skip0, 'N.~Beldiceanu', []).

ctr_arguments(sliding_card_skip0,
              ['ATLEAST'-int                   ,
               'ATMOST'-int                    ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int )   ]).

ctr_exchangeable(sliding_card_skip0,
                 [vals(['ATLEAST'],int(>=(0)),>,dontcare,dontcare),
                  vals(['ATMOST'],int(=<(size('VARIABLES'))),<,dontcare,dontcare),
                  items('VARIABLES',reverse),
                  vals(['VARIABLES'^var],comp_diff('VALUES'^val,=\=(0)),=,dontcare,dontcare)]).

ctr_restrictions(sliding_card_skip0,
                 ['ATLEAST' >= 0                ,
                  'ATLEAST' =< size('VARIABLES'),
                  'ATMOST'  >= 0                ,
                  'ATMOST'  =< size('VARIABLES'),
                  'ATMOST'  >= 'ATLEAST'        ,
                  required('VARIABLES',var)     ,
                  required('VALUES',val)        ,
                  distinct('VALUES',val)        ,
                  'VALUES'^val =\= 0            ]).

ctr_typical(sliding_card_skip0,
            [size('VARIABLES') > 1                         ,
             size('VALUES')    > 0                         ,
             size('VARIABLES') > size('VALUES')            ,
             atleast(1,'VARIABLES',0)                      ,
             'ATLEAST' > 0 #\/ 'ATMOST' < size('VARIABLES')]).

ctr_graph(sliding_card_skip0,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2),
           'LOOP'>>collection(variables1,variables2)],
          [variables1^var =\= 0,
           variables2^var =\= 0],
          [],
          [],
          ['CC'>>[variables]],
          [among_low_up('ATLEAST','ATMOST',variables,'VALUES')]).

ctr_example(sliding_card_skip0,
            sliding_card_skip0(2,
                               3,
                               [[var-0],[var-7],[var-2],[var-9],[var-0],
                                [var-0],[var-9],[var-4],[var-9]],
                               [[val-7],[val-9]])).

ctr_draw_example(sliding_card_skip0,
                 ['VARIABLES'],
                 [[[var-0],[var-7],[var-2],[var-9],[var-0],
                   [var-0],[var-9],[var-4],[var-9]]],
                 ['PATH','LOOP'],
                 [2-[2,3],
                  3-[3,4],
                  4-4,
                  7-[7,8],
                  8-[8,9],
                  9-9],
                 ['SET'([[2,3,4],[7,8,9]])],
                 '','',
                 [2.4,3.8,1.6,1.6]).

ctr_see_also(sliding_card_skip0,
 [link('specialisation', among_low_up,       'maximal sequences replaced by the full sequence', []),
  link('related',        among,              '%k on the full sequence',                         ['counting constraint']),
  link('related',        global_cardinality, '%k for different values on the full sequence',    ['counting constraint'])]).

ctr_key_words(sliding_card_skip0,['timetabling constraint'             ,
                                  'sliding sequence constraint'        ,
                                  'sequence'                           ,
                                  'automaton'                          ,
                                  'automaton with counters'            ,
                                  'alpha-acyclic constraint network(2)']).

ctr_persons(sliding_card_skip0,['Beldiceanu N.']).

ctr_eval(sliding_card_skip0, [automaton(sliding_card_skip0_a)]).

% 0: VAR=0
% 1: VAR=\=0 and not_in(VAR,VALUES)
% 2: VAR=\=0 and in(VAR,VALUES)
sliding_card_skip0_a(FLAG, ATLEAST, ATMOST, VARIABLES, VALUES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    check_type(int(0,N), ATLEAST),
    check_type(int(0,N), ATMOST),
    ATMOST >= ATLEAST,
    collection(VALUES, [int_diff(0)]),
    get_attr1(VALUES, LIST_VALUES),
    all_different(LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    sliding_card_skip0_signature(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(t),sink(s)],
              [arc(s,0,s),
               arc(s,1,t,[0,L,U]),
               arc(s,2,t,[1,L,U]),
               arc(t,0,s,[C,min(L,C),max(U,C)]),
               arc(t,1,t),
               arc(t,2,t,[C+1,L,U])],
              [C,L,U],[ATLEAST,ATLEAST,ATMOST],[C1,L1,U1]),
    min(C1,L1) #>= ATLEAST #/\ max(C1,U1) #=< ATMOST #<=> FLAG.

sliding_card_skip0_signature([], [], _).
sliding_card_skip0_signature([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR #\= 0                #<=> NZ,
    VAR in_set SET_OF_VALUES #<=> In,
    S in 0..2,
    S #= max(2*NZ + In - 1, 0),
    sliding_card_skip0_signature(VARs, Ss, SET_OF_VALUES).
