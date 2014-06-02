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

:-dynamic ith_pos_different_from_0_a/3.

ctr_date(ith_pos_different_from_0,['20040811']).

ctr_origin(ith_pos_different_from_0, 'N.~Beldiceanu', []).

ctr_arguments(ith_pos_different_from_0,
              ['ITH'-int                       ,
               'POS'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(ith_pos_different_from_0,
                 [vals(['VARIABLES'^var],int(=\=(0)),=\=,dontcare,dontcare)]).

ctr_restrictions(ith_pos_different_from_0,
                 ['ITH' >= 1                ,
                  'ITH' =< size('VARIABLES'),
                  'POS' >= 'ITH'            ,
                  'POS' =< size('VARIABLES'),
                  required('VARIABLES',var) ]).

ctr_typical(ith_pos_different_from_0,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1,
             atleast(1,'VARIABLES',0)  ]).

ctr_extensible(ith_pos_different_from_0, [], 'VARIABLES', suffix).

ctr_example(ith_pos_different_from_0,
            ith_pos_different_from_0(2, 4, [[var-3],[var-0],[var-0],[var-8],[var-6]])).

ctr_key_words(ith_pos_different_from_0,['data constraint'                    ,
                                        'table'                              ,
                                        'joker value'                        ,
                                        'automaton'                          ,
                                        'automaton with counters'            ,
                                        'alpha-acyclic constraint network(3)']).

ctr_persons(ith_pos_different_from_0,['Beldiceanu N.']).

ctr_eval(ith_pos_different_from_0, [automaton(ith_pos_different_from_0_a)]).

% 0: VAR<>0
% 1: VAR=0
ith_pos_different_from_0_a(FLAG, ITH, POS, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    integer(ITH),
    ITH >= 1,
    ITH =< N,
    check_type(dvar(ITH,N), POS),
    ith_pos_different_from_0_signature(VARIABLES, SIGNATURE),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s,(C#<ITH -> [C+1,D+1] ; C#>=ITH -> [C,D])),
               arc(s,1,s,(C#<ITH -> [C  ,D+1] ; C#>=ITH -> [C,D]))],
              [C,D],[0,0],[C1,D1]),
   C1 #= ITH #/\ D1 #= POS #<=> FLAG.

ith_pos_different_from_0_signature([], []).
ith_pos_different_from_0_signature([[var-V]|VARs], [S|Ss]) :-
    V #= 0 #<=> S,
    ith_pos_different_from_0_signature(VARs, Ss).
