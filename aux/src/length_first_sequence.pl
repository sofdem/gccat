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

:-dynamic length_first_sequence_a/2.

ctr_date(length_first_sequence,['20081123']).

ctr_origin(length_first_sequence, 'Inspired by %c', [stretch_path]).

ctr_arguments(length_first_sequence,
              ['LEN'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(length_first_sequence,
                 [vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(length_first_sequence,
                 ['LEN' >= 0                ,
                  'LEN' =< size('VARIABLES'),
                  required('VARIABLES',var) ]).

ctr_typical(length_first_sequence,
            ['LEN'             < size('VARIABLES'),
             size('VARIABLES') > 1                ]).

ctr_pure_functional_dependency(length_first_sequence, []).
ctr_functional_dependency(length_first_sequence, 1, [2]).

ctr_example(length_first_sequence,
            [length_first_sequence(3, [[var-4],[var-4],[var-4],[var-5],[var-5],[var-4]]),
	     length_first_sequence(6, [[var-4],[var-4],[var-4],[var-4],[var-4],[var-4]]),
	     length_first_sequence(5, [[var-4],[var-4],[var-4],[var-4],[var-4],[var-1]])]).

ctr_see_also(length_first_sequence,
 [link('common keyword', length_last_sequence, '%k,%k', ['counting constraint', 'sequence'])]).

ctr_key_words(length_first_sequence,['value constraint'                       ,
                                     'counting constraint'                    ,
                                     'sequence'                               ,
                                     'automaton'                              ,
                                     'automaton with counters'                ,
                                     'reverse of a constraint'                ,
		                     'glue matrix'                            ,
                                     'sliding cyclic(1) constraint network(2)',
                                     'functional dependency'                  ,
		                     'pure functional dependency'             ]).

ctr_eval(length_first_sequence, [      checker(length_first_sequence_c),
				 reformulation(length_first_sequence_r),
                                     automaton(length_first_sequence_a)]).

ctr_sol(length_first_sequence,2,0,2,9,[1-6,2-3]).
ctr_sol(length_first_sequence,3,0,3,64,[1-48,2-12,3-4]).
ctr_sol(length_first_sequence,4,0,4,625,[1-500,2-100,3-20,4-5]).
ctr_sol(length_first_sequence,5,0,5,7776,[1-6480,2-1080,3-180,4-30,5-6]).
ctr_sol(length_first_sequence,6,0,6,117649,[1-100842,2-14406,3-2058,4-294,5-42,6-7]).
ctr_sol(length_first_sequence,7,0,7,2097152,[1-1835008,2-229376,3-28672,4-3584,5-448,6-56,7-8]).
ctr_sol(length_first_sequence,8,0,8,43046721,[1-38263752,2-4251528,3-472392,4-52488,5-5832,6-648,7-72,8-9]).

length_first_sequence_c(LEN, VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), LEN),
    get_attr1(VARIABLES, VARS),
    length_first_sequence_c(N, LEN, VARS).

length_first_sequence_c(0, 0, _) :- !.
length_first_sequence_c(1, 1, _) :- !.
length_first_sequence_c(_, LEN, VARS) :-
    length_first_eq_sequence(VARS, 1, LEN).

length_first_sequence_r(LEN, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    check_type(dvar(0,N), LEN),
    get_attr1(VARIABLES, VARS),
    (N = 0 -> LEN #= 0
              ;
     N = 1 -> LEN #= 1
              ;
              reverse(VARS, RVARS),
              length_first_sequence1(RVARS, _, TERM),
              call(LEN #= TERM)
    ).

length_first_sequence1([_], 1, 1) :- !.
length_first_sequence1([VAR1,VAR2|R], AND1, AND1+S) :-
    length_first_sequence1([VAR2|R], AND2, S),
    B12 #<=> VAR1 #= VAR2,
    AND1 #<=> AND2 #/\ B12.

% 0: VAR1=\=VAR2
% 1: VAR1=VAR2
length_first_sequence_a(1, 0, []) :- !.
length_first_sequence_a(0, 0, []) :- !, fail.
length_first_sequence_a(1, 1, [_]) :- !.
length_first_sequence_a(0, 1, [_]) :- !, fail.
length_first_sequence_a(FLAG, LEN, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    check_type(dvar(0,N), LEN),
    length_first_sequence_signature(VARIABLES, SIGNATURE),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s),sink(t)],
              [arc(s,0,t      ),
               arc(s,1,s,[C+1]),
               arc(t,0,t      ),
               arc(t,1,t      )],
              [C],[1],[COUNT]),
    COUNT #= LEN #<=> FLAG.

length_first_sequence_signature([], []).
length_first_sequence_signature([_], []) :- !.
length_first_sequence_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss]) :-
        S in 0..1,
        VAR1 #= VAR2 #<=> S,
        length_first_sequence_signature([[var-VAR2]|VARs], Ss).
