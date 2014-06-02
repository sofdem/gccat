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

:-dynamic longest_change_a/3.

ctr_date(longest_change,['20000128','20030820','20040530','20060811']).

ctr_origin(longest_change, 'Derived from %c.', [change]).

ctr_arguments(longest_change,
              ['SIZE'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ]).

ctr_exchangeable(longest_change,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(longest_change,
                 ['SIZE' >= 0                     ,
                  'SIZE' =< size('VARIABLES')     ,
                  required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(longest_change,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1,
             in_list('CTR',[=\=])      ]).

ctr_pure_functional_dependency(longest_change, []).
ctr_functional_dependency(longest_change, 1, [2,3]).

ctr_graph(longest_change,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          ['CTR'(variables1^var,variables2^var)],
          ['MAX_NCC' = 'SIZE'],
          []).

ctr_example(longest_change,
            longest_change(4,
                           [[var-8],[var-8],[var-3],[var-4],[var-1],
                            [var-1],[var-5],[var-5],[var-2]],
                           =\=)).

ctr_draw_example(longest_change,
                 ['VARIABLES'],
                 [[[var-8],[var-8],[var-3],[var-4],[var-1],
                   [var-1],[var-5],[var-5],[var-2]]],
                 ['PATH'],
                 [2-3,3-4,4-5,6-7,8-9],
                 ['MAX_NCC'([2,3,4,5])],
                 '','MAX_NCC=4',
                 [2.145,4,2.145,2.145]).

ctr_see_also(longest_change,
 [link('root concept', change, '', [])]).

ctr_key_words(longest_change,['timetabling constraint'                 ,
                              'automaton'                              ,
                              'automaton with counters'                ,
                              'reverse of a constraint'                ,
		              'glue matrix'                            ,
                              'sliding cyclic(1) constraint network(3)',
                              'functional dependency'                  ,
		              'pure functional dependency'             ]).

ctr_eval(longest_change, [  checker(longest_change_c),
			  automaton(longest_change_a)]).

longest_change_c(SIZE, VARIABLES, =) :-
    !,
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    get_attr1(VARIABLES, VARS),
    longest_change_eq_c(VARS, 0, 0, SIZE).
longest_change_c(SIZE, VARIABLES, =\=) :-
    !,
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    get_attr1(VARIABLES, VARS),
    longest_change_neq_c(VARS, 0, 0, SIZE).
longest_change_c(SIZE, VARIABLES, <) :-
    !,
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    get_attr1(VARIABLES, VARS),
    longest_change_lt_c(VARS, 0, 0, SIZE).
longest_change_c(SIZE, VARIABLES, >=) :-
    !,
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    get_attr1(VARIABLES, VARS),
    longest_change_geq_c(VARS, 0, 0, SIZE).
longest_change_c(SIZE, VARIABLES, >) :-
    !,
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    get_attr1(VARIABLES, VARS),
    longest_change_gt_c(VARS, 0, 0, SIZE).
longest_change_c(SIZE, VARIABLES, =<) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    get_attr1(VARIABLES, VARS),
    longest_change_leq_c(VARS, 0, 0, SIZE).

longest_change_eq_c([V,V|R], C, _D, SIZE) :-
   !,
   longest_change_eq_c1([V|R], C, 2, SIZE).
longest_change_eq_c([_,V|R], C, D, SIZE) :-
   !,
   longest_change_eq_c([V|R], C, D, SIZE).
longest_change_eq_c(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_eq_c1([V,V|R], C, D, SIZE) :-
   !,
   D1 is D+1,
   longest_change_eq_c1([V|R], C, D1, SIZE).
longest_change_eq_c1([_,V|R], C, D, SIZE) :-
   !,
   C1 is max(C,D),
   longest_change_eq_c1([V|R], C1, 1, SIZE).
longest_change_eq_c1(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_neq_c([V,V|R], C, D, SIZE) :-
   !,
   longest_change_neq_c([V|R], C, D, SIZE).
longest_change_neq_c([_,V|R], C, _D, SIZE) :-
   !,
   longest_change_neq_c1([V|R], C, 2, SIZE).
longest_change_neq_c(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_neq_c1([V,V|R], C, D, SIZE) :-
   !,
   C1 is max(C,D),
   longest_change_neq_c1([V|R], C1, 1, SIZE).
longest_change_neq_c1([_,V|R], C, D, SIZE) :-
   !,
   D1 is D+1,
   longest_change_neq_c1([V|R], C, D1, SIZE).
longest_change_neq_c1(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_lt_c([V1,V2|R], C, _D, SIZE) :-
   V1 < V2,
   !,
   longest_change_lt_c1([V2|R], C, 2, SIZE).
longest_change_lt_c([_,V|R], C, D, SIZE) :-
   !,
   longest_change_lt_c([V|R], C, D, SIZE).
longest_change_lt_c(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_lt_c1([V1,V2|R], C, D, SIZE) :-
   V1 < V2,
   !,
   D1 is D+1,
   longest_change_lt_c1([V2|R], C, D1, SIZE).
longest_change_lt_c1([_,V|R], C, D, SIZE) :-
   !,
   C1 is max(C,D),
   longest_change_lt_c1([V|R], C1, 1, SIZE).
longest_change_lt_c1(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_geq_c([V1,V2|R], C, _D, SIZE) :-
   V1 >= V2,
   !,
   longest_change_geq_c1([V2|R], C, 2, SIZE).
longest_change_geq_c([_,V|R], C, D, SIZE) :-
   !,
   longest_change_geq_c([V|R], C, D, SIZE).
longest_change_geq_c(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_geq_c1([V1,V2|R], C, D, SIZE) :-
   V1 >= V2,
   !,
   D1 is D+1,
   longest_change_geq_c1([V2|R], C, D1, SIZE).
longest_change_geq_c1([_,V|R], C, D, SIZE) :-
   !,
   C1 is max(C,D),
   longest_change_geq_c1([V|R], C1, 1, SIZE).
longest_change_geq_c1(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_gt_c([V1,V2|R], C, _D, SIZE) :-
   V1 > V2,
   !,
   longest_change_gt_c1([V2|R], C, 2, SIZE).
longest_change_gt_c([_,V|R], C, D, SIZE) :-
   !,
   longest_change_gt_c([V|R], C, D, SIZE).
longest_change_gt_c(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_gt_c1([V1,V2|R], C, D, SIZE) :-
   V1 > V2,
   !,
   D1 is D+1,
   longest_change_gt_c1([V2|R], C, D1, SIZE).
longest_change_gt_c1([_,V|R], C, D, SIZE) :-
   !,
   C1 is max(C,D),
   longest_change_gt_c1([V|R], C1, 1, SIZE).
longest_change_gt_c1(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_leq_c([V1,V2|R], C, _D, SIZE) :-
   V1 =< V2,
   !,
   longest_change_leq_c1([V2|R], C, 2, SIZE).
longest_change_leq_c([_,V|R], C, D, SIZE) :-
   !,
   longest_change_leq_c([V|R], C, D, SIZE).
longest_change_leq_c(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_leq_c1([V1,V2|R], C, D, SIZE) :-
   V1 =< V2,
   !,
   D1 is D+1,
   longest_change_leq_c1([V2|R], C, D1, SIZE).
longest_change_leq_c1([_,V|R], C, D, SIZE) :-
   !,
   C1 is max(C,D),
   longest_change_leq_c1([V|R], C1, 1, SIZE).
longest_change_leq_c1(_, C, D, SIZE) :-
   M is max(C,D),
   SIZE #= M.

longest_change_neq_counters_check([V,V|R], C, D, [C1|S]) :-
   !,
   C1 is max(C,D),
   longest_change_neq_counters_check([V|R], C, D, S).
longest_change_neq_counters_check([_,V|R], C, D, [C1|S]) :-
   !,
   C1 is max(C,D),
   longest_change_neq_counters_check1([V|R], C, 2, S).
longest_change_neq_counters_check(_, _, _, [0]).

longest_change_neq_counters_check1([V,V|R], C, D, [C1|S]) :-
   !,
   C1 is max(C,D),
   longest_change_neq_counters_check1([V|R], C1, 1, S).
longest_change_neq_counters_check1([_,V|R], C, D, [C1|S]) :-
   !,
   C1 is max(C,D),
   D1 is D+1,
   longest_change_neq_counters_check1([V|R], C, D1, S).
longest_change_neq_counters_check1(_, _, _, [0]).

% CTR: =
% 0: VAR1=\=VAR2
% 1: VAR1=VAR2
%
% CTR: =\=
% 0: VAR1=VAR2
% 1: VAR1=\=VAR2
%
% CTR: <
% 0: VAR1>=VAR2
% 1: VAR1<VAR2
%
% CTR: >=
% 0: VAR1<VAR2
% 1: VAR1>=VAR2
%
% CTR: >
% 0: VAR1=<VAR2
% 1: VAR1>VAR2
%
% CTR: =<
% 0: VAR1>VAR2
% 1: VAR1=<VAR2
longest_change_a(FLAG, SIZE, VARIABLES, CTR) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    longest_change_signature(VARIABLES, SIGNATURE, CTR),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(s)],
              [arc(s,0,s               ),
               arc(s,1,i,[C       ,2  ]),
               arc(i,1,i,[C       ,D+1]),
               arc(i,0,i,[max(C,D),1  ])],
              [C,D],[0,0],[C1,D1]),
    SIZE #= max(C1,D1) #<=> FLAG.

longest_change_signature([], [], _).
longest_change_signature([_], [], _) :- !.
longest_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =) :- !,
        VAR1 #= VAR2 #<=> S,
        longest_change_signature([[var-VAR2]|VARs], Ss, =).
longest_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =\=) :- !,
        VAR1 #\= VAR2 #<=> S,
        longest_change_signature([[var-VAR2]|VARs], Ss, =\=).
longest_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], <) :- !,
        VAR1 #< VAR2 #<=> S,
        longest_change_signature([[var-VAR2]|VARs], Ss, <).
longest_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], >=) :- !,
        VAR1 #>= VAR2 #<=> S,
        longest_change_signature([[var-VAR2]|VARs], Ss, >=).
longest_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], >) :- !,
        VAR1 #> VAR2 #<=> S,
        longest_change_signature([[var-VAR2]|VARs], Ss, >).
longest_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =<) :- !,
        VAR1 #=< VAR2 #<=> S,
        longest_change_signature([[var-VAR2]|VARs], Ss, =<).
