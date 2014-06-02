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

ctr_date(power,['20070930']).

ctr_origin(power, '\\cite{DenmatGotliebDucasse07}', []).

ctr_arguments(power,
              ['X'-dvar,
               'N'-dvar,
               'Y'-dvar]).

ctr_synonyms(power,[xexpyeqz]).

ctr_restrictions(power,
                 ['X' >= 0,
                  'N' >= 0,
                  'Y' >= 0]).

ctr_typical(power,
            ['X' > 1,
             'N' > 1,
	     'N' < 5,
             'Y' > 1]).

ctr_pure_functional_dependency(power, []).
ctr_functional_dependency(power, 3, [1,2]).

ctr_predefined(power).

ctr_example(power,
            power(2, 3, 8)).

ctr_see_also(power,
 [link('common keyword', gcd, '%k', ['abstract interpretation'])]).

ctr_key_words(power,['arithmetic constraint'     ,
                     'predefined constraint'     ,
                     'ternary constraint'        ,
                     'functional dependency'     ,
		     'pure functional dependency',
                     'abstract interpretation'   ]).

ctr_persons(power,['Denmat T.'     ,
                   'Gotlieb A.'    ,
                   'Ducass\\\'e M.']).

ctr_eval(power, [checker(power_c)      ,
		 reformulation(power_r)]).

power_c(X, 0, Y) :- !,
    check_type(int_gteq(0), X),
    Y = 1.
power_c(X, N, Y) :-
    check_type(int_gteq(0), X),
    check_type(int_gteq(0), N),
    check_type(int_gteq(0), Y),
    power_c(N, X, X, Y).

power_c(1, Cur, _, Y) :- !,
    Cur = Y.
power_c(N, Cur, X, Y) :-
    Next is Cur*X,
    N1 is N-1,
    power_c(N1, Next, X, Y).

power_r(X, 0, Y) :- !,
    check_type(dvar_gteq(0), X),
    Y = 1.
power_r(X, N, Y) :-
    check_type(dvar_gteq(0), X),
    check_type(dvar_gteq(0), N),
    check_type(dvar_gteq(0), Y),
    fd_min(N, Min),
    fd_max(N, Max),
    Min1 is max(1, Min),
    power1(0, Min1, Max, 1, X, Y, N, Disj),
    call(Disj).

power1(I, _, Max, _, _, _, _, 0) :-
    I > Max, !.
power1(I, Min, Max, P, X, Y, N, R) :-
    I < Min, !,
    I1 is I+1,
    power1(I1, Min, Max, P*X, X, Y, N, R).
power1(I, Min, Max, P, X, Y, N, (P #= Y #/\ N #= I) #\/ R) :-
    I >= Min,
    I =< Max,
    I1 is I+1,
    power1(I1, Min, Max, P*X, X, Y, N, R).
