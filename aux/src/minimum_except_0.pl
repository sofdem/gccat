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

:-dynamic minimum_except_0_a/4.

ctr_date(minimum_except_0,['20030820','20040530','20041230','20060812','20090101']).

ctr_origin(minimum_except_0, 'Derived from %c.', [minimum]).

ctr_arguments(minimum_except_0,
              ['MIN'-dvar                      ,
               'VARIABLES'-collection(var-dvar),
               'DEFAULT'-int                   ]).

ctr_exchangeable(minimum_except_0,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in)]).

ctr_restrictions(minimum_except_0,
                 ['MIN'             >  0        ,
                  'MIN'             =< 'DEFAULT',
                  size('VARIABLES') >  0        ,
                  required('VARIABLES',var)     ,
                  'VARIABLES'^var   >= 0        ,
                  'VARIABLES'^var   =< 'DEFAULT',
                  'DEFAULT'         >  0        ]).

ctr_typical(minimum_except_0,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1,
             atleast(1,'VARIABLES',0)  ]).

ctr_pure_functional_dependency(minimum_except_0, []).
ctr_functional_dependency(minimum_except_0, 1, [2,3]).

ctr_graph(minimum_except_0,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var =\= 0,
           variables2^var =\= 0,
           variables1^key = variables2^key #\/ variables1^var < variables2^var],
          ['ORDER'(0,'DEFAULT',var) = 'MIN'],
          []).

ctr_example(minimum_except_0,
    [minimum_except_0(      3, [[var-3],[var-7],[var-6],[var-7],[var-4],[var-7]], 1000000),
     minimum_except_0(      2, [[var-3],[var-2],[var-0],[var-7],[var-2],[var-6]], 1000000),
     minimum_except_0(1000000, [[var-0],[var-0],[var-0],[var-0],[var-0],[var-0]], 1000000)]).

ctr_draw_example(minimum_except_0,
                 ['VARIABLES'],
                 [[[var-3],[var-2],[var-0],[var-7],[var-2],[var-6]]],
                 ['CLIQUE'],
                 [1-[1,4,6],
                  2-[1,2,4,6],
                  4-4,
                  5-[1,4,5,6],
                  6-[4,6]],
                 ['ORDER'([2,5])],
                 '','ORDER(0,DEFAULT,var)=2',
                 []).

ctr_cond_imply(minimum_except_0, atmost, [maxval('VARIABLES'^var) < 'DEFAULT'], [], id).

ctr_see_also(minimum_except_0,
 [link('hard version', minimum, 'value %e is not ignored any more', [0])]).

ctr_key_words(minimum_except_0,['order constraint'                        ,
                                'joker value'                             ,
                                'minimum'                                 ,
                                'automaton'                               ,
                                'automaton without counters'              ,
                                'reified automaton constraint'            ,
                                'centered cyclic(1) constraint network(1)',
                                'functional dependency'                   ,
		                'pure functional dependency'              ]).

ctr_eval(minimum_except_0, [checker(minimum_except_0_c),
			    reformulation(minimum_except_0_r),
                            automaton(minimum_except_0_a)]).

minimum_except_0_c(MIN, VARIABLES, DEFAULT) :-
    check_type(int_gteq(1), DEFAULT),
    check_type(dvar(1,DEFAULT), MIN),
    collection(VARIABLES, [int(0,DEFAULT)]),
    length(VARIABLES, N),
    N > 0,
    get_attr1(VARIABLES, VARS),
    minimum_except_0_c(VARS, 1, DEFAULT, DEFAULT, MIN).

minimum_except_0_c([V|R], AllZero, Min, DEFAULT, RESULT) :-
    !,
    V >= 0,
    V =< DEFAULT,
    (V > 0 ->
	NextAllZero = 0,
	NextMin is min(Min,V)
    ;
	NextAllZero = AllZero,
	NextMin = Min),
    minimum_except_0_c(R, NextAllZero, NextMin, DEFAULT, RESULT).
minimum_except_0_c([], 1, _, RESULT, RESULT) :- !.
minimum_except_0_c([], 0, RESULT, _, RESULT).

minimum_except_0_r(MIN, VARIABLES, DEFAULT) :-
    check_type(int_gteq(1), DEFAULT),
    check_type(dvar(1,DEFAULT), MIN),
    collection(VARIABLES, [dvar(0,DEFAULT)]),
    length(VARIABLES, N),
    N > 0,
    get_attr1(VARIABLES, VARS),
    minimum_except_01(VARS, ALLZEROS),
    call(ALLZEROS #=> MIN #= DEFAULT),
    append([0], VARS, VARS0),
    N1 is N+1,
    length(RANKS, N1),
    domain(RANKS, 0, N),
    min_n1(VARS0, RANKS, MIN, 1).

minimum_except_01([], 1).
minimum_except_01([V|R], V#=0 #/\ S) :-
    minimum_except_01(R, S).

% 0: VAR = 0 and MIN=\=DEFAULT
% 1: VAR = 0 and MIN = DEFAULT
% 2: VAR=\=0 and MIN = VAR
% 3: VAR=\=0 and MIN < VAR
% 4: VAR=\=0 and MIN > VAR
minimum_except_0_a(FLAG, MIN, VARIABLES, DEFAULT) :-
    check_type(int_gteq(1), DEFAULT),
    check_type(dvar(1,DEFAULT), MIN),
    collection(VARIABLES, [dvar(0,DEFAULT)]),
    length(VARIABLES, N),
    N > 0,
    minimum_except_0_signature(VARIABLES, SIGNATURE, MIN, DEFAULT),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(j),sink(k)],
                          [arc(s,0,s),
                           arc(s,3,s),
                           arc(s,2,j),
                           arc(s,1,k),
                           arc(j,0,j),
                           arc(j,1,j),
                           arc(j,2,j),
                           arc(j,3,j),
                           arc(k,1,k)],
                           [],[],[]),
    automaton_bool(FLAG, [0,1,2,3,4], AUTOMATON).

minimum_except_0_signature([], [], _, _).
minimum_except_0_signature([[var-VAR]|VARs], [S|Ss], MIN, DEFAULT) :-
    S in 0..4,
    VAR #=  0 #/\ MIN #\= DEFAULT #<=> S #= 0,
    VAR #=  0 #/\ MIN #=  DEFAULT #<=> S #= 1,
    VAR #\= 0 #/\ MIN #=  VAR     #<=> S #= 2,
    VAR #\= 0 #/\ MIN #<  VAR     #<=> S #= 3,
    VAR #\= 0 #/\ MIN #>  VAR     #<=> S #= 4,
    minimum_except_0_signature(VARs, Ss, MIN, DEFAULT).
