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

ctr_date(increasing_nvalue,['20091104']).

ctr_origin(increasing_nvalue, 'Conjoin %c and %c.', [nvalue,increasing]).

ctr_arguments(increasing_nvalue,
              ['NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(increasing_nvalue,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(increasing_nvalue,
                 ['NVAL' >= min(1,size('VARIABLES')),
                  'NVAL' =< size('VARIABLES')       ,
                  required('VARIABLES',var)         ,
                  increasing('VARIABLES')           ]).

ctr_typical(increasing_nvalue,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_graph(increasing_nvalue,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSCC' = 'NVAL'],
          ['EQUIVALENCE']).

ctr_example(increasing_nvalue,
            [increasing_nvalue(2,[[var-6],[var-6],[var-8],[var-8],[var-8]]),
             increasing_nvalue(1,[[var-6],[var-6],[var-6],[var-6],[var-6]]),
	     increasing_nvalue(5,[[var-0],[var-2],[var-3],[var-6],[var-7]])]).

ctr_draw_example(increasing_nvalue,
                 ['VARIABLES'],
                 [[[var-6],[var-6],[var-8],[var-8],[var-8]]],
                 ['CLIQUE'],
                 [1-[1,2],
                  2-[1,2],
                  3-[3,4,5],
                  4-[3,4,5],
                  5-[3,4,5]],
                 ['NSCC'([[1,2],[3,4,5]])],
                 '','NSCC=2',
                 [2.145,2.3,2.8,2.33]).

ctr_see_also(increasing_nvalue,
 [link('implies',          increasing,              'remove $\\argument{NVAL}$ parameter from %c',  [increasing_nvalue]),
  link('implies',          nvalue,                  '',                                             []),
  link('implies',          nvisible_from_start,     '',                                             []),
  link('related',          increasing_nvalue_chain, '',                                             []),
  link('shift of concept', ordered_nvector,         '%e replaced by %e and $\\leq$ replaced by %c', [variable, vector, lex_lesseq])]).

ctr_key_words(increasing_nvalue,['counting constraint'                   ,
                                 'value partitioning constraint'         ,
                                 'Berge-acyclic constraint network'      ,
                                 'automaton'                             ,
                                 'automaton without counters'            ,
                                 'reified automaton constraint'          ,
                                 'arc-consistency'                       ,
                                 'number of distinct equivalence classes',
                                 'number of distinct values'             ,
                                 'strongly connected component'          ,
                                 'equivalence'                           ,
                                 'order constraint'                      ,
                                 'symmetry'                              ,
                                 'functional dependency'                 ]).

ctr_persons(increasing_nvalue,['Beldiceanu N.',
                               'Hermenier F.' ,
                               'Lorca X.'     ,
                               'Petit T.'     ,
                               'Pesant G.'    ]).

ctr_eval(increasing_nvalue, [checker(increasing_nvalue_c)      ,
			     builtin(increasing_nvalue_b)      ,
			     reformulation(increasing_nvalue_r),
                             automata(increasing_nvalue_a)     ]).

ctr_sol(increasing_nvalue,2,0,2,6,[1-3,2-3]).
ctr_sol(increasing_nvalue,3,0,3,20,[1-4,2-12,3-4]).
ctr_sol(increasing_nvalue,4,0,4,70,[1-5,2-30,3-30,4-5]).
ctr_sol(increasing_nvalue,5,0,5,252,[1-6,2-60,3-120,4-60,5-6]).
ctr_sol(increasing_nvalue,6,0,6,924,[1-7,2-105,3-350,4-350,5-105,6-7]).
ctr_sol(increasing_nvalue,7,0,7,3432,[1-8,2-168,3-840,4-1400,5-840,6-168,7-8]).
ctr_sol(increasing_nvalue,8,0,8,12870,[1-9,2-252,3-1764,4-4410,5-4410,6-1764,7-252,8-9]).

increasing_nvalue_c(_, [[var-X],[var-Y]|_]) :-
    X > Y,
    !,
    fail.
increasing_nvalue_c(0, []) :-
    !.
increasing_nvalue_c(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARS, N),
    (N = 1 ->
	NVAL = 1
    ;
	VARS = [VAR|R],
	increasing_nvalue_c(R, VAR, 1, NVAL)
    ).

increasing_nvalue_c([], _, NVAL, NVAL) :- !.
increasing_nvalue_c([V|R], Prev, Count, NVAL) :-
    Prev =< V,
    (V = Prev -> Count1 = Count ; Count1 is Count+1),
    increasing_nvalue_c(R, V, Count1, NVAL).

increasing_nvalue_counters_check([], _, _, []) :- !.
increasing_nvalue_counters_check([V|R], Prev, Count, [Count1|S]) :-
    integer(Prev), Prev =< V, !,
    (V = Prev -> Count1 = Count ; Count1 is Count+1),
    increasing_nvalue_counters_check(R, V, Count1, S).
increasing_nvalue_counters_check([_|R], _, _, [-|S]) :-
    increasing_nvalue_counters_check(R, -, _, S).

decreasing_nvalue_counters_check([], _, _, []) :- !.
decreasing_nvalue_counters_check([V|R], Prev, Count, [Count1|S]) :-
    integer(Prev), Prev >= V, !,
    (V = Prev -> Count1 = Count ; Count1 is Count+1),
    decreasing_nvalue_counters_check(R, V, Count1, S).
decreasing_nvalue_counters_check([_|R], _, _, [-|S]) :-
    decreasing_nvalue_counters_check(R, -, _, S).

increasing_nvalue_b(0, []) :- !.
increasing_nvalue_b(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    NVAL #>= min(1,N),
    NVAL #=< N,
    increasing_nvalue_b1(VARS, S),
    call(NVAL #= S).

increasing_nvalue_b1([_], 1) :- !.
increasing_nvalue_b1([V1,V2|R], B+S) :-
    V1 #=< V2,
    B #<=> V1 #< V2,
    increasing_nvalue_b1([V2|R], S).

increasing_nvalue_r(0, []) :-
    !.
increasing_nvalue_r(NVAL, VARIABLES) :-
    eval(increasing(VARIABLES)),
    eval(nvalue(NVAL, VARIABLES)).

increasing_nvalue_a(0, []) :-
    !.
increasing_nvalue_a(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    NVAL #>= min(1,N),
    NVAL #=< N,
    get_minimum(VARS, MINVARS),
    get_maximum(VARS, MAXVARS),
    SIZE is MAXVARS-MINVARS+1,
    fd_min(NVAL, MINNVAL),
    fd_max(NVAL, MAXNVAL),
    D is min(N,min(SIZE,MAXNVAL)),
    fd_set(NVAL, SVAL),
    fdset_to_list(SVAL, VALUES),
    increasing_nvalue_states(VALUES, SIZE, MINNVAL, STATES),
    gen_automaton_state('s', 0, 0, S_00),
    increasing_nvalue_class1(1, SIZE, MINNVAL, MINVARS, S_00, TRANS1),
    increasing_nvalue_class2(1, D, SIZE, MINNVAL, MINVARS, TRANS2),
    increasing_nvalue_class3(1, D, SIZE, MINNVAL, MINVARS, TRANS3),
    append(TRANS1, TRANS2, TRANS12),
    append(TRANS12, TRANS3, ALL_TRANSITIONS),
    automaton(VARS, _, VARS, STATES, ALL_TRANSITIONS, [], [], []),
    eval(nvalue(NVAL, VARIABLES)).

increasing_nvalue_states([], _, _, [source(S_00)]) :-
    gen_automaton_state('s', 0, 0, S_00).
increasing_nvalue_states([V|R], SIZE, MINNVAL, STATES) :-
    increasing_nvalue_states1(V, SIZE, V, MINNVAL, STATES1),
    increasing_nvalue_states(R, SIZE, MINNVAL, STATES2),
    append(STATES1, STATES2, STATES).

increasing_nvalue_states1(J, SIZE, _, _, []) :-
    J > SIZE,
    !.
increasing_nvalue_states1(J, SIZE, I, MINNVAL, [sink(S_IJ)|STATES]) :-
    J =< SIZE,
    I_SIZE_J is I+SIZE-J,
    I_SIZE_J >= MINNVAL,
    !,
    gen_automaton_state('s', I, J, S_IJ),
    J1 is J+1,
    increasing_nvalue_states1(J1, SIZE, I, MINNVAL, STATES).
increasing_nvalue_states1(J, SIZE, I, MINNVAL, STATES) :-
    J =< SIZE,
    J1 is J+1,
    increasing_nvalue_states1(J1, SIZE, I, MINNVAL, STATES).

increasing_nvalue_class1(J, SIZE, _, _, _, []) :-
    J > SIZE,
    !.
increasing_nvalue_class1(J, SIZE, MINNVAL, MINVARS, S_00, [arc(S_00,LABEL,S_1J)|TRANS]) :-
    J =< SIZE,
    I_SIZE_J is 1+SIZE-J,
    I_SIZE_J >= MINNVAL,
    !,
    gen_automaton_state('s', 1, J, S_1J),
    LABEL is MINVARS+J-1,
    J1 is J+1,
    increasing_nvalue_class1(J1, SIZE, MINNVAL, MINVARS, S_00, TRANS).
increasing_nvalue_class1(J, SIZE, MINNVAL, MINVARS, S_00, TRANS) :-
    J =< SIZE,
    J1 is J+1,
    increasing_nvalue_class1(J1, SIZE, MINNVAL, MINVARS, S_00, TRANS).

increasing_nvalue_class2(I, D, _SIZE, _MINNVAL, _MINVARS, []) :-
    I > D,
    !.
increasing_nvalue_class2(I, D, SIZE, MINNVAL, MINVARS, TRANS) :-
    I =< D,
    increasing_nvalue_class21(I, SIZE, I, MINNVAL, MINVARS, TRANS1),
    I1 is I+1,
    increasing_nvalue_class2(I1, D, SIZE, MINNVAL, MINVARS, TRANS2),
    append(TRANS1, TRANS2, TRANS).    

increasing_nvalue_class21(J, SIZE, _I, _MINNVAL, _MINVARS, []) :-
    J > SIZE,
    !.
increasing_nvalue_class21(J, SIZE, I, MINNVAL, MINVARS, [arc(S_IJ,LABEL,S_IJ)|TRANS]) :-
    J =< SIZE,
    I_SIZE_J is I+SIZE-J,
    I_SIZE_J >= MINNVAL,
    !,    
    gen_automaton_state('s', I, J, S_IJ),
    LABEL is MINVARS+J-1,
    J1 is J+1,
    increasing_nvalue_class21(J1, SIZE, I, MINNVAL, MINVARS, TRANS).
increasing_nvalue_class21(J, SIZE, I, MINNVAL, MINVARS, TRANS) :-
    J =< SIZE,
    J1 is J+1,
    increasing_nvalue_class21(J1, SIZE, I, MINNVAL, MINVARS, TRANS).

increasing_nvalue_class3(I, D, _, _, _, []) :-
    I >= D,
    !.
increasing_nvalue_class3(I, D, SIZE, MINNVAL, MINVARS, TRANS) :-
    I < D,
    increasing_nvalue_class31(I, SIZE, I, MINNVAL, MINVARS, TRANS1),
    I1 is I+1,
    increasing_nvalue_class3(I1, D, SIZE, MINNVAL, MINVARS, TRANS2),
    append(TRANS1, TRANS2, TRANS).

increasing_nvalue_class31(J, SIZE, _, _, _, []) :-
    J > SIZE,
    !.
increasing_nvalue_class31(J, SIZE, I, MINNVAL, MINVARS, TRANS) :-
    J =< SIZE,
    I_SIZE_J is I+SIZE-J,
    I_SIZE_J >= MINNVAL,
    !,
    gen_automaton_state('s', I, J, S_IJ),
    J1 is J+1,
    increasing_nvalue_class32(J1, SIZE, I, J, S_IJ, MINNVAL, MINVARS, TRANS1),
    increasing_nvalue_class31(J1, SIZE, I, MINNVAL, MINVARS, TRANS2),
    append(TRANS1, TRANS2, TRANS).
increasing_nvalue_class31(J, SIZE, I, MINNVAL, MINVARS, TRANS) :-
    J =< SIZE,
    J1 is J+1,
    increasing_nvalue_class31(J1, SIZE, I, MINNVAL, MINVARS, TRANS).

increasing_nvalue_class32(K, SIZE, _, _, _, _, _, []) :-
    K > SIZE,
    !.
increasing_nvalue_class32(K, SIZE, I, J, S_IJ, MINNVAL, MINVARS, [arc(S_IJ,LABEL,S_I1K)|TRANS]) :-
    K =< SIZE,
    I1 is I+1,
    I1_SIZE_K is I1+SIZE-K,
    I1_SIZE_K >= MINNVAL,
    !,
    gen_automaton_state('s', I1, K, S_I1K),
    LABEL is MINVARS+K-1,
    K1 is K+1,
    increasing_nvalue_class32(K1, SIZE, I, J, S_IJ, MINNVAL, MINVARS, TRANS).
increasing_nvalue_class32(K, SIZE, I, J, S_IJ, MINNVAL, MINVARS, TRANS) :-
    K =< SIZE,
    K1 is K+1,
    increasing_nvalue_class32(K1, SIZE, I, J, S_IJ, MINNVAL, MINVARS, TRANS).
