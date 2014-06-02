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

ctr_date(int_value_precede_chain,['20041003','20090728','20090822']).

ctr_origin(int_value_precede_chain, '\\cite{YatChiuLawJimmyLee04}', []).

ctr_arguments(int_value_precede_chain,
              ['VALUES'-collection(var-int),		% call it var since want to use the used_by constraint in the ctr_typical
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(int_value_precede_chain,
                 [vals(['VARIABLES'^var],int(notin('VALUES'^var)),=\=,dontcare,dontcare)]).

ctr_synonyms(int_value_precede_chain,[precede            ,
				      precedence         ,
				      value_precede_chain]).

ctr_restrictions(int_value_precede_chain,
                 [required('VALUES',var)   ,
                  distinct('VALUES',var)   ,
                  required('VARIABLES',var)]).

ctr_typical(int_value_precede_chain,
            [size('VALUES')         > 1             ,
             strictly_increasing('VALUES')          ,
             size('VARIABLES')      > size('VALUES'),
             range('VARIABLES'^var) > 1             ,
             used_by('VARIABLES','VALUES')          ]).

ctr_contractible(int_value_precede_chain, [], 'VALUES', any).
ctr_contractible(int_value_precede_chain, [], 'VARIABLES', suffix).

% int_value_precede_chain('VALUES', 'VARIABLES1') and
% int_value_precede_chain('VALUES', 'VARIABLES2') =>
% int_value_precede_chain('VALUES', union('VARIABLES1','VARIABLES2'))
ctr_aggregate(int_value_precede_chain, [], [id, union]).

ctr_example(int_value_precede_chain,
            int_value_precede_chain([[var-4], [var-0], [var-1]],
                                    [[var-4], [var-0], [var-6], [var-1], [var-0]])).

ctr_see_also(int_value_precede_chain,
 [link(specialisation, int_value_precede, '%e of at least %e %e replaced by %e of %e %e', [sequence, 2, values, sequence, 2, values])]).

ctr_key_words(int_value_precede_chain,['order constraint'                ,
                                       'symmetry'                        ,
                                       'indistinguishable values'        ,
                                       'value precedence'                ,
                                       'graph colouring'                 ,
                                       'Berge-acyclic constraint network',
                                       'automaton'                       ,
                                       'automaton without counters'      ,
                                       'reified automaton constraint'    ,
                                       'arc-consistency'                 ]).

ctr_persons(int_value_precede_chain,['Law Y. C.'    ,
                                     'Lee J. H. M.' ,
                                     'Sulanke T.'   ,
                                     'Beldiceanu N.',
                                     'Walsh T.'     ]).

ctr_eval(int_value_precede_chain, [automaton(int_value_precede_chain_a)]).

int_value_precede_chain_a(FLAG, [], VARIABLES) :- !,
    collection(VARIABLES, [dvar]),
    (FLAG=1 -> true ; fail).
int_value_precede_chain_a(FLAG, VALUES, []) :- !,
    collection(VALUES, [int]),
    get_attr1(VALUES, VALS),
    all_different(VALS),
    (FLAG=1 -> true ; fail).
int_value_precede_chain_a(FLAG, VALUES, VARIABLES) :-
    collection(VALUES, [int]),
    collection(VARIABLES, [dvar]),
    length(VALUES, 1),
    !,
    (FLAG=1 -> true ; fail).
int_value_precede_chain_a(FLAG, VALUES, VARIABLES) :-
    collection(VALUES, [int]),
    collection(VARIABLES, [dvar]),
    get_attr1(VALUES, VALS),
    all_different(VALS),
    length(VALS, N),
    get_attr1(VARIABLES, VARS),
    int_value_precede_chain_gen_complement(VARS, VALS, COMPLEMENT),
    int_value_precede_chain_gen_states1(0, N, STATES),
    int_value_precede_chain_gen_transitions(N, VALS, COMPLEMENT, STATES, TRANSITIONS),
    nth0(0, STATES, S0),
    int_value_precede_chain_gen_states2(STATES, S0, AUTOMATON_STATES),
    AUTOMATON = automaton(VARS, _, VARS, AUTOMATON_STATES, TRANSITIONS, [], [], []),
    append(VALS, COMPLEMENT, ALPHABET),
    automaton_bool(FLAG, ALPHABET, AUTOMATON).

int_value_precede_chain_gen_states2([], S0, [source(S0)]) :-
    !.
int_value_precede_chain_gen_states2([S|R], S0, [sink(S)|T]) :-
    int_value_precede_chain_gen_states2(R, S0, T).

int_value_precede_chain_gen_complement(VARS, VALS, COMPLEMENT) :-
    union_dom_set(VARS, UNION),
    list_to_fdset(VALS, VALUES),
    fdset_subtract(UNION, VALUES, DIFFERENCE),
    fdset_to_list(DIFFERENCE, COMPLEMENT).

int_value_precede_chain_gen_states1(I, N, []) :-
    I > N,
    !.
int_value_precede_chain_gen_states1(I, N, [INAME|R]) :-
    I =< N,
    number_codes(I, ICODE), atom_codes(IATOM, ICODE), atom_concat('s', IATOM, INAME),
    I1 is I+1,
    int_value_precede_chain_gen_states1(I1, N, R).

int_value_precede_chain_gen_transitions(N, VALS, COMPLEMENT, STATES, TRANSITIONS) :-
    N1 is N-1,
    int_value_precede_chain_gen_transitions1(0, N1, VALS, STATES, TR1),
    int_value_precede_chain_gen_transitions2(1, N , VALS, STATES, TR2),
    int_value_precede_chain_gen_transitions3(0, N , VALS, STATES, COMPLEMENT, TR3),
    append(TR1, TR2, TR12),
    append(TR12, TR3, TRANSITIONS).

int_value_precede_chain_gen_transitions1(I, N1, _, _, []) :-
    I > N1,
    !.
int_value_precede_chain_gen_transitions1(I, N1, VALS, STATES, [arc(Si,Vii,Sii)|R]) :-
    I =< N1,
    I1 is I+1,
    nth0(I , STATES, Si ),
    nth1(I1, VALS  , Vii),
    nth0(I1, STATES, Sii),
    int_value_precede_chain_gen_transitions1(I1, N1, VALS, STATES, R).

int_value_precede_chain_gen_transitions2(I, N1, _, _, []) :-
    I > N1,
    !.
int_value_precede_chain_gen_transitions2(I, N1, VALS, STATES, TRANSITIONS) :-
    I =< N1,
    int_value_precede_chain_gen_transitions21(1, I, VALS, STATES, TR),
    I1 is I+1,
    int_value_precede_chain_gen_transitions2(I1, N1, VALS, STATES, R),
    append(TR, R, TRANSITIONS).

int_value_precede_chain_gen_transitions21(J, I, _, _, []) :-
    J > I,
    !.
int_value_precede_chain_gen_transitions21(J, I, VALS, STATES, [arc(Si,Vj,Si)|R]) :-
    J =< I,
    nth0(I, STATES, Si),
    nth1(J, VALS  , Vj),
    J1 is J+1,
    int_value_precede_chain_gen_transitions21(J1, I, VALS, STATES, R).

int_value_precede_chain_gen_transitions3(_, _, _, _, [], []) :-
    !.
int_value_precede_chain_gen_transitions3(I, N1, _, _, _, []) :-
    I > N1,
    !.
int_value_precede_chain_gen_transitions3(I, N1, VALS, STATES, [C|CC], TRANSITIONS) :-
    I =< N1,
    length([C|CC], LC),
    int_value_precede_chain_gen_transitions31(1, LC, I, [C|CC], STATES, TR),
    I1 is I+1,
    int_value_precede_chain_gen_transitions3(I1, N1, VALS, STATES, [C|CC], R),
    append(TR, R, TRANSITIONS).

int_value_precede_chain_gen_transitions31(J, LC, _, _, _, []) :-
    J > LC,
    !.
int_value_precede_chain_gen_transitions31(J, LC, I, C, STATES, [arc(Si,Cj,Si)|R]) :-
    J =< LC,
    nth0(I, STATES, Si),
    nth1(J, C     , Cj),
    J1 is J+1,
    int_value_precede_chain_gen_transitions31(J1, LC, I, C, STATES, R).
