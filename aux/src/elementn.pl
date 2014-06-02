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

ctr_date(elementn,['20061004']).

ctr_origin(elementn, 'P. Flener', []).

ctr_arguments(elementn,
              ['INDEX'-dvar                    ,
               'TABLE'-collection(value-int)   ,
               'ENTRIES'-collection(entry-dvar)]).

ctr_exchangeable(elementn,
                 [vals(['TABLE'^value,'ENTRIES'^entry],int,=\=,all,dontcare)]).

ctr_restrictions(elementn,
                 ['INDEX' >= 1                              ,
                  'INDEX' =< size('TABLE')-size('ENTRIES')+1,
                  size('TABLE')   > 0                       ,
                  size('ENTRIES') > 0                       ,
                  size('TABLE') >= size('ENTRIES')          ,
                  required('TABLE',value)                   ,
                  required('ENTRIES',entry)                 ]).

ctr_typical(elementn,
            [size('TABLE')        > 1,
             range('TABLE'^value) > 1,
             size('ENTRIES')      > 1]).

ctr_extensible(elementn, [], 'TABLE', suffix).

ctr_example(elementn,
            elementn(3, [[value-6],[value-9],[value-2],[value-9]], [[entry-2],[entry-9]])).

ctr_see_also(elementn,
 [link('common keyword', element, '%k', ['data constraint'])]).

ctr_key_words(elementn,['data constraint'                 ,
                        'sliding sequence constraint'     ,
                        'table'                           ,
                        'automaton'                       ,
                        'automaton without counters'      ,
                        'reified automaton constraint'    ,
                        'Berge-acyclic constraint network',
                        'arc-consistency'                 ]).

ctr_persons(elementn,['Flener P.']).

ctr_eval(elementn, [reformulation(elementn_r),
                    automaton(elementn_a)]).

elementn_r(INDEX, TABLE, ENTRIES) :-
    length(TABLE, N),
    length(ENTRIES, M),
    N >  0,
    M >  0,
    N >= M,
    NM is N-M+1,
    check_type(dvar(1,NM), INDEX),
    collection(TABLE, [int]),
    collection(ENTRIES, [dvar]),
    get_attr1(TABLE  , TAB),
    get_attr1(ENTRIES, VALS),
    elementn1(VALS, 0, INDEX, TAB).

elementn1([], _, _, _).
elementn1([V|R], K, INDEX, TAB) :-
    IND #= INDEX + K,
    element(IND, TAB, V),
    K1 is K+1,
    elementn1(R, K1, INDEX, TAB).

elementn_a(FLAG, INDEX, TABLE, ENTRIES) :-
    length(TABLE, T),
    length(ENTRIES, E),
    T >  0,
    E >  0,
    T >= E,
    TE is T-E+1,
    check_type(dvar(1,TE), INDEX),
    collection(TABLE, [int]),
    collection(ENTRIES, [dvar]),
    elementn_get_para(TABLE, Table),           % extract the list of integers
    elementn_get_para(ENTRIES, Entries),       % extract the list of variables
    elementn_gen_val(1, TE, LV),               % generate values 1,...,T-E+1
    elementn_gen_arc(1, TE, E, LV, Table, Arcs),
    append([INDEX], Entries, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          Arcs,
                          [], [], []),
    union_dom_list_int(SIGNATURE, ALPHABET),
    automaton_bool(FLAG, ALPHABET, AUTOMATON).

elementn_get_para([], []).
elementn_get_para([[_-P]|R], [P|S]) :-
	elementn_get_para(R, S).

elementn_gen_val(I, I, [I]) :- !.
elementn_gen_val(I, J, [I|R]) :-
	I < J,
	I1 is I + 1,
	elementn_gen_val(I1, J, R).

% elementn_gen_arc(I, J, E, LV, LT, A): build the list of arcs of the automaton of elementn
%  I : start of a sequence of E consecutive entries of the table
%  J : end   of a sequence of E consecutive entries of the table
%  E : number of values of the sequence
%  LV: list of the form 1,2,...,T-E+1
%  LT: list of the form T1,T2,... giving each entry of the table
%  A : list of arcs that will be generated
elementn_gen_arc(I, J, _, _, _, []) :-
	I > J, !.
elementn_gen_arc(I, J, E, [I|S], [F|T], Arcs) :-
	I =< J,
	K is 1 + E*(I-1),
	A0 = [arc(s,I,K)],
	elementn_gen_arc1(1, E, K, [F|T], A1),
	I1 is I + 1,
	elementn_gen_arc(I1, J, E, S, T, A),
	append(A0, A1, A2),
	append(A2, A , Arcs).

elementn_gen_arc1(J, E, K, [F|T], [arc(K,F,K1)|R]) :-
	J < E, !,
	K1 is K + 1,
	J1 is J + 1,
	elementn_gen_arc1(J1, E, K1, T, R).
elementn_gen_arc1(E, E, K, [F|_], [arc(K,F,t)]).
