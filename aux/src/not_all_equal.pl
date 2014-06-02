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

ctr_date(not_all_equal,['20030820','20040530','20040726','20060812','20100418']).

ctr_origin(not_all_equal, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(not_all_equal,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(not_all_equal,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(not_all_equal,
                 [required('VARIABLES',var),
                  size('VARIABLES') > 1    ]).

ctr_typical(not_all_equal,
            [size('VARIABLES')     > 2,
	     nval('VARIABLES'^var) > 2]).

ctr_extensible(not_all_equal, [], 'VARIABLES', any).

ctr_graph(not_all_equal,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSCC' > 1],
          []).

ctr_example(not_all_equal,
            not_all_equal([[var-3],[var-1],[var-3],[var-3],[var-3]])).

ctr_draw_example(not_all_equal,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-3],[var-3],[var-3]]],
                 ['CLIQUE'],
                 [1-[1,3,4,5],
                  2-[2],
                  3-[1,3,4,5],
                  4-[1,3,4,5],
                  5-[1,3,4,5]],
                 ['NSCC'([[1,3,4,5],[2]])],
                 '','NSCC=2',
                 [2.4,2.3,2.145,2.3]).

ctr_see_also(not_all_equal,
 [link('negation',              all_equal,      '',                                                                []),
  link('implied by',            alldifferent,   '',                                                                []),
  link('used in reformulation', atleast_nvalue, '',                                                                []),
  link('generalisation',        nvalue,         'introduce a variable for counting the number of distinct values', []),
  link('specialisation',        neq,            'when go down to two variables',                                   [])]).

ctr_key_words(not_all_equal,['value constraint'                       ,
                             'disequality'                            ,
                             'automaton'                              ,
                             'automaton without counters'             ,
                             'reified automaton constraint'           ,
                             'sliding cyclic(1) constraint network(1)',
                             'equivalence'                            ,
                             'arc-consistency'                        ]).

ctr_eval(not_all_equal, [checker(not_all_equal_c),
			 reformulation(not_all_equal_r),
                         automaton(not_all_equal_a)]).

ctr_sol(not_all_equal,2,0,2,6,-).
ctr_sol(not_all_equal,3,0,3,60,-).
ctr_sol(not_all_equal,4,0,4,620,-).
ctr_sol(not_all_equal,5,0,5,7770,-).
ctr_sol(not_all_equal,6,0,6,117642,-).
ctr_sol(not_all_equal,7,0,7,2097144,-).
ctr_sol(not_all_equal,8,0,8,43046712,-).

not_all_equal_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    not_all_equal_c1(VARIABLES).

not_all_equal_c1([V,V|R]) :- !,
    not_all_equal_c1([V|R]).
not_all_equal_c1([_,_|_]).

not_all_equal_r(VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 1,
    get_attr1(VARIABLES, VARS),
    NVAL in 2..N,
    nvalue(NVAL, VARS).

% 0: VAR1=\=VAR2
% 1: VAR1=VAR2
not_all_equal_a(FLAG, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES,N),
    N > 1,
    not_all_equal_signature(VARIABLES, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,1,s),
                           arc(s,0,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

not_all_equal_signature([], []).
not_all_equal_signature([_], []) :- !.
not_all_equal_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss]) :-
    VAR1 #= VAR2 #<=> S,
    not_all_equal_signature([[var-VAR2]|VARs], Ss).
