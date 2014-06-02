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

ctr_date(global_contiguity,['20030820','20040530','20060809']).

ctr_origin(global_contiguity, '\\cite{Maher02}', []).

ctr_arguments(global_contiguity,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(global_contiguity,
                 [items('VARIABLES',reverse)]).

ctr_synonyms(global_contiguity,[contiguity]).

ctr_restrictions(global_contiguity,
                 [required('VARIABLES',var),
                  'VARIABLES'^var >= 0     ,
                  'VARIABLES'^var =< 1     ]).

ctr_typical(global_contiguity,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1,
	     atleast(2,'VARIABLES',1)  ]).

ctr_contractible(global_contiguity, [], 'VARIABLES', any).

ctr_graph(global_contiguity,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2),
           'LOOP'>>collection(variables1,variables2)],
          [variables1^var = variables2^var,
           variables1^var = 1             ],
          ['NCC' =< 1],
          []).

ctr_example(global_contiguity,
            global_contiguity([[var-0],[var-1],[var-1],[var-0]])).

ctr_draw_example(global_contiguity,
                 ['VARIABLES'],
                 [[[var-0],[var-1],[var-1],[var-0]]],
                 ['PATH','LOOP'],
                 [2-[2,3],3-3],
                 ['NCC'([[2,3]])],
                 '','NCC=1',
                 [2.145,2.145,1.9,1.4]).

ctr_cond_imply(global_contiguity, some_equal, [size('VARIABLES') > 2], [], id).

ctr_see_also(global_contiguity,
 [link('implies',        multi_global_contiguity, '', []),
  link('implies',        no_valley,               '', []),
  link('implies',        consecutive_values,      '', []),
  link('common keyword', group,                 '%k', ['sequence']),
  link('common keyword', inflexion,             '%k', ['sequence']),
  link('related',        roots,                   '', [])]).

ctr_key_words(global_contiguity,['sequence'                        ,
                                 'connected component'             ,
                                 'convex'                          ,
                                 'Berge-acyclic constraint network',
                                 'automaton'                       ,
                                 'automaton without counters'      ,
			         'automaton with same input symbol',
                                 'reified automaton constraint'    ,
                                 'arc-consistency'                 ]).

ctr_persons(global_contiguity,['Maher M. J.']).

ctr_eval(global_contiguity, [checker(global_contiguity_c),
			     automaton(global_contiguity_a)]).

ctr_sol(global_contiguity,2,0,1,4,-).
ctr_sol(global_contiguity,3,0,1,7,-).
ctr_sol(global_contiguity,4,0,1,11,-).
ctr_sol(global_contiguity,5,0,1,16,-).
ctr_sol(global_contiguity,6,0,1,22,-).
ctr_sol(global_contiguity,7,0,1,29,-).
ctr_sol(global_contiguity,8,0,1,37,-).
ctr_sol(global_contiguity,9,0,1,46,-).
ctr_sol(global_contiguity,10,0,1,56,-).
ctr_sol(global_contiguity,11,0,1,67,-).
ctr_sol(global_contiguity,12,0,1,79,-).
ctr_sol(global_contiguity,13,0,1,92,-).
ctr_sol(global_contiguity,14,0,1,106,-).
ctr_sol(global_contiguity,15,0,1,121,-).
ctr_sol(global_contiguity,16,0,1,137,-).
ctr_sol(global_contiguity,17,0,1,154,-).
ctr_sol(global_contiguity,18,0,1,172,-).
ctr_sol(global_contiguity,19,0,1,191,-).
ctr_sol(global_contiguity,20,0,1,211,-).
ctr_sol(global_contiguity,21,0,1,232,-).
ctr_sol(global_contiguity,22,0,1,254,-).
ctr_sol(global_contiguity,23,0,1,277,-).
ctr_sol(global_contiguity,24,0,1,301,-).

global_contiguity_c([]) :- !.
global_contiguity_c(VARIABLES) :-
	collection(VARIABLES, [int(0,1)]),
	get_attr1(VARIABLES, VARS),
	global_contiguity_c1(VARS).

global_contiguity_c1([]) :- !.
global_contiguity_c1([0|R]) :-
	!,
	global_contiguity_c1(R).
global_contiguity_c1([1|R]) :-
	global_contiguity_c2(R).

global_contiguity_c2([]) :- !.
global_contiguity_c2([1|R]) :-
	!,
	global_contiguity_c2(R).
global_contiguity_c2([0|R]) :-
	global_contiguity_c3(R).

global_contiguity_c3([]) :- !.
global_contiguity_c3([0|R]) :-
	global_contiguity_c3(R).

% 0: VAR=0
% 1: VAR=1
global_contiguity_a(1, []) :- !.
global_contiguity_a(0, []) :- !, fail.
global_contiguity_a(FLAG, VARIABLES) :-
    collection(VARIABLES, [dvar(0,1)]),
    get_attr1(VARIABLES, LIST_VARIABLES),
    AUTOMATON = automaton(LIST_VARIABLES, _,
                          LIST_VARIABLES, 
                          [source(s),sink(m),sink(z),sink(s)],
                          [arc(s,0,s),
                           arc(s,1,m),
                           arc(m,0,z),
                           arc(m,1,m),
                           arc(z,0,z)],
                           [], [], []),
    automaton_bool(FLAG, [0,1], AUTOMATON).

