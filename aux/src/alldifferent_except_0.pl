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

ctr_date(alldifferent_except_0,['20000128','20030820','20040530','20060803']).

ctr_origin(alldifferent_except_0, 'Derived from %c.', [alldifferent]).

ctr_arguments(alldifferent_except_0,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(alldifferent_except_0,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int(=\=(0)),=\=,all,dontcare)]).

ctr_synonyms(alldifferent_except_0,[alldiff_except_0    ,
                                    alldistinct_except_0]).

ctr_restrictions(alldifferent_except_0,
                 [required('VARIABLES',var)]).

ctr_typical(alldifferent_except_0,
            [size('VARIABLES')     > 2 ,
             atleast(2,'VARIABLES',0)  ,
	     range('VARIABLES'^var) > 1]).

ctr_contractible(alldifferent_except_0, [], 'VARIABLES', any).

ctr_graph(alldifferent_except_0,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var =\= 0             ,
           variables1^var  =  variables2^var],
          ['MAX_NSCC' =< 1],
	  []).

ctr_example(alldifferent_except_0,
            alldifferent_except_0([[var-5],[var-0],[var-1],[var-9],[var-0],[var-3]])).

ctr_draw_example(alldifferent_except_0,
                 ['VARIABLES'],
                 [[[var-5],[var-0],[var-1],[var-9],[var-0],[var-3]]],
                 ['CLIQUE'],
                 [1-1,3-3,4-4,6-6],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 []).

ctr_see_also(alldifferent_except_0,
 [link('implied by',   alldifferent,             '', []),
  link('implies',      multi_global_contiguity,  '', []),
  link('hard version', alldifferent,             '', []),
  link('cost variant', weighted_partial_alldiff, '', [])]).

ctr_key_words(alldifferent_except_0,['value constraint'                ,
                                     'relaxation'                      ,
                                     'joker value'                     ,
                                     'all different'                   ,
				     'bipartite matching'              ,
                                     'sort based reformulation'        ,
                                     'automaton'                       ,
                                     'automaton with array of counters',
                                     'one\\_succ'                      ,
                                     'arc-consistency'                 ]).

ctr_persons(alldifferent_except_0,['Cymer R.']).

ctr_eval(alldifferent_except_0, [reformulation(alldifferent_except_0_r),
				 checker(alldifferent_except_0_c)      ,
				 density(alldifferent_except_0_d)      ]).

ctr_sol(alldifferent_except_0,2,0,2,7,-).
ctr_sol(alldifferent_except_0,3,0,3,34,-).
ctr_sol(alldifferent_except_0,4,0,4,209,-).
ctr_sol(alldifferent_except_0,5,0,5,1546,-).
ctr_sol(alldifferent_except_0,6,0,6,13327,-).
ctr_sol(alldifferent_except_0,7,0,7,130922,-).
ctr_sol(alldifferent_except_0,8,0,8,1441729,-).

alldifferent_except_0_c([[var-V],[var-V]|_]) :-
	V =\= 0,
	!,
	fail.
alldifferent_except_0_c(VARIABLES) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
        filter_zeros(VARS, L),
	sort(L, SL),
	length(L, N),
	length(SL, N).

filter_zeros([], []) :- !.
filter_zeros([0|R], S) :-
	!,
	filter_zeros(R, S).
filter_zeros([X|R], [X|S]) :-
	filter_zeros(R, S).

alldifferent_except_0_r(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	alldifferent_except_01(VARS).

alldifferent_except_01([]).
alldifferent_except_01([_]) :- !.
alldifferent_except_01([V1|R]) :-
	alldifferent_except_01(R, V1),
	alldifferent_except_01(R).

alldifferent_except_01([], _).
alldifferent_except_01([V2|R], V1) :-
	V1#=0 #\/ V2#=0 #\/ V1#\=V2,
	alldifferent_except_01(R, V1).

alldifferent_except_0_d(Density, VARIABLES) :-
	get_attr1(VARIABLES, VARS),
	sort(VARS, SVARS),
	length(VARS, N),
	length(SVARS, S),
	Density is S / N.
