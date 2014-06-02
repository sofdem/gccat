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

ctr_date(period_except_0,['20030820','20040530','20060813']).

ctr_origin(period_except_0, 'Derived from %c.', [period]).

ctr_arguments(period_except_0,
              ['PERIOD'-dvar                   ,
               'VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ]).

ctr_exchangeable(period_except_0,
                 [items('VARIABLES',reverse),
                  items('VARIABLES',shift),
                  vals(['VARIABLES'^var],int(=\=(0)),=\=,all,dontcare)]).

ctr_restrictions(period_except_0,
                 ['PERIOD' >= 1                   ,
                  'PERIOD' =< size('VARIABLES')   ,
                  required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(period_except_0,
            ['PERIOD'               > 1                ,
             'PERIOD'               < size('VARIABLES'),
             size('VARIABLES')      > 2                ,
             range('VARIABLES'^var) > 1                ,
             atleast(1,'VARIABLES',0)                  ,
             in_list('CTR',[=])                        ]).

ctr_pure_functional_dependency(period_except_0, []).
ctr_functional_dependency(period_except_0, 1, [2,3]).

ctr_contractible(period_except_0, [in_list('CTR',[=]),'PERIOD'=1], 'VARIABLES', any).
ctr_contractible(period_except_0, [], 'VARIABLES', prefix).
ctr_contractible(period_except_0, [], 'VARIABLES', suffix).

ctr_predefined(period_except_0).

ctr_example(period_except_0,
            period_except_0(3,
                            [[var-1],[var-1],[var-4],
                             [var-1],[var-1],[var-0],
                             [var-1],[var-1]],
                            =)).

ctr_see_also(period_except_0,
 [link('implied by',   period, '', []),
  link('hard version', period, '', [])]).

ctr_key_words(period_except_0,['predefined constraint'     ,
                               'periodic'                  ,
                               'timetabling constraint'    ,
                               'scheduling constraint'     ,
                               'sequence'                  ,
                               'joker value'               ,
                               'functional dependency'     ,
		               'pure functional dependency']).

ctr_persons(period_except_0,['Beldiceanu N.',
                             'Poder E.'     ]).

ctr_eval(period_except_0, [      checker(period_except_0_c),
			   reformulation(period_except_0_r)]).

period_except_0_c(PERIOD, VARIABLES, CTR) :-
	check_type(dvar, PERIOD),
	collection(VARIABLES, [int]),
	memberchk(CTR, [=, =\=, <, >=, >, =<]),
	length(VARIABLES, N),
	PERIOD #>= 1,
	PERIOD #=< N,
	fd_min(PERIOD, PMin),
	fd_max(PERIOD, PMax),
	get_attr1(VARIABLES, VARS),
	P in PMin..PMax,
	indomain(P),
	PERIOD = P,
	(P = N ->
	    true
	;
	    M is min(N,P),
	    append_length(FIRSTS, REST, VARS, M),
	    period_except_0_c(REST, FIRSTS, P, CTR)
	),
	!.

period_except_0_c([], _, _, _) :- !.
period_except_0_c(VARS, FIRSTS, P, CTR) :-
	length(VARS, N),
	M is min(N,P),
	append_length(NEXTS, REST, VARS, M),
	period_compare_except_0(CTR, FIRSTS, NEXTS),
	period_except_0_c(REST, NEXTS, P, CTR).

period_compare_except_0(_, [], _) :- !.
period_compare_except_0(_, _, []) :- !.
period_compare_except_0(CTR, [0|R], [_|S]) :-
	!,
	period_compare_except_0(CTR, R, S).
period_compare_except_0(CTR, [_|R], [0|S]) :-
	!,
	period_compare_except_0(CTR, R, S).
period_compare_except_0(=, [U|R], [V|S]) :-
	!,
	U = V,
	period_compare_except_0(=, R, S).
period_compare_except_0(=\=, [U|R], [V|S]) :-
	!,
	U =\= V,
	period_compare_except_0(=\=, R, S).
period_compare_except_0(<, [U|R], [V|S]) :-
	!,
	U < V,
	period_compare_except_0(<, R, S).
period_compare_except_0(>=, [U|R], [V|S]) :-
	!,
	U >= V,
	period_compare_except_0(>=, R, S).
period_compare_except_0(>, [U|R], [V|S]) :-
	!,
	U > V,
	period_compare_except_0(>, R, S).
period_compare_except_0(=<, [U|R], [V|S]) :-
	U =< V,
	period_compare_except_0(=<, R, S).

period_except_0_r(PERIOD, VARIABLES, CTR) :-
	check_type(dvar, PERIOD),
	collection(VARIABLES, [dvar]),
	memberchk(CTR, [=, =\=, <, >=, >, =<]),
	length(VARIABLES, N),
	PERIOD #>= 1,
	PERIOD #=< N,
	get_attr1(VARIABLES, VARS),
	period1(N, VARS, LISTS),
	period4(LISTS, 0, CTR, BOOLS),
	reverse(BOOLS, RBOOLS),
	period7(RBOOLS, 1, PERIOD, 1, EXPR),
	call(EXPR).
