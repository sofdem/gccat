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

ctr_date(period,['20000128','20030820','20040530','20060812']).

ctr_origin(period, 'N.~Beldiceanu', []).

ctr_arguments(period,
              ['PERIOD'-dvar                   ,
               'VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ]).

ctr_exchangeable(period,
                 [items('VARIABLES',reverse),
                  items('VARIABLES',shift),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(period,
                 ['PERIOD' >= 1                   ,
                  'PERIOD' =< size('VARIABLES')   ,
                  required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(period,
            ['PERIOD'               > 1                ,
             'PERIOD'               < size('VARIABLES'),
             size('VARIABLES')      > 2                ,
             range('VARIABLES'^var) > 1                ,
             in_list('CTR',[=])                        ]).

ctr_pure_functional_dependency(period, []).
ctr_functional_dependency(period, 1, [2,3]).

ctr_contractible(period, [in_list('CTR',[=]),'PERIOD'=1], 'VARIABLES', any).
ctr_contractible(period, [], 'VARIABLES', prefix).
ctr_contractible(period, [], 'VARIABLES', suffix).

ctr_predefined(period).

ctr_example(period,
            period(3,
                   [[var-1],[var-1],[var-4],
                    [var-1],[var-1],[var-4],
                    [var-1],[var-1]],
                   =)).

ctr_see_also(period,
 [link('generalisation', period_vectors , '%e replaced by %k',                  [variable, vector]),
  link('implies',        period_except_0, '',                                   []                ),
  link('soft variant',   period_except_0, 'value %e can match any other value', [0]               )]).

ctr_key_words(period,['predefined constraint'     ,
                      'periodic'                  ,
                      'timetabling constraint'    ,
                      'scheduling constraint'     ,
                      'sequence'                  ,
                      'border'                    ,
                      'functional dependency'     ,
		      'pure functional dependency']).

ctr_persons(period,['Beldiceanu N.',
                    'Poder E.'     ]).

ctr_eval(period, [      checker(period_c),
		  reformulation(period_r)]).

period_c(PERIOD, VARIABLES, CTR) :-
	check_type(dvar, PERIOD),
	collection(VARIABLES, [int]),
	memberchk(CTR, [=, =\=, <, >=, >, =<]),
	length(VARIABLES, N),
	PERIOD #>= 1,
	PERIOD #=< N,
	fd_min(PERIOD, PMin),
	fd_max(PERIOD, PMax),
	get_attr1(VARIABLES, VARS),
	(integer(PERIOD)     -> MinPeriod is PMin                                          ;
	 memberchk(CTR, [=]) -> VARS = [V|RVARS],
	                        compute_occ_consecutive_identical_values(RVARS, V, 1, OCCS),
	                        compute_max_sliding2(OCCS, 0, MinPeriod)                   ;
	                        MinPeriod is PMin                                          ),
	P in MinPeriod..PMax,
	indomain(P),
	PERIOD = P,
	(P = N ->
	    true
	;
	    M is min(N,P),
	    append_length(FIRSTS, REST, VARS, M),
	    period_c(REST, FIRSTS, P, CTR)
	),
	!.

compute_occ_consecutive_identical_values([V|R], Prev, Occ, Res) :-
	V = Prev,
	!,
	Occ1 is Occ+1,
	compute_occ_consecutive_identical_values(R, Prev, Occ1, Res).
compute_occ_consecutive_identical_values([V|R], Prev, Occ, [Occ|S]) :-
	V =\= Prev,
	!,
	compute_occ_consecutive_identical_values(R, V, 1, S).
compute_occ_consecutive_identical_values([], _, Occ, [Occ]).

compute_max_sliding2([],  Max, Max) :- !.
compute_max_sliding2([_], Max, Max) :- !.
compute_max_sliding2([O1,O2|R], MaxCur, Max) :-
	M is O1+O2,
	(M > MaxCur ->
	    compute_max_sliding2([O2|R], M, Max)
	;
	    compute_max_sliding2([O2|R], MaxCur, Max)
	).

period_c([], _, _, _) :- !.
period_c(VARS, FIRSTS, P, CTR) :-
	length(VARS, N),
	M is min(N,P),
	append_length(NEXTS, REST, VARS, M),
	period_compare(CTR, FIRSTS, NEXTS),
	period_c(REST, NEXTS, P, CTR).

period_compare(_, [], _) :- !.
period_compare(_, _, []) :- !.
period_compare(=, [U|R], [V|S]) :-
	!,
	U = V,
	period_compare(=, R, S).
period_compare(=\=, [U|R], [V|S]) :-
	!,
	U =\= V,
	period_compare(=\=, R, S).
period_compare(<, [U|R], [V|S]) :-
	!,
	U < V,
	period_compare(<, R, S).
period_compare(>=, [U|R], [V|S]) :-
	!,
	U >= V,
	period_compare(>=, R, S).
period_compare(>, [U|R], [V|S]) :-
	!,
	U > V,
	period_compare(>, R, S).
period_compare(=<, [U|R], [V|S]) :-
	U =< V,
	period_compare(=<, R, S).

period_r(PERIOD, VARIABLES, CTR) :-
	check_type(dvar, PERIOD),
	collection(VARIABLES, [dvar]),
	memberchk(CTR, [=, =\=, <, >=, >, =<]),
	length(VARIABLES, N),
	PERIOD #>= 1,
	PERIOD #=< N,
	get_attr1(VARIABLES, VARS),
	period1(N, VARS, LISTS),
	period4(LISTS, 1, CTR, BOOLS),
	reverse(BOOLS, RBOOLS),
	period7(RBOOLS, 1, PERIOD, 1, EXPR),
	call(EXPR).
