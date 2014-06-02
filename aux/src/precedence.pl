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

ctr_date(precedence,['20111015']).

ctr_origin(precedence, 'Scheduling', []).

ctr_arguments(precedence,
              ['TASKS'-collection(origin-dvar, duration-dvar)]).

ctr_exchangeable(precedence,
                 [vals(['TASKS'^duration],int(>=(0)),>,dontcare,dontcare),
                  translate(['TASKS'^origin])]).

ctr_restrictions(precedence,
                 [required('TASKS',[origin,duration]),
                  'TASKS'^duration >= 0              ]).

ctr_typical(precedence,
            [size('TASKS')    >  2,
             'TASKS'^duration >= 1]).

ctr_contractible(precedence, [], 'TASKS', any).

ctr_graph(precedence,
          ['TASKS'],
          2,
          ['PATH'>>collection(tasks1,tasks2)],
          [tasks1^origin+tasks1^duration =< tasks2^origin],
          ['NARC' = size('TASKS')-1],
          []).

ctr_example(precedence,
            precedence([[origin-1, duration-3],
                        [origin-4, duration-0],
                        [origin-5, duration-2],
                        [origin-8, duration-1]])).

ctr_draw_example(precedence,
                 ['TASKS'],
                 [[[origin-1, duration-3],
                   [origin-4, duration-0],
                   [origin-5, duration-2],
                   [origin-8, duration-1]]],
                 ['PATH'],
                 [1-2,2-3,3-4],
                 ['NARC'],
                 '','NARC=3',
                 []).

ctr_see_also(precedence,
 [link('implies',                       disjunctive,      '',   []),
  link('implies (items to collection)', lex_chain_lesseq, '',   []),
  link('common keyword',                increasing,       '%k', ['order constraint'])]).

ctr_key_words(precedence,['decomposition'   ,
                          'order constraint',
                          'arc-consistency' ]).

ctr_application(precedence, [1]).

ctr_eval(precedence, [checker(precedence_c),
		      reformulation(precedence_r)]).

precedence_r(TASKS) :-
	length(TASKS, N),
	N > 1,
	collection(TASKS, [dvar,dvar_gteq(0)]),
	get_attr1(TASKS, ORIGINS  ),
	get_attr2(TASKS, DURATIONS),
	gen_precedences(ORIGINS, DURATIONS).

gen_precedences([_], [_]) :- !.
gen_precedences([O1,O2|R], [D1,D2|S]) :-
	O1+D1 #=< O2,
	gen_precedences([O2|R], [D2|S]).

precedence_c(TASKS) :-
	length(TASKS, N),
	N > 1,
	collection(TASKS, [int,int_gteq(0)]),
	get_attr1(TASKS, ORIGINS  ),
	get_attr2(TASKS, DURATIONS),
	gen_precedences_fix(ORIGINS, DURATIONS).

gen_precedences_fix([_], [_]) :- !.
gen_precedences_fix([O1,O2|R], [D1,D2|S]) :-
	E1 is O1+D1,
	E1 =< O2,
	gen_precedences_fix([O2|R], [D2|S]).
