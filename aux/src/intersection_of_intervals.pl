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

ctr_date(intersection_of_intervals,['20140511']).

ctr_origin(intersection_of_intervals, 'Inspired by video summarization.', []).

ctr_arguments(intersection_of_intervals,
              ['INTERSECTION'-dvar,
	       'TASKS'-collection(origin-dvar, duration-dvar, end-dvar),
	       'INTERVALS'-collection(low-int, up-int)]).

ctr_synonyms(intersection_of_intervals,[intersection_between_tasks_and_intervals,
					ordered_tasks_intersection              ]).

ctr_restrictions(intersection_of_intervals,
                 ['INTERSECTION' >= 0                              ,
		  'INTERSECTION' =< sum('TASKS'^duration)          ,
		  require_at_least(2,'TASKS',[origin,duration,end]),
                  'TASKS'^duration >= 0                            ,
                  'TASKS'^origin   =< 'TASKS'^end                  ,
                  required('INTERVALS',[low,up])                   ,
		  'INTERVALS'^low  =< 'INTERVALS'^up               ]).

ctr_typical(intersection_of_intervals,
            ['INTERSECTION'          > 0,
	     size('TASKS')           > 1,
             range('TASKS'^duration) > 1,
	     size('INTERVALS')       > 1]).

ctr_predefined(intersection_of_intervals).

ctr_pure_functional_dependency(intersection_of_intervals, []).
ctr_functional_dependency(intersection_of_intervals, 1, [2,3]).

ctr_example(intersection_of_intervals,
            intersection_of_intervals(3,
				      [[origin-2, duration-2, end-4],
                                       [origin-7, duration-2, end-9],
                                       [origin-9, duration-0, end-9]],
				      [[low-1, up-3],
				       [low-5, up-5],
                                       [low-8, up-9]])).

ctr_key_words(intersection_of_intervals,['scheduling constraint',
                                         'zero-duration task'   ]).

ctr_eval(intersection_of_intervals, [reformulation(intersection_of_intervals_r)]).

intersection_of_intervals_r(INTERSECTION, TASKS, INTERVALS) :-
	check_type(dvar_gteq(0), INTERSECTION),
	collection(TASKS, [dvar,dvar_gteq(0),dvar]),
	collection(INTERVALS, [dvar,dvar]),
	intersection_of_intervals1(TASKS),
	intersection_of_intervals2(INTERVALS),
	intersection_of_intervals3(TASKS, INTERVALS, INTER),
	call(INTERSECTION #= INTER).

intersection_of_intervals1([[origin-O,duration-D,end-E]]) :- !,
	E #= O+D.
intersection_of_intervals1([[origin-O1,duration-D1,end-E1],[origin-O2,duration-D2,end-E2]|R]) :-
	E1 #= O1+D1,
	E1 #=< O2,
	intersection_of_intervals1([[origin-O2,duration-D2,end-E2]|R]).

intersection_of_intervals2([[low-L,up-U]]) :- !,
	L #=< U.
intersection_of_intervals2([[low-L1,up-U1],[low-L2,up-U2]|R]) :-
	L1 #=< U1,
	U1 #< L2,
	intersection_of_intervals2([[low-L2,up-U2]|R]).

intersection_of_intervals3([], _, 0) :- !.
intersection_of_intervals3([TASK|R], INTERVALS, I+S) :-
	intersection_of_intervals4(INTERVALS, TASK, INTER),
	call(I #= INTER),
	intersection_of_intervals3(R, INTERVALS, S).

intersection_of_intervals4([], _, 0) :- !.
intersection_of_intervals4([[low-L,up-U]|R], [origin-O,duration-D,end-E], INTER+S) :-
	INTER #= max(min(U,E-1)-max(L,O)+1,0),
	intersection_of_intervals4(R, [origin-O,duration-D,end-E], S).
