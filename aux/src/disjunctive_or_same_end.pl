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

ctr_date(disjunctive_or_same_end,['20120205']).

ctr_origin(disjunctive_or_same_end, 'Scheduling.', []).

ctr_arguments(disjunctive_or_same_end,
              ['TASKS'-collection(origin-dvar, duration-dvar)]).

ctr_exchangeable(disjunctive_or_same_end,
                 [items('TASKS',all),
                  vals(['TASKS'^duration],int(>=(0)),>,dontcare,dontcare),
                  translate(['TASKS'^origin])]).

ctr_synonyms(disjunctive_or_same_end,[same_end_or_disjunctive, non_overlap_or_same_end, same_end_or_non_overlap]).

ctr_restrictions(disjunctive_or_same_end,
                 [required('TASKS',[origin,duration]),
                  'TASKS'^duration >= 0              ]).

ctr_typical(disjunctive_or_same_end,
            [size('TASKS')    >  2,
             'TASKS'^duration >= 1]).

ctr_contractible(disjunctive_or_same_end, [], 'TASKS', any).

ctr_graph(disjunctive_or_same_end,
          ['TASKS'],
          2,
          ['CLIQUE'(<)>>collection(tasks1,tasks2)],
          [tasks1^duration = 0                              #\/
           tasks2^duration = 0                              #\/
           tasks1^origin + tasks1^duration =< tasks2^origin #\/
           tasks2^origin + tasks2^duration =< tasks1^origin #\/
	   tasks1^origin + tasks1^duration = tasks2^origin + tasks2^duration],
          ['NARC' = (size('TASKS')*(size('TASKS')-1)) / 2],
          []).

ctr_example(disjunctive_or_same_end,
            disjunctive_or_same_end([[origin-4, duration-3],
                                     [origin-7, duration-2],
				     [origin-5, duration-2]])).

ctr_draw_example(disjunctive_or_same_end,
                 ['TASKS'],
                 [[[origin-4, duration-3],
                   [origin-7, duration-2],
                   [origin-5, duration-2]]],
                 ['CLIQUE'(<)],
                 [1-[2,3],2-[3]],
                 ['NARC'],
                 '','NARC=3',
                 []).

ctr_see_also(disjunctive_or_same_end,
 [link('implied by',     disjunctive,               '',   []                       ),
  link('common keyword', disjunctive,               '%k', ['scheduling constraint']),
  link('common keyword', disjunctive_or_same_start, '%k', ['scheduling constraint'])]).

ctr_key_words(disjunctive_or_same_end,['scheduling constraint',
                                       'resource constraint'  ,
                                       'decomposition'        ,
                                       'disjunction'          ,
                                       'zero-duration task'   ]).

ctr_application(disjunctive_or_same_end, [1]).

ctr_eval(disjunctive_or_same_end, [checker(disjunctive_or_same_end_c),
				   reformulation(disjunctive_or_same_end_r)]).

disjunctive_or_same_end_r([]) :- !.
disjunctive_or_same_end_r(TASKS) :-
    collection(TASKS, [dvar,dvar_gteq(0)]),
    get_attr1(TASKS, ORIGINS),
    get_attr2(TASKS, DURATIONS),
    disjunctive_or_same_end1(ORIGINS, DURATIONS).

disjunctive_or_same_end1([], []).
disjunctive_or_same_end1([ORI|RO], [DUR|RD]) :-
    disjunctive_or_same_end2(RO, RD, ORI, DUR),
    disjunctive_or_same_end1(RO, RD).

disjunctive_or_same_end2([], [], _, _).
disjunctive_or_same_end2([O2|RO], [D2|RD], O1, D1) :-
    D1 #= 0 #\/ D2 #= 0 #\/ O1+D1 #=< O2 #\/ O2+D2 #=< O1 #\/ O1+D1 #= O2+D2,
    disjunctive_or_same_end2(RO, RD, O1, D1).

% create end and -duration
% sort in increasing order   8,-6   8,-1  10,-5  10,-3  10,-1
% reverse                   10,-1  10,-3  10,-5   8,-1   8,-6
% check                     skip   skip  10-5>=8 skip   end
disjunctive_or_same_end_c([]) :- !.
disjunctive_or_same_end_c(TASKS) :-
    collection(TASKS, [int,int_gteq(0)]),
    (TASKS = [_] ->
	true
    ;
        get_attr12_diff20_end(TASKS, ENDS_NEGDURS),
        sort(ENDS_NEGDURS, SORTED_NON_ZERO_TASKS), % can remove duplicate since keep one representative
	reverse(SORTED_NON_ZERO_TASKS, RSORTED_NON_ZERO_TASKS),
	disjunctive_or_same_end_check_prec(RSORTED_NON_ZERO_TASKS)
    ).

disjunctive_or_same_end_check_prec([]) :- !.
disjunctive_or_same_end_check_prec([_]) :- !.
disjunctive_or_same_end_check_prec([E-_D1,E-D2|R]) :- !, % -D1>-D2 (D1<D2): important that biggest duration kept at the end when same end
    disjunctive_or_same_end_check_prec([E-D2|R]).
disjunctive_or_same_end_check_prec([E1-D1,E2-D2|R]) :- !,
    O1 is E1+D1,
    O1 >= E2,
    disjunctive_or_same_end_check_prec([E2-D2|R]).
