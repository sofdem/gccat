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

ctr_date(disjunctive,['20050506','20060808']).

ctr_origin(disjunctive, '\\cite{Carlier82}', []).

ctr_arguments(disjunctive,
              ['TASKS'-collection(origin-dvar, duration-dvar)]).

ctr_exchangeable(disjunctive,
                 [items('TASKS',all),
                  vals(['TASKS'^duration],int(>=(0)),>,dontcare,dontcare),
                  translate(['TASKS'^origin])]).

ctr_synonyms(disjunctive,[one_machine]).

ctr_restrictions(disjunctive,
                 [required('TASKS',[origin,duration]),
                  'TASKS'^duration >= 0              ]).

ctr_typical(disjunctive,
            [size('TASKS')    >  2,
             'TASKS'^duration >= 1]).

ctr_contractible(disjunctive, [], 'TASKS', any).

ctr_graph(disjunctive,
          ['TASKS'],
          2,
          ['CLIQUE'(<)>>collection(tasks1,tasks2)],
          [tasks1^duration = 0                              #\/
           tasks2^duration = 0                              #\/
           tasks1^origin + tasks1^duration =< tasks2^origin #\/
           tasks2^origin + tasks2^duration =< tasks1^origin   ],
          ['NARC' = (size('TASKS')*(size('TASKS')-1)) / 2],
          []).

ctr_example(disjunctive,
            disjunctive([[origin-1, duration-3],
                         [origin-2, duration-0],
                         [origin-7, duration-2],
                         [origin-4, duration-1]])).

ctr_draw_example(disjunctive,
                 ['TASKS'],
                 [[[origin-1, duration-3],
                   [origin-2, duration-0],
                   [origin-7, duration-2],
                   [origin-4, duration-1]]],
                 ['CLIQUE'(<)],
                 [1-[2,3,4],2-[3,4],3-4],
                 ['NARC'],
                 '','NARC=6',
                 []).

ctr_cond_imply(disjunctive, alldifferent,     [minval('TASKS'^duration) > 0], [], ['TASKS'^origin]).
ctr_cond_imply(disjunctive, alldifferent_cst, [minval('TASKS'^duration) > 0], [], same            ).

ctr_see_also(disjunctive,
 [link('common keyword', calendar,                   '%k',                                                 ['scheduling constraint']),
  link('common keyword', disj,                       '%k',                                                 ['scheduling constraint']),
  link('common keyword', disjunctive_or_same_start,  '%k',                                                 ['scheduling constraint']),
  link('common keyword', disjunctive_or_same_end,    '%k',                                                 ['scheduling constraint']),
  link('implies',        disjunctive_or_same_start,  '',                                                   []),
  link('implies',        disjunctive_or_same_end,    '',                                                   []),
  link('implied by',     precedence,                 '',                                                   []),
  link(specialisation,   alldifferent,               '%e replaced by %e',                                  [task,variable]),
  link(specialisation,   all_min_dist,               '%e replaced by %e, of same length',                  ['line~segment','line~segment']),
  link(generalisation,   cumulative,                 '%e %e and %e %e are not necessarly all equal to %e', [task,heights,resource,limit,1]),
  link(generalisation,   diffn,                      '%e of %e %e replaced by %k',                         [task,heigth,1,orthotope])]).

ctr_key_words(disjunctive,['core'                                       ,
                           'scheduling constraint'                      ,
                           'resource constraint'                        ,
                           'decomposition'                              ,
                           'disjunction'                                ,
			   'sort based reformulation'                   ,
                           'maximum clique'                             ,
                           'sequencing with release times and deadlines',
                           'sequence dependent set-up'                  ,
                           'compulsory part'                            ,
                           'constructive disjunction'                   ,
                           'Phi-tree'                                   ,
                           'zero-duration task'                         ]).

ctr_persons(disjunctive,['Jackson J. R.'   ,
                         'Carlier J.'      ,
                         'Pinson E.'       ,
                         'Vil\\\'im P.'    ,
                         'P\\\'eridy L.'   ,
                         'Rivreau D.'      ,
                         'Baptiste P.'     ,
                         'Le Pape C.'      ,
                         'Artiouchine K.'  ,
                         'Bron C.'         ,
                         'Kerbosch J.'     ]).

ctr_application(disjunctive, [1]).

ctr_eval(disjunctive, [checker(disjunctive_c),
		       builtin(disjunctive_b)]).

disjunctive_b([]) :- !.
disjunctive_b(TASKS) :-
    collection(TASKS, [dvar,dvar_gteq(0)]),
    length(TASKS, N),
    (N = 1 ->
	true
    ;
        get_attr1(TASKS, ORIGINS),
	get_attr2(TASKS, DURATIONS),
	length(ENDS, N),
        ori_dur_end(ORIGINS, DURATIONS, ENDS),
	length(HEIGHTS, N),
	domain(HEIGHTS, 1, 1),
	gen_cum_tasks(ORIGINS, DURATIONS, ENDS, HEIGHTS, 1, Tasks),
	cumulative(Tasks, [limit(1)])
    ).

disjunctive_c([]) :- !.
disjunctive_c(TASKS) :-
    collection(TASKS, [int,int_gteq(0)]),
    (TASKS = [_] ->
	true
    ;
	get_attr12_diff20(TASKS, ORIS_DURS),
	keysort(ORIS_DURS, SORTED_NON_ZERO_TASKS),
	disjunctive_check_prec(SORTED_NON_ZERO_TASKS)
    ).

disjunctive_check_prec([]) :- !.
disjunctive_check_prec([_]) :- !.
disjunctive_check_prec([O1-D1,O2-D2|R]) :-
    E1 is O1+D1,
    E1 =< O2,
    disjunctive_check_prec([O2-D2|R]).
