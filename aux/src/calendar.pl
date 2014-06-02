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

ctr_date(calendar,['20061014']).

ctr_origin(calendar, '\\cite{BeldiceanuR00}', []).

ctr_types(calendar,
          ['UNAVAILABILITIES'-collection(low-int, up-int)]).

ctr_arguments(calendar,
              ['INSTANTS'-collection(machine-dvar, virtual-dvar, ireal-dvar, flagend-int),
               'MACHINES'-collection(id-int, cal-'UNAVAILABILITIES')                     ]).

ctr_exchangeable(calendar,
                 [items('INSTANTS',all),
                  items('MACHINES',all)]).

ctr_restrictions(calendar,
                 [required('UNAVAILABILITIES',[low,up])               ,
                  'UNAVAILABILITIES'^low =< 'UNAVAILABILITIES'^up     ,
                  required('INSTANTS',[machine,virtual,ireal,flagend]),
                  in_attr('INSTANTS',machine,'MACHINES',id)           ,
                  'INSTANTS'^flagend >= 0                             ,
                  'INSTANTS'^flagend =< 1                             ,
                  size('MACHINES')   >  0                             ,
                  required('MACHINES',[id,cal])                       ,
                  distinct('MACHINES',id)                             ]).

ctr_typical(calendar,
            [size('INSTANTS') > 1,
             size('MACHINES') > 1]).

ctr_contractible(calendar, [], 'INSTANTS', any).

ctr_predefined(calendar).

ctr_example(calendar,
            calendar([[machine-1, virtual-2, ireal-3, flagend-0],
                      [machine-1, virtual-5, ireal-6, flagend-1],
                      [machine-2, virtual-4, ireal-5, flagend-0],
                      [machine-2, virtual-6, ireal-9, flagend-1],
                      [machine-3, virtual-2, ireal-2, flagend-0],
                      [machine-3, virtual-5, ireal-5, flagend-1],
                      [machine-4, virtual-2, ireal-2, flagend-0],
                      [machine-4, virtual-7, ireal-9, flagend-1]],
                     [[id-1, cal-[[low-2, up-2],[low-6, up-7]]],
                      [id-2, cal-[[low-2, up-2],[low-6, up-7]]],
                      [id-3, cal-[                           ]],
                      [id-4, cal-[[low-3, up-4]              ]]])).

ctr_see_also(calendar,
 [link('common keyword', disjunctive, '%k',         ['scheduling constraint'],'\\\\ '),
  link('common keyword', cumulative,  '%k',         ['scheduling constraint']),
  link('common keyword', cumulatives, '%k',         ['scheduling with machine choice, calendars and preemption']),
  link('common keyword', diffn,       '%k,\\\\ %k', ['multi-site employee scheduling with calendar constraints',
                                                     'scheduling with machine choice, calendars and preemption']),
  link('common keyword', geost,       '%k,\\\\ %k', ['multi-site employee scheduling with calendar constraints',
                                                     'scheduling with machine choice, calendars and preemption'])]).

ctr_key_words(calendar,['predefined constraint'                                   ,
                        'channelling constraint'                                  ,
                        'temporal constraint'                                     ,
                        'scheduling constraint'                                   ,
                        'multi-site employee scheduling with calendar constraints',
                        'scheduling with machine choice, calendars and preemption',
                        'assignment dimension'                                    ]).

ctr_persons(calendar,['Beldiceanu N.']).

ctr_application(calendar, [1]).

ctr_eval(calendar, [reformulation(calendar_r)]).

calendar_r(INSTANTS, MACHINES) :-
	collection(INSTANTS, [dvar,dvar,dvar,int(0,1)]),
	collection(MACHINES, [int, col([int,int])]),
	length(MACHINES, M),
	M > 0,
	get_attr1(MACHINES, IDS),
	all_different(IDS),
	calendar_low_up(MACHINES),
	(INSTANTS=[] ->
		true
	;
            calendar_in_attr(INSTANTS, IDS),
		calendar_normalize(MACHINES, MACHINESN),
		calendar_gen(INSTANTS, MACHINESN)
	).

calendar_in_attr([], _).
calendar_in_attr([[_-M|_]|R], IDS) :-
	build_or_var_in_values(IDS, M, TERM),
	call(TERM),
	calendar_in_attr(R, IDS).

calendar_low_up([]).
calendar_low_up([[_, _-CAL]|R]) :-
	calendar_low_up1(CAL),
	calendar_low_up(R).

calendar_low_up1([]).
calendar_low_up1([[_-L,_-U]|R]) :-
	L =< U,
	calendar_low_up1(R).

calendar_normalize([], []).
calendar_normalize([[id-ID, cal-CAL]|R], [[id-ID, cal-MERGED_CAL]|S]) :-
	calendar_merge_intervals(CAL, MERGED_CAL),
	calendar_normalize(R, S).

calendar_merge_intervals(List, NewList) :-
	(   foreach([low-L,up-U],List),
	    fromto([],S1,S3,Set)
	do  fdset_interval(S2, L, U),
	    fdset_union(S1, S2, S3)
	),
	(   foreach([A|B],Set),
	    foreach([low-A,up-B],NewList)
	do  true
	).

calendar_gen([], _).
calendar_gen([[machine-M, virtual-V, ireal-R, flagend-F]|T], CALENDARS) :-
	calendar_gen(CALENDARS, M, V, R, F),
	calendar_gen(T, CALENDARS).

calendar_gen([], _, _, _, _).
calendar_gen([[id-I,cal-C]|S], M, V, R, F) :-
	calendar_gen(C, 1, 0, I, M, V, R, F),
	calendar_gen(S, M, V, R, F).

calendar_gen([], 1, 0, I, M, V, R, _F) :-								% CASE 1: no unavailability at all
	M #= I #<=> M #= I #/\ R #= V.								% equality between real and virtual coordinates
calendar_gen([[low-L,up-U]|S], 1, 0, I, M, V, R, F) :-					% CASE 2: real coordinate before first unavailability
	LF is L+F,
	M #= I #/\ R #< LF #<=> M #= I #/\ R #= V,						% equality between real and virtual coordinates
	calendar_gen([[low-L,up-U]|S], 0, 0, I, M, V, R, F).
calendar_gen([[low-K,up-U],[low-L,up-W]|S], 0, Sum, I, M, V, R, F) :-			% CASE 3: real coordinate between two consecutive unavailabilities
	NSum is Sum + U - K + 1,
	KF is K+F,
	UF is U+F,
	LF is L+F,
	R in KF..UF #=> M #\= I,									% real coordinate cannot be within an unavailibility
	M #= I #/\ R #> UF #/\ R #< LF #<=> M #= I #/\ R #= V+NSum,				% real coordinate = virtual coordinate + sum of unavailibilities
	calendar_gen([[low-L,up-W]|S], 0, NSum, I, M, V, R, F).				% before real coordinate
calendar_gen([[low-L,up-U]], 0, Sum, I, M, V, R, F) :-					% CASE 4: real coordinate after last unavailability
	NSum is Sum + U - L + 1,
	LF is L+F,
	UF is U+F,
	R in LF..UF #=> M #\= I,									% real coordinate cannot be within an unavailibility
	M #= I #/\ R #> UF #<=> M #= I #/\ R #= V+NSum.						% real coordinate = virtual coordinate + sum of all unavailibilities

