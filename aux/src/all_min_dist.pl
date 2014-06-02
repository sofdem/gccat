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

ctr_date(all_min_dist,['20050508','20060803']).

ctr_origin(all_min_dist, '\\cite{Regin97}', []).

ctr_arguments(all_min_dist,
              ['MINDIST'-int                   ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(all_min_dist,
                 [vals(['MINDIST'],int(>=(1)),>,dontcare,dontcare),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  translate(['VARIABLES'^var])]).

ctr_synonyms(all_min_dist,[minimum_distance, inter_distance]).

ctr_restrictions(all_min_dist,
                 ['MINDIST' > 0                                               ,
                  size('VARIABLES') < 2 #\/ 'MINDIST' < range('VARIABLES'^var),
                  required('VARIABLES',var)                                   ]).

ctr_typical(all_min_dist,
            ['MINDIST'         > 1,
             size('VARIABLES') > 1]).

ctr_contractible(all_min_dist, [], 'VARIABLES', any).

ctr_graph(all_min_dist,
          ['VARIABLES'],
          2,
          ['CLIQUE'(<)>>collection(variables1,variables2)],
          [abs(variables1^var - variables2^var) >= 'MINDIST'],
          ['NARC' = (size('VARIABLES')*(size('VARIABLES')-1)) / 2],	
          ['ACYCLIC','NO_LOOP']).

ctr_example(all_min_dist,
            all_min_dist(2,[[var-5],[var-1],[var-9],[var-3]])).

ctr_draw_example(all_min_dist,
                 ['VARIABLES'],
                 [[[var-5],[var-1],[var-9],[var-3]]],
                 ['CLIQUE'(<)],
                 [1-[2,3,4],
                  2-[3,4],
                  3-4],
                 ['NARC'],
                 '','NARC=6',
                 []).

ctr_cond_imply(all_min_dist, soft_all_equal_max_var, [], ['N' >= size('VARIABLES')-1], [none,'VARIABLES']).

ctr_see_also(all_min_dist,
 [link(implies,        alldifferent_interval, '',                                                      []),
  link(implies,        soft_alldifferent_var, '',                                                      []),
  link(specialisation, alldifferent,          '%e, of same length, replaced by %e',                    ['line~segment',variable]),
  link(generalisation, multi_inter_distance,  '%e parameter introduced to specify capacity $\\geq$%e', ['LIMIT',1]),
  link(generalisation, disjunctive,           '%e, of same length, replaced by %e',                    ['line~segment','line~segment']),
  link(generalisation, diffn,                 '%e, of same length, replaced by %k',                    ['line~segment',orthotope]),
  link(related,        distance,              '',                                                      [])]).

ctr_key_words(all_min_dist,['value constraint'            ,
                            'decomposition'               ,
                            'acyclic'                     ,
                            'scheduling constraint'       ,
			    'sort based reformulation'    ,
                            'maximum clique'              ,
                            'bound-consistency'           ,
                            'frequency allocation problem',
                            'air traffic management'      ]).

ctr_persons(all_min_dist,['R\\\'egin J.-C.'   ,
                          'Artiouchine K.'    ,
                          'Baptiste P.'       ,
                          'Garey M. R.'       ,
                          'Johnson D. S.'     ,
                          'Simons B. B.'      ,
                          'Tarjan R. E.'      ,
                          'Quimper C.-G.'     ,
                          'L\\\'opez-Ortiz A.',
                          'Pesant G.'         ,
                          'Bron C.'           ,
                          'Kerbosch J.'       ]).

ctr_eval(all_min_dist, [      checker(all_min_dist_c),
			reformulation(all_min_dist_r)]).

ctr_sol(all_min_dist,2,0,2,8,[1-6,2-2]).
ctr_sol(all_min_dist,3,0,3,24,[1-24]).
ctr_sol(all_min_dist,4,0,4,120,[1-120]).
ctr_sol(all_min_dist,5,0,5,720,[1-720]).
ctr_sol(all_min_dist,6,0,6,5040,[1-5040]).
ctr_sol(all_min_dist,7,0,7,40320,[1-40320]).
ctr_sol(all_min_dist,8,0,8,362880,[1-362880]).

all_min_dist_c(MINDIST, []) :-
	!,
	integer(MINDIST),
	MINDIST > 0.
all_min_dist_c(MINDIST, VARIABLES) :-
	integer(MINDIST),
	MINDIST > 0,
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	(VARS = [_,_|_] ->
	    samsort(VARS, SVARS),
	    all_dist_geq_mindist(SVARS, MINDIST)
	;
	    true
	).

all_dist_geq_mindist([V1,V2|R], MINDIST) :-
	!,
	Dist is V2 - V1,
	Dist >= MINDIST,
	all_dist_geq_mindist([V2|R], MINDIST).
all_dist_geq_mindist(_, _).

all_min_dist_r(MINDIST, []) :-
	!,
	integer(MINDIST),
	MINDIST > 0.
all_min_dist_r(MINDIST, VARIABLES) :-
	integer(MINDIST),
	MINDIST > 0,
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	length(VARS, N),
	(N > 1 -> list_dvar_range(VARS, RANGE), MINDIST #< RANGE, all_min_dist1(VARIABLES, MINDIST) ; true).

all_min_dist1([], _).
all_min_dist1([[_-VAR1]|R], MINDIST) :-
	all_min_dist2(R, VAR1, MINDIST),
	all_min_dist1(R, MINDIST).

all_min_dist2([], _, _).
all_min_dist2([[_-VAR2]|R], VAR1, MINDIST) :-
	abs(VAR1-VAR2) #>= MINDIST,
	all_min_dist2(R, VAR1, MINDIST).
