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

ctr_date(multi_inter_distance,['20110814']).

ctr_origin(multi_inter_distance, '\\cite{OuelletQuimper11}', []).

ctr_arguments(multi_inter_distance,
              ['VARIABLES'-collection(var-dvar),
               'LIMIT'-int                     ,
               'DIST'-int                      ]).

ctr_exchangeable(multi_inter_distance,
                 [items('VARIABLES',all),
                  translate(['VARIABLES'^var]),
                  vals(['LIMIT'],int,<,dontcare,dontcare),
                  vals(['MINDIST'],int(>=(1)),>,dontcare,dontcare)]).

ctr_synonyms(multi_inter_distance,[multi_all_min_distance,
                                   multi_all_min_dist    ,
                                   sliding_atmost        ,
                                   atmost_sliding        ]).

ctr_restrictions(multi_inter_distance,
                 [required('VARIABLES',var),
                  'LIMIT' > 0              ,
                  'DIST'  > 0              ]).

ctr_typical(multi_inter_distance,
            ['LIMIT' > 1                     ,
             'LIMIT' < size('VARIABLES')     ,
             'DIST'  > 1                     ,
             'DIST'  < range('VARIABLES'^var)]).

ctr_predefined(multi_inter_distance).

ctr_contractible(multi_inter_distance, [], 'VARIABLES', any).

ctr_example(multi_inter_distance,
            multi_inter_distance([[var-4],[var-0],[var-9],[var-4],[var-7]], 2, 3)).

ctr_see_also(multi_inter_distance,
 [link(specialisation, all_min_dist,       '%e parameter set to %e',                                        ['LIMIT',1]),
  link(specialisation, cardinality_atmost, 'window of %e consecutive values replaced by window of size %e', ['DIST',1]),
  link(generalisation, cumulative,         '%e, of same length, replaced by %e', ['line~segment','line~segment'])]).

ctr_key_words(multi_inter_distance,['predefined constraint' ,
				    'value constraint'      ,
                                    'at most'               ,
                                    'scheduling constraint' ,
                                    'bound-consistency'     ,
                                    'air traffic management']).

ctr_persons(multi_inter_distance,['Ouellet P.'   ,
                                  'Quimper C.-G.']).

ctr_eval(multi_inter_distance, [reformulation(multi_inter_distance_r)]).

multi_inter_distance_r([], LIMIT, DIST) :-
	!,
	integer(LIMIT),
	integer(DIST),
	LIMIT > 0,
	DIST  > 0.
multi_inter_distance_r(VARIABLES, LIMIT, DIST) :-
	collection(VARIABLES, [dvar]),
	integer(LIMIT),
	integer(DIST),
	LIMIT > 0,
	DIST  > 0,
	get_attr1(VARIABLES, ORIGINS),
    length(VARIABLES, N),
    length(DURATIONS, N),
    length(ENDS,      N),
    length(HEIGHTS,   N),
    domain(DURATIONS, DIST, DIST),
    domain(HEIGHTS, 1, 1),
    ori_dur_end(ORIGINS, DURATIONS, ENDS),
    gen_cum_tasks(ORIGINS, DURATIONS, ENDS, HEIGHTS, 1, Tasks),
    cumulative(Tasks, [limit(LIMIT)]).
