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

ctr_date(equilibrium,['20130714']).

ctr_origin(equilibrium, 'Inspired by the Irish Collegiate Programming Competition 2012 (equilibrium index)', []).

ctr_arguments(equilibrium,
              ['VARIABLES'-collection(var-dvar),
	       'INDEX1'-dvar,
	       'INDEX2'-dvar,
	       'EPSILON'-int,
	       'COEF1'-int,
	       'COEF2'-int,
	       'TOLERANCE'-int,
               'CTR'-atom]).

ctr_synonyms(equilibrium,[balanced]).

ctr_restrictions(equilibrium,
                 [size('VARIABLES') >= 1                ,
		  'INDEX1'          >= 1                ,
                  'INDEX1'          =< size('VARIABLES'),
                  'INDEX2'          >= 1                ,
                  'INDEX2'          =< size('VARIABLES'),
		  'INDEX1'          =< 'INDEX2'         ,
		  'EPSILON'         >= 0                ,
		  'EPSILON'         =< 2                ,
                  'EPSILON'         = 'INDEX2'-'INDEX1' ,
		  'COEF1'           =\= 0               ,
		  'COEF2'           =\= 0               ,
		  'TOLERANCE'       >=  0               ,
                  in_list('CTR',[among_diff_0               ,
				 and                        ,
				 change                     ,
				 deepest_valley             ,
				 highest_peak               ,
				 increasing_nvalue          ,
				 inflexion                  ,
				 longest_change             ,
				 longest_decreasing_sequence,
				 longest_increasing_sequence,
				 max_decreasing_slope       ,
				 max_increasing_slope       ,
				 min_decreasing_slope       ,
				 min_increasing_slope       ,
				 min_width_peak             ,
				 min_width_valley           ,
				 peak                       ,
				 sum_ctr                    ,
				 valley                     ])]).

ctr_typical(equilibrium,
            [size('VARIABLES') >  2                ,
	     'INDEX1'          >  1                ,
             'INDEX1'          <  size('VARIABLES'),
             'INDEX2'          >  1                ,
             'INDEX2'          <  size('VARIABLES'),
	     'COEF1'           =  1                ,
	     'COEF2'           =  1                ,
	     'EPSILON'         =  1                ,
	     'TOLERANCE'       =  0                ]).

ctr_predefined(equilibrium).

ctr_example(equilibrium,
            [equilibrium([[var-4],[var-4],[var-3],[var-6],[var-2]],2,4,2,1,1,0,sum_ctr),
	     equilibrium([[var-(-2)],[var-5],[var-(-2)],[var-6],[var-(-1)],[var-0],[var-(-3)],[var-5],[var-(-7)],[var-6],[var-(-1)],[var-7],[var-0]],5,5,0,1,1,0,sum_ctr),
	     equilibrium([[var-(-2)],[var-5],[var-(-2)],[var-6],[var-(-1)],[var-0],[var-(-3)],[var-5],[var-(-7)],[var-6],[var-(-1)],[var-7],[var-0]],11,11,0,1,1,0,sum_ctr),
	     equilibrium([[var-0],[var-3],[var-2],[var-6],[var-2],[var-2],[var-5],[var-8],[var-7],[var-6],[var-7],[var-3]],5,7,2,1,1,0,peak),
	     equilibrium([[var-0],[var-5],[var-3],[var-8],[var-2],[var-2],[var-5],[var-5],[var-8],[var-7],[var-2],[var-7],[var-3]],7,7,0,1,1,0,change)]).

ctr_see_also(equilibrium,
 [link('root concept', balance, '', [])]).

ctr_key_words(equilibrium,['automaton with counters']).

ctr_eval(equilibrium, [       checker(equilibrium_c),
		        reformulation(equilibrium_r),
		       ground_typical(equilibrium_t)]).

equilibrium_t(VARIABLES, _, _, _, _, _, _, CTR) :-
	(CTR = among_diff_0                ->                among_diff_0(C, VARIABLES),      C #> 0;
	 CTR = change                      ->                      change(C, VARIABLES, =\=), C #> 0;
	 CTR = increasing_nvalue           ->           increasing_nvalue(C, VARIABLES),      C #> 0;
	 CTR = inflexion                   ->                   inflexion(C, VARIABLES),      C #> 0;
	 CTR = longest_change              ->              longest_change(C, VARIABLES, =\=), C #> 0;
	 CTR = longest_decreasing_sequence -> longest_decreasing_sequence(C, VARIABLES),      C #> 1;
	 CTR = longest_increasing_sequence -> longest_increasing_sequence(C, VARIABLES),      C #> 1;
	 CTR = max_decreasing_slope        ->        max_decreasing_slope(C, VARIABLES),      C #> 1;
	 CTR = max_increasing_slope        ->        max_increasing_slope(C, VARIABLES),      C #> 1;
	 CTR = min_decreasing_slope        ->        min_decreasing_slope(C, VARIABLES),      C #> 1;
	 CTR = min_increasing_slope        ->        min_increasing_slope(C, VARIABLES),      C #> 1;
	 CTR = min_width_peak              ->              min_width_peak(C, VARIABLES),      C #> 0;
	 CTR = min_width_valley            ->            min_width_valley(C, VARIABLES),      C #> 0;
	 CTR = peak                        ->                        peak(C, VARIABLES),      C #> 0;
	 CTR = sum_ctr                     ->                     sum_ctr(C, VARIABLES),      C #> 0;
	 CTR = valley                      ->                      valley(C, VARIABLES),      C #> 0; true).

equilibrium_c(VARIABLES, INDEX1, INDEX2, EPSILON, COEF1, COEF2, TOLERANCE, CTR) :-
	integer(INDEX1),
	integer(INDEX2),
	!,
        collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	length(VARS, N),
	N         >= 1          ,
        INDEX1    >= 1          ,
        INDEX1    =< N          ,
        INDEX2    >= 1          ,
        INDEX2    =< N          ,
        INDEX1    =< INDEX2     ,
        EPSILON   >= 0          ,
        EPSILON   =< 2          ,
	EPSILON is INDEX2-INDEX1,
        TOLERANCE >= 0          ,
	memberchk(CTR, [among_diff_0               ,
			and                        ,
			change                     ,
			deepest_valley             ,
			highest_peak               ,
			increasing_nvalue          ,
			inflexion                  ,
			longest_change             ,
			longest_decreasing_sequence,
			longest_increasing_sequence,
			max_decreasing_slope       ,
			max_increasing_slope       ,
                        min_decreasing_slope       ,
                        min_increasing_slope       ,
			min_width_peak             ,
			min_width_valley           ,
			peak                       ,
			sum_ctr                    ,
			valley                     ]),
	(CTR = among_diff_0 ->
	    prefix_length(VARS, PVARS, INDEX1),
	    among_diff_0_c(PVARS, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    among_diff_0_c(SVARS, 0, C2)
	;
	 CTR = and ->
	    prefix_length(VARIABLES, PVARS, INDEX1),
	    C1 in 0..1, and_c(C1, PVARS),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARIABLES, PREF),
	    C2 in 0..1, and_c(C2, SVARS)
	;
	 CTR = change ->
	    prefix_length(VARS, PVARS, INDEX1),
	    change_neq_c(PVARS, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    change_neq_c(SVARS, 0, C2)
	;
	 CTR = deepest_valley ->
	    MAXINT = 1000000,
	    prefix_length(VARS, PVARS, INDEX1),
	    deepest_valley_c(PVARS, s, MAXINT, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    deepest_valley_c(SVARS, s, MAXINT, C2)
	;
	 CTR = highest_peak ->
	    MININT = -1000000,
	    prefix_length(VARS, PVARS, INDEX1),
	    highest_peak_c(PVARS, s, MININT, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    highest_peak_c(SVARS, s, MININT, C2)
	;
	 CTR = increasing_nvalue ->
	    prefix_length(VARS, PVARS, INDEX1),
	    PVARS = [PVAR|RPVARS],
	    increasing_nvalue_c(RPVARS, PVAR, 1, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    SVARS = [SVAR|RSVARS],
	    increasing_nvalue_c(RSVARS, SVAR, 1, C2)
	;
	 CTR = inflexion ->
	    prefix_length(VARS, PVARS, INDEX1),
	    inflexion_c(PVARS, s, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    inflexion_c(SVARS, s, 0, C2)
	;
	 CTR = longest_change ->
	    prefix_length(VARS, PVARS, INDEX1),
	    longest_change_neq_c(PVARS, 0, 1, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    longest_change_neq_c(SVARS, 0, 1, C2)
	;
	 CTR = longest_decreasing_sequence ->
	    prefix_length(VARS, PVARS, INDEX1),
	    longest_decreasing_sequence_c(PVARS, s, 0, 0, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    longest_decreasing_sequence_c(SVARS, s, 0, 0, 0, C2)
	;
	 CTR = longest_increasing_sequence ->
	    prefix_length(VARS, PVARS, INDEX1),
	    longest_increasing_sequence_c(PVARS, s, 0, 0, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    longest_increasing_sequence_c(SVARS, s, 0, 0, 0, C2)
	;
	 CTR = max_decreasing_slope ->
	    prefix_length(VARS, PVARS, INDEX1),
	    max_decreasing_slope_c1(PVARS, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    max_decreasing_slope_c1(SVARS, 0, C2)
	;
	 CTR = max_increasing_slope ->
	    prefix_length(VARS, PVARS, INDEX1),
	    max_increasing_slope_c1(PVARS, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    max_increasing_slope_c1(SVARS, 0, C2)
	;
	 CTR = min_decreasing_slope ->
	    prefix_length(VARS, PVARS, INDEX1),
	    min_decreasing_slope_c1(PVARS, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    min_decreasing_slope_c1(SVARS, 0, C2)
	;
	 CTR = min_increasing_slope ->
	    prefix_length(VARS, PVARS, INDEX1),
	    min_increasing_slope_c1(PVARS, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    min_increasing_slope_c1(SVARS, 0, C2)
	;
	 CTR = min_width_peak ->
	    prefix_length(VARS, PVARS, INDEX1),
            min_width_peak_c(PVARS, s, 1, 0, 0, 0, INDEX1, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    LEN2 is N-INDEX1+1,
            min_width_peak_c(SVARS, s, 1, 0, 0, 0, LEN2, C2)
	;
	 CTR = min_width_valley ->
	    prefix_length(VARS, PVARS, INDEX1),
	    min_width_valley_c(PVARS, s, 1, 0, 0, 0, INDEX1, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    LEN2 is N-INDEX1+1,
            min_width_valley_c(SVARS, s, 1, 0, 0, 0, LEN2, C2)
	;
	 CTR = peak ->
	    prefix_length(VARS, PVARS, INDEX1),
	    peak_c(PVARS, s, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    peak_c(SVARS, s, 0, C2)
	;
	 CTR = sum_ctr ->
	    sum_c(INDEX1, 0, VARS, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    get_sum(SVARS, C2)
	;
	 CTR = valley ->
	    prefix_length(VARS, PVARS, INDEX1),
	    valley_c(PVARS, s, 0, C1),
	    PREF is INDEX2-1,
	    append_length(SVARS, VARS, PREF),
	    valley_c(SVARS, s, 0, C2)
	),
	CC1 is COEF1*C1,
	CC2 is COEF2*C2,
	DIF is abs(CC1-CC2),
	DIF =< TOLERANCE.

% construct the values of INDEX1, INDEX2 if not given (used in the context of learning)
equilibrium_c(VARIABLES, INDEX1, INDEX2, EPSILON, COEF1, COEF2, TOLERANCE, CTR) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	length(VARS, N),
	N         >= 1,
        EPSILON   >= 0,
        EPSILON   =< 2,
        TOLERANCE >= 0,
	check_type(dvar(1,N), INDEX1),
	check_type(dvar(1,N), INDEX2),
	memberchk(CTR, [among_diff_0               ,
			and                        ,
			change                     ,
			deepest_valley             ,
			highest_peak               ,
			increasing_nvalue          ,
			inflexion                  ,
			longest_change             ,
			longest_decreasing_sequence,
			longest_increasing_sequence,
			max_decreasing_slope       ,
			max_increasing_slope       ,
			min_decreasing_slope       ,
			min_increasing_slope       ,
			min_width_peak             ,
			min_width_valley           ,
			peak                       ,
			sum_ctr                    ,
			valley                     ]),
	(CTR = among_diff_0                -> equilibrium_among_diff_0(VARS, PREFIX, SUFFIX)               ;
	 CTR = and                         -> equilibrium_and(VARS, PREFIX, SUFFIX)                        ;
	 CTR = change                      -> equilibrium_change(VARS, PREFIX, SUFFIX)                     ;
	 CTR = deepest_valley              -> equilibrium_deepest_valley(VARS, PREFIX, SUFFIX)             ;
	 CTR = highest_peak                -> equilibrium_highest_peak(VARS, PREFIX, SUFFIX)               ;
	 CTR = increasing_nvalue           -> equilibrium_increasing_nvalue(VARS, PREFIX, SUFFIX)          ;
	 CTR = inflexion                   -> equilibrium_inflexion(VARS, PREFIX, SUFFIX)                  ;
	 CTR = longest_change              -> equilibrium_longest_change(VARS, PREFIX, SUFFIX)             ;
	 CTR = longest_decreasing_sequence -> equilibrium_longest_decreasing_sequence(VARS, PREFIX, SUFFIX);
	 CTR = longest_increasing_sequence -> equilibrium_longest_increasing_sequence(VARS, PREFIX, SUFFIX);
	 CTR = max_decreasing_slope        -> equilibrium_max_decreasing_slope(VARS, PREFIX, SUFFIX)       ;
	 CTR = max_increasing_slope        -> equilibrium_max_increasing_slope(VARS, PREFIX, SUFFIX)       ;
	 CTR = min_decreasing_slope        -> equilibrium_min_decreasing_slope(VARS, PREFIX, SUFFIX)       ;
	 CTR = min_increasing_slope        -> equilibrium_min_increasing_slope(VARS, PREFIX, SUFFIX)       ;
         CTR = min_width_peak              -> equilibrium_min_width_peak(VARS, N, PREFIX, SUFFIX)          ;
         CTR = min_width_valley            -> equilibrium_min_width_valley(VARS, N, PREFIX, SUFFIX)        ;
	 CTR = peak                        -> equilibrium_peak(VARS, PREFIX, SUFFIX)                       ;
	 CTR = sum_ctr                     -> equilibrium_sum_ctr(VARS, 1, PREFIX, SUFFIX)                 ;
	 CTR = valley                      -> equilibrium_valley(VARS, PREFIX, SUFFIX)                     ),
	write(prefix(PREFIX)), nl,
	write(suffix(SUFFIX)), nl,
	append_length(SUFFIX_INDEX2, SUFFIX, EPSILON),
	L2 is 1 + EPSILON,
	equilibrium_search_sols(1, L2, PREFIX, SUFFIX_INDEX2, COEF1, COEF2, TOLERANCE, LIM1, LIM2),
	(LIM1 = [_|_] ->
	    list_to_fdset(LIM1, SET1), Lim1 in_set SET1, Lim1 = INDEX1,
	    list_to_fdset(LIM2, SET2), Lim2 in_set SET2, Lim2 = INDEX2,
	    EPSILON #= INDEX2-INDEX1
	;
	    fail
	).

equilibrium_among_diff_0(VARS, PREFIX, SUFFIX) :-
	among_diff_0_counters_check(VARS, 0, PREFIX),
	reverse(VARS, RVARS),
	among_diff_0_counters_check(RVARS, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_and(VARS, PREFIX, SUFFIX) :-
	and_counters_check(VARS, init, PREFIX),
	reverse(VARS, RVARS),
	and_counters_check(RVARS, init, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_change(VARS, PREFIX, SUFFIX) :-
	change_neq_counters_check(VARS, 0, PREFIX),
	reverse(VARS, RVARS),
	change_neq_counters_check(RVARS, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_deepest_valley(VARS, PREFIX, SUFFIX) :-
	MAXINT = 1000000,
	deepest_valley_counters_check(VARS, init, MAXINT, PREFIX),
	reverse(VARS, RVARS),
	deepest_valley_counters_check(RVARS, init, MAXINT, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_highest_peak(VARS, PREFIX, SUFFIX) :-
	MININT = -1000000,
	highest_peak_counters_check(VARS, init, MININT, PREFIX),
	reverse(VARS, RVARS),
	highest_peak_counters_check(RVARS, init, MININT, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_increasing_nvalue(VARS, PREFIX, SUFFIX) :-
	VARS = [V|R], increasing_nvalue_counters_check(R, V, 1, RPREFIX), PREFIX = [1|RPREFIX],
	reverse(VARS, RVARS),
	RVARS = [U|S], decreasing_nvalue_counters_check(S, U, 1, COUNTS),
	reverse([1|COUNTS], SUFFIX).

equilibrium_inflexion(VARS, PREFIX, SUFFIX) :-
	inflexion_counters_check(init, VARS, 0, PREFIX),
	reverse(VARS, RVARS),
	inflexion_counters_check(init, RVARS, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_longest_change(VARS, PREFIX, SUFFIX) :-
	longest_change_neq_counters_check(VARS, 0, 1, PREFIX),
	reverse(VARS, RVARS),
	longest_change_neq_counters_check(RVARS, 0, 1, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_longest_decreasing_sequence(VARS, PREFIX, SUFFIX) :-
	longest_decreasing_sequence_counters_check(VARS, s, 0, 0, 0, PREFIX),
	reverse(VARS, RVARS),
	longest_increasing_sequence_counters_check(RVARS, s, 0, 0, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_longest_increasing_sequence(VARS, PREFIX, SUFFIX) :-
	longest_increasing_sequence_counters_check(VARS, s, 0, 0, 0, PREFIX),
	reverse(VARS, RVARS),
	longest_decreasing_sequence_counters_check(RVARS, s, 0, 0, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_max_decreasing_slope(VARS, PREFIX, SUFFIX) :-
	max_decreasing_slope_counters_check(VARS, PREFIX),
	reverse(VARS, RVARS),
	max_increasing_slope_counters_check(RVARS, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_max_increasing_slope(VARS, PREFIX, SUFFIX) :-
	max_increasing_slope_counters_check(VARS, PREFIX),
	reverse(VARS, RVARS),
	max_decreasing_slope_counters_check(RVARS, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_min_decreasing_slope(VARS, PREFIX, SUFFIX) :-
	min_decreasing_slope_counters_check(VARS, PREFIX),
	reverse(VARS, RVARS),
	min_increasing_slope_counters_check(RVARS, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_min_increasing_slope(VARS, PREFIX, SUFFIX) :-
	min_increasing_slope_counters_check(VARS, PREFIX),
	reverse(VARS, RVARS),
	min_decreasing_slope_counters_check(RVARS, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_min_width_peak(VARS, N, PREFIX, SUFFIX) :-
	min_width_peak_counters_check(VARS, N, PREFIX),
	reverse(VARS, RVARS),
	min_width_peak_counters_check(RVARS, N, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_min_width_valley(VARS, N, PREFIX, SUFFIX) :-
	min_width_valley_counters_check(VARS, N, PREFIX),
	reverse(VARS, RVARS),
	min_width_valley_counters_check(RVARS, N, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_peak(VARS, PREFIX, SUFFIX) :-
	peak_counters_check(VARS, init, 0, PREFIX),
	reverse(VARS, RVARS),
	peak_counters_check(RVARS, init, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_sum_ctr(VARS, 1, PREFIX, SUFFIX) :-
	sum_counters_check(VARS, 0, PREFIX),
	reverse(VARS, RVARS),
	sum_counters_check(RVARS, 0, COUNTS),
	reverse(COUNTS, SUFFIX).
equilibrium_sum_ctr(VARS, 0, PREFIX, SUFFIX) :-
	sum_counters_ref(VARS, 0, PREFIX),
	reverse(VARS, RVARS),
	sum_counters_ref(RVARS, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_valley(VARS, PREFIX, SUFFIX) :-
	valley_counters_check(VARS, init, 0, PREFIX),
	reverse(VARS, RVARS),
	valley_counters_check(RVARS, init, 0, COUNTS),
	reverse(COUNTS, SUFFIX).

equilibrium_search_sols(_, _, _, [], _, _, _, [], []) :- !.
equilibrium_search_sols(INDEX1, INDEX2, [C1|RC1], [C2|RC2], COEF1, COEF2, TOLERANCE, [INDEX1|RL1], [INDEX2|RL2]) :-
	integer(C1),
	integer(C2),
	CC1 is COEF1*C1,
	CC2 is COEF2*C2,
	DIF is abs(CC1-CC2),
	DIF =< TOLERANCE,
	!,
	NEXT_INDEX1 is INDEX1+1,
	NEXT_INDEX2 is INDEX2+1,
	equilibrium_search_sols(NEXT_INDEX1, NEXT_INDEX2, RC1, RC2, COEF1, COEF2, TOLERANCE, RL1, RL2).
equilibrium_search_sols(INDEX1, INDEX2, [_|RC1], [_|RC2], COEF1, COEF2, TOLERANCE, RL1, RL2) :-
	NEXT_INDEX1 is INDEX1+1,
	NEXT_INDEX2 is INDEX2+1,
	equilibrium_search_sols(NEXT_INDEX1, NEXT_INDEX2, RC1, RC2, COEF1, COEF2, TOLERANCE, RL1, RL2).

% returns the lists of prefix and suffix counters associated with constraint CTR and EPSILON
equilibrium_expose_prefix_suffix_counters(VARIABLES, EPSILON, CTR, PREFIX, SUFFIX_INDEX2) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	length(VARS, N),
	N         >= 1,
        EPSILON   >= 0,
        EPSILON   =< 2,
	memberchk(CTR, [among_diff_0               ,
			and                        ,
			change                     ,
			deepest_valley             ,
			highest_peak               ,
			increasing_nvalue          ,
			inflexion                  ,
			longest_change             ,
			longest_decreasing_sequence,
			longest_increasing_sequence,
			max_decreasing_slope       ,
			max_increasing_slope       ,
			min_decreasing_slope       ,
			min_increasing_slope       ,
			min_width_peak             ,
			min_width_valley           ,
			peak                       ,
			sum_ctr                    ,
			valley                     ]),
	(CTR = among_diff_0                -> equilibrium_among_diff_0(VARS, PREFIX, SUFFIX)               ;
	 CTR = and                         -> equilibrium_and(VARS, PREFIX, SUFFIX)                        ;
	 CTR = change                      -> equilibrium_change(VARS, PREFIX, SUFFIX)                     ;
	 CTR = deepest_valley              -> equilibrium_deepest_valley(VARS, PREFIX, SUFFIX)             ;
	 CTR = highest_peak                -> equilibrium_highest_peak(VARS, PREFIX, SUFFIX)               ;
	 CTR = increasing_nvalue           -> equilibrium_increasing_nvalue(VARS, PREFIX, SUFFIX)          ;
	 CTR = inflexion                   -> equilibrium_inflexion(VARS, PREFIX, SUFFIX)                  ;
	 CTR = longest_change              -> equilibrium_longest_change(VARS, PREFIX, SUFFIX)             ;
	 CTR = longest_decreasing_sequence -> equilibrium_longest_decreasing_sequence(VARS, PREFIX, SUFFIX);
	 CTR = longest_increasing_sequence -> equilibrium_longest_increasing_sequence(VARS, PREFIX, SUFFIX);
	 CTR = max_decreasing_slope        -> equilibrium_max_decreasing_slope(VARS, PREFIX, SUFFIX)       ;
	 CTR = max_increasing_slope        -> equilibrium_max_increasing_slope(VARS, PREFIX, SUFFIX)       ;
	 CTR = min_decreasing_slope        -> equilibrium_min_decreasing_slope(VARS, PREFIX, SUFFIX)       ;
	 CTR = min_increasing_slope        -> equilibrium_min_increasing_slope(VARS, PREFIX, SUFFIX)       ;
         CTR = min_width_peak              -> equilibrium_min_width_peak(VARS, N, PREFIX, SUFFIX)          ;
         CTR = min_width_valley            -> equilibrium_min_width_valley(VARS, N, PREFIX, SUFFIX)        ;
	 CTR = peak                        -> equilibrium_peak(VARS, PREFIX, SUFFIX)                       ;
	 CTR = sum_ctr                     -> equilibrium_sum_ctr(VARS, 1, PREFIX, SUFFIX)                 ;
	 CTR = valley                      -> equilibrium_valley(VARS, PREFIX, SUFFIX)                     ),
	append_length(SUFFIX_INDEX2, SUFFIX, EPSILON).

equilibrium_r(VARIABLES, INDEX1, INDEX2, EPSILON, COEF1, COEF2, TOLERANCE, CTR) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	length(VARS, N),
	N         >= 1,
	check_type(dvar(1,N), INDEX1),
	check_type(dvar(1,N), INDEX2),
        EPSILON   >= 0          ,
        EPSILON   =< 2          ,
        TOLERANCE >= 0          ,
        INDEX1    #=< INDEX2    ,
	EPSILON #= INDEX2-INDEX1,
	memberchk(CTR, [among_diff_0               ,
			and                        ,
			change                     ,
			deepest_valley             ,
			highest_peak               ,
			increasing_nvalue          ,
			inflexion                  ,
			longest_change             ,
			longest_decreasing_sequence,
			longest_increasing_sequence,
			max_decreasing_slope       ,
			max_increasing_slope       ,
			min_decreasing_slope       ,
			min_increasing_slope       ,
			min_width_peak             ,
			min_width_valley           ,
			peak                       ,
			sum_ctr                    ,
			valley                     ]),
	(CTR = among_diff_0 ->
	    true
	;
         CTR = and ->
	    true
	;
         CTR = change ->
	    true
	;
         CTR = deepest_valley ->
	    true
	;
         CTR = highest_peak ->
	    true
	;
         CTR = increasing_nvalue ->
	    true
	;
         CTR = inflexion ->
	    true
	;
         CTR = longest_change ->
	    true
	;
         CTR = longest_decreasing_sequence ->
	    true
	;
         CTR = longest_increasing_sequence ->
	    true
	;
         CTR = max_decreasing_slope ->
	    true
	;
         CTR = max_increasing_slope ->
	    true
	;
         CTR = min_decreasing_slope ->
	    true
	;
         CTR = min_increasing_slope ->
	    true
	;
         CTR = min_width_peak ->
	    true
	;
         CTR = min_width_valley ->
	    true
	;
	CTR = peak ->
	    true
	;
	 CTR = sum_ctr ->
	    equilibrium_sum_ctr(VARS, 0, SUM_PREFIX, SUM_SUFFIX),
	    append_length(SUM_SUFFIX_INDEX2, SUM_SUFFIX, EPSILON),
	    equilibrium_sum_ctr1(1, INDEX1, SUM_PREFIX, SUM_SUFFIX_INDEX2, COEF1, COEF2, TOLERANCE)
	;
         CTR = valley ->
	    true
	).

equilibrium_sum_ctr1(_, _, _, [], _, _, _) :- !.
equilibrium_sum_ctr1(I, INDEX1, [C1|RC1], [C2|RC2], COEF1, COEF2, TOLERANCE) :-
	INDEX1 #= I #=> abs(COEF1*C1-COEF2*C2) #=< TOLERANCE,
	I1 is I+1,
	equilibrium_sum_ctr1(I1, INDEX1, RC1, RC2, COEF1, COEF2, TOLERANCE).

sum_c(0, C, _, C) :- !.
sum_c(INDEX, C, [VAR|R], RES) :-
	INDEX > 0,
	NextC is C+VAR,
	NextINDEX is INDEX-1,
	sum_c(NextINDEX, NextC, R, RES).

sum_counters_check([], _, []).
sum_counters_check([VAR|R], SUM_CUR, [SUM_NEXT|S]) :-
	SUM_NEXT is SUM_CUR + VAR,
	sum_counters_check(R, SUM_NEXT, S).

sum_counters_ref([], _, []).
sum_counters_ref([VAR|R], SUM_CUR, [SUM_NEXT|S]) :-
	SUM_NEXT #= SUM_CUR + VAR,
	sum_counters_ref(R, SUM_NEXT, S).
