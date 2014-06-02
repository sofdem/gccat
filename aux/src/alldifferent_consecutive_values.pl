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

ctr_date(alldifferent_consecutive_values,['20080618']).

ctr_origin(alldifferent_consecutive_values, 'Derived from %c.', [alldifferent]).

ctr_arguments(alldifferent_consecutive_values,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(alldifferent_consecutive_values,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(alldifferent_consecutive_values,
                 [required('VARIABLES',var),
                  alldifferent('VARIABLES')]).

ctr_typical(alldifferent_consecutive_values,
            [size('VARIABLES') > 2]).

ctr_graph(alldifferent_consecutive_values,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          ['TRUE'],
          ['RANGE'('VARIABLES',var)=size('VARIABLES')-1],
          []).

ctr_example(alldifferent_consecutive_values,
            alldifferent_consecutive_values([[var-5],[var-4],[var-3],[var-6]])).

ctr_cond_imply(alldifferent_consecutive_values,                              among_diff_0,
	       [minval('VARIABLES'^var) =< 0, maxval('VARIABLES'^var) >= 0], ['NVAR' = size('VARIABLES') - 1],
	       [none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, among_diff_0,
	       [minval('VARIABLES'^var) > 0],   ['NVAR' = size('VARIABLES')],
               [none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, among_diff_0,
	       [maxval('VARIABLES'^var) < 0], ['NVAR' = size('VARIABLES')],
	       [none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, balance,
	       [],                              ['BALANCE' = 0],
	       [none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, length_first_sequence,
	       [size('VARIABLES') > 0],         ['LEN' = 1],
	       [none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, length_last_sequence,
	       [size('VARIABLES') > 0],         ['LEN' = 1],
	       [none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, max_n,
	       [],                              ['MAX' = maxval('VARIABLES'^var) - 'RANK'],
	       [none, none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, min_n,
	       [],                              ['MIN' = minval('VARIABLES'^var) + 'RANK'],
	       [none, none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, min_nvalue,
	       [size('VARIABLES') > 0],         ['MIN' = 1],
	       [none,'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, ninterval,
	       [minval('VARIABLES'^var) = 0], ['NVAL' = (size('VARIABLES')+'SIZE_INTERVAL'-1) / 'SIZE_INTERVAL'],
	       [none, 'VARIABLES', none]).
ctr_cond_imply(alldifferent_consecutive_values, range_ctr,
	       [],                              [in_list('CTR',[=<]), 'R' = size('VARIABLES')],
	       [none, none, 'VARIABLES']).
ctr_cond_imply(alldifferent_consecutive_values, soft_alldifferent_ctr,
	       [],                              [],
	       [none, 'VARIABLES']).
ctr_see_also(alldifferent_consecutive_values,
 [link('implied by', permutation,        '', []),
  link('implies',    alldifferent,       '', []),
  link('implies',    consecutive_values, '', [])]).

ctr_key_words(alldifferent_consecutive_values,['value constraint'        ,
                                               'permutation'             ,
                                               'all different'           ,
                                               'disequality'             ,
                                               'sort based reformulation']).

ctr_eval(alldifferent_consecutive_values, [checker(alldifferent_consecutive_values_c),
					   reformulation(alldifferent_consecutive_values_r)]).

ctr_sol(alldifferent_consecutive_values,2,0,2,4,-).
ctr_sol(alldifferent_consecutive_values,3,0,3,12,-).
ctr_sol(alldifferent_consecutive_values,4,0,4,48,-).
ctr_sol(alldifferent_consecutive_values,5,0,5,240,-).
ctr_sol(alldifferent_consecutive_values,6,0,6,1440,-).
ctr_sol(alldifferent_consecutive_values,7,0,7,10080,-).
ctr_sol(alldifferent_consecutive_values,8,0,8,80640,-).
ctr_sol(alldifferent_consecutive_values,9,0,9,725760,-).
ctr_sol(alldifferent_consecutive_values,10,0,10,7257600,-).

alldifferent_consecutive_values_c([V,V|_]) :-
	!,
	fail.
alldifferent_consecutive_values_c([]) :- !.
alldifferent_consecutive_values_c(VARIABLES) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	sort(VARS, SVARS),
	length(VARS, N),
	length(SVARS, N),
	min_member(MIN, VARS),
	max_member(MAX, VARS),
        N is MAX-MIN+1.

alldifferent_consecutive_values_r([]) :- !.
alldifferent_consecutive_values_r(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	all_different(VARS),
	minimum(MIN, VARS),
	maximum(MAX, VARS),
	length(VARIABLES, N),
	N #= MAX-MIN+1.
