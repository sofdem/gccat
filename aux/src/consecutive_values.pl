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

ctr_date(consecutive_values,['20100106']).

ctr_origin(consecutive_values, 'Derived from %c.', [alldifferent_consecutive_values]).

ctr_arguments(consecutive_values,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(consecutive_values,
                 [items('VARIABLES',all),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(consecutive_values,
                 [required('VARIABLES',var)]).

ctr_typical(consecutive_values,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_predefined(consecutive_values).

ctr_example(consecutive_values,
            consecutive_values([[var-5],[var-4],[var-3],[var-5]])).

ctr_cond_imply(consecutive_values, some_equal, [size('VARIABLES') > range('VARIABLES'^var)], [], id).

ctr_see_also(consecutive_values,
 [link('implied by',            all_equal,                       '', []),
  link('implied by',            global_contiguity,               '', []),
  link('implied by',            alldifferent_consecutive_values, '', []),
  link('used in reformulation', nvalue,                          '', [])]).

ctr_key_words(consecutive_values,['value constraint'        ,
                                  'predefined constraint'   ,
                                  'sort based reformulation']).

ctr_eval(consecutive_values, [checker(consecutive_values_c),
			      reformulation(consecutive_values_r)]).

ctr_sol(consecutive_values,2,0,2,7,-).
ctr_sol(consecutive_values,3,0,3,34,-).
ctr_sol(consecutive_values,4,0,4,217,-).
ctr_sol(consecutive_values,5,0,5,1716,-).
ctr_sol(consecutive_values,6,0,6,16159,-).
ctr_sol(consecutive_values,7,0,7,176366,-).
ctr_sol(consecutive_values,8,0,8,2187637,-).

consecutive_values_c([]) :-
	!.
consecutive_values_c(VARIABLES) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	min_member(MIN, VARS),
	max_member(MAX, VARS),
	RANGE is MAX-MIN+1,
	length(VARS, N),
	N >= RANGE,
	sort(VARS, S),
	length(S, RANGE).

consecutive_values_r([]) :-
    !.
consecutive_values_r(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	minimum(MIN, VARS),
	maximum(MAX, VARS),
	length(VARIABLES, N),
	NVAL in 1..N,
	nvalue(NVAL, VARS),
	NVAL #= MAX-MIN+1.
