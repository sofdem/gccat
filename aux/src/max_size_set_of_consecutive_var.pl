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

ctr_date(max_size_set_of_consecutive_var,['20030820','20040530','20060811']).

ctr_origin(max_size_set_of_consecutive_var, 'N.~Beldiceanu', []).

ctr_arguments(max_size_set_of_consecutive_var,
              ['MAX'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(max_size_set_of_consecutive_var,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(max_size_set_of_consecutive_var,
                 ['MAX' >= 1                ,
                  'MAX' =< size('VARIABLES'),
                  required('VARIABLES',var) ]).

ctr_typical(max_size_set_of_consecutive_var,
            ['MAX'                  < size('VARIABLES'),
             size('VARIABLES')      > 0                ,
             range('VARIABLES'^var) > 1                ]).

ctr_pure_functional_dependency(max_size_set_of_consecutive_var, []).
ctr_functional_dependency(max_size_set_of_consecutive_var, 1, [2]).

ctr_graph(max_size_set_of_consecutive_var,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [abs(variables1^var - variables2^var) =< 1],
          ['MAX_NSCC' = 'MAX'],
          []).

ctr_example(max_size_set_of_consecutive_var,
            [max_size_set_of_consecutive_var(6, [[var-3],[var-1],[var-3],[var-7],[var-4],[var-1],[var-2],[var-8],[var-7],[var-6]]),
	     max_size_set_of_consecutive_var(2, [[var-2],[var-6],[var-7],[var-3],[var-0],[var-9]])]).

ctr_draw_example(max_size_set_of_consecutive_var,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-3],[var-7],[var-4],
                   [var-1],[var-2],[var-8],[var-7],[var-6]]],
                 ['CLIQUE'],
                 [1-[1,3,5,7],
                  2-[2,6,7],
                  3-[1,3,5,7],
                  4-[4,8,9,10],
                  5-[1,3,5],
                  6-[2,6,7],
                  7-[1,2,3,6,7],
                  8-[4,8,10],
                  9-[4,8,9,10],
                  10-[4,9,10]],
                 ['MAX_NSCC'([1,2,3,5,6,7])],
                 '','MAX_NSCC=6',
                 [3.5,3,3,3]).

ctr_see_also(max_size_set_of_consecutive_var,
 [link('common keyword', nset_of_consecutive_values, '%k', ['consecutive values'])]).

ctr_key_words(max_size_set_of_consecutive_var,['value constraint'          ,
                                               'consecutive values'        ,
                                               'maximum'                   ,
                                               'functional dependency'     ,
		                               'pure functional dependency']).

ctr_persons(max_size_set_of_consecutive_var,['Beldiceanu N.']).

ctr_eval(max_size_set_of_consecutive_var, [checker(max_size_set_of_consecutive_var_c)]).

ctr_sol(max_size_set_of_consecutive_var,2,0,2,9,[1-2,2-7]).
ctr_sol(max_size_set_of_consecutive_var,3,0,3,64,[2-30,3-34]).
ctr_sol(max_size_set_of_consecutive_var,4,0,4,625,[2-168,3-240,4-217]).
ctr_sol(max_size_set_of_consecutive_var,5,0,5,7776,[2-720,3-3080,4-2260,5-1716]).
ctr_sol(max_size_set_of_consecutive_var,6,0,6,117649,[2-5220,3-35580,4-36030,5-24660,6-16159]).
ctr_sol(max_size_set_of_consecutive_var,7,0,7,2097152,[2-27720,3-426720,4-683550,5-477162,6-305634,7-176366]).
ctr_sol(max_size_set_of_consecutive_var,8,0,8,43046721,[2-249480,3-6059760,4-12672940,5-10592848,6-7044632,7-4239424,8-2187637]).

max_size_set_of_consecutive_var_c(MAX, VARIABLES) :-
	length(VARIABLES, N),
	check_type(dvar(1,N), MAX),
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	samsort(VARS, SVARS),
	SVARS = [V|R],
	max_size_set_of_consecutive_var_c(R, V, 1, 0, M),
	MAX #= M.

max_size_set_of_consecutive_var_c([V|R], Prev, Occ, MaxOcc, Res) :-
	Diff is V - Prev,
	Diff =< 1,
	!,
	Occ1 is Occ + 1,
	max_size_set_of_consecutive_var_c(R, V, Occ1, MaxOcc, Res).
max_size_set_of_consecutive_var_c([V|R], _, Occ, MaxOcc, Res) :-
	!,
	Max is max(Occ,MaxOcc),
	max_size_set_of_consecutive_var_c(R, V, 1, Max, Res).
max_size_set_of_consecutive_var_c([], _, Occ, MaxOcc, Res) :-
	Res is max(Occ,MaxOcc).
