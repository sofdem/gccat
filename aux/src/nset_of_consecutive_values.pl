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

ctr_date(nset_of_consecutive_values,['20030820','20040530','20060812']).

ctr_origin(nset_of_consecutive_values, 'N.~Beldiceanu', []).

ctr_arguments(nset_of_consecutive_values,
              ['N'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(nset_of_consecutive_values,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(nset_of_consecutive_values,
                 ['N' >= 1                 ,
                  'N' =< size('VARIABLES') ,
                  required('VARIABLES',var)]).

ctr_typical(nset_of_consecutive_values,
            ['N'                    > 1,
             size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_pure_functional_dependency(nset_of_consecutive_values, []).
ctr_functional_dependency(nset_of_consecutive_values, 1, [2]).

ctr_graph(nset_of_consecutive_values,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [abs(variables1^var - variables2^var) =< 1],
          ['NSCC' = 'N'],
          []).

ctr_example(nset_of_consecutive_values,
            [nset_of_consecutive_values(2,[[var-3],[var-1],[var-7],[var-1],[var-1],[var-2],[var-8]]),
	     nset_of_consecutive_values(7,[[var-3],[var-1],[var-5],[var-7],[var-9],[var-11],[var-13]]),
	     nset_of_consecutive_values(1,[[var-3],[var-3],[var-3],[var-3],[var-3],[var-3],[var-3]])]).

ctr_draw_example(nset_of_consecutive_values,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-7],[var-1],[var-1],[var-2],[var-8]]],
                 ['CLIQUE'],
                 [1-[1,6],
                  2-[2,4,5,6],
                  3-[3,7],
                  4-[2,4,5,6],
                  5-[2,4,5,6],
                  6-[1,2,4,5,6],
                  7-[3,7]],
                 ['NSCC'([[1,2,4,5,6],[3,7]])],
                 '','NSCC=2',
                 [2.4,3,2.5,3.03]).

ctr_see_also(nset_of_consecutive_values,
 [link('common keyword', max_size_set_of_consecutive_var, '%k', ['consecutive values']),
  link('common keyword', min_size_set_of_consecutive_var, '%k', ['consecutive values'])]).

ctr_key_words(nset_of_consecutive_values,['value constraint'            ,
                                          'consecutive values'          ,
                                          'strongly connected component',
                                          'functional dependency'       ,
		                          'pure functional dependency'  ]).

ctr_persons(nset_of_consecutive_values,['Beldiceanu N.']).

ctr_eval(nset_of_consecutive_values, [checker(nset_of_consecutive_values_c)]).

ctr_sol(nset_of_consecutive_values,2,0,2,9,[1-7,2-2]).
ctr_sol(nset_of_consecutive_values,3,0,3,64,[1-34,2-30]).
ctr_sol(nset_of_consecutive_values,4,0,4,625,[1-217,2-372,3-36]).
ctr_sol(nset_of_consecutive_values,5,0,5,7776,[1-1716,2-4740,3-1320]).
ctr_sol(nset_of_consecutive_values,6,0,6,117649,[1-16159,2-65010,3-34920,4-1560]).
ctr_sol(nset_of_consecutive_values,7,0,7,2097152,[1-176366,2-969066,3-842520,4-109200]).
ctr_sol(nset_of_consecutive_values,8,0,8,43046721,[1-2187637,2-15695624,3-19989900,4-5047560,5-126000]).

nset_of_consecutive_values_c(N, VARIABLES) :-
	length(VARIABLES, L),
	check_type(dvar(1,L), N),
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	samsort(VARS, SVARS),
	SVARS = [V|R],
	nset_of_consecutive_values_c(R, V, 1, NSet),
	N #= NSet.

nset_of_consecutive_values_c([V|R], Prev, NSet, Res) :-
	Diff is V - Prev,
	Diff =< 1,
	!,
	nset_of_consecutive_values_c(R, V, NSet, Res).
nset_of_consecutive_values_c([V|R], _, NSet, Res) :-
	!,
	NSet1 is NSet + 1,
	nset_of_consecutive_values_c(R, V, NSet1, Res).
nset_of_consecutive_values_c([], _, Res, Res).
