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

ctr_date(first_value_diff_0,['20120418']).

ctr_origin(first_value_diff_0, 'Paparazzi puzzle', []).

ctr_arguments(first_value_diff_0,
              ['VAR'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_synonyms(first_value_diff_0,[first_value_diff_from_0     ,
				 first_value_different_from_0]).

ctr_restrictions(first_value_diff_0,
                 ['VAR' =\= 0              ,
                  size('VARIABLES') >= 1   ,
                  required('VARIABLES',var)]).

ctr_typical(first_value_diff_0,
            [size('VARIABLES') > 1                                                           ,
             minval('VARIABLES'^var) < 0 #\/ maxval('VARIABLES'^var) > 1                     ,
	     size('VARIABLES') - among_diff_0('VARIABLES'^var) >= 1                          ,
	     size('VARIABLES') =< 4 #\/ size('VARIABLES') - among_diff_0('VARIABLES'^var) > 1]).

ctr_functional_dependency(first_value_diff_0, 1, [2]).

ctr_example(first_value_diff_0,
            [first_value_diff_0(8,[[var-0],[var-0],[var-8],[var-0],[var-5]]),
	     first_value_diff_0(4,[[var-4],[var-0],[var-8],[var-0],[var-5]])]).

ctr_see_also(first_value_diff_0,
 [link('implies', between_min_max, '', [])]).

ctr_key_words(first_value_diff_0,['joker value'            ,
				  'automaton'              ,
                                  'automaton with counters',
                                  'functional dependency'  ]).

ctr_eval(first_value_diff_0, [checker(first_value_diff_0_c),
			      automaton(first_value_diff_0_a)]).

ctr_sol(first_value_diff_0,2,0,2,8,[1-4,2-4]).
ctr_sol(first_value_diff_0,3,0,3,63,[1-21,2-21,3-21]).
ctr_sol(first_value_diff_0,4,0,4,624,[1-156,2-156,3-156,4-156]).
ctr_sol(first_value_diff_0,5,0,5,7775,[1-1555,2-1555,3-1555,4-1555,5-1555]).
ctr_sol(first_value_diff_0,6,0,6,117648,[1-19608,2-19608,3-19608,4-19608,5-19608,6-19608]).
ctr_sol(first_value_diff_0,7,0,7,2097151,[1-299593,2-299593,3-299593,4-299593,5-299593,6-299593,7-299593]).
ctr_sol(first_value_diff_0,8,0,8,43046720,[1-5380840,2-5380840,3-5380840,4-5380840,5-5380840,6-5380840,7-5380840,8-5380840]).

first_value_diff_0_c(VAR, VARIABLES) :-
    check_type(dvar, VAR),
    VAR #\= 0,
    collection(VARIABLES, [int]),
    first_value_diff_0_c1(VARIABLES, VAR).

first_value_diff_0_c1([[var-0]|R], VAR) :- !,
    first_value_diff_0_c1(R, VAR).
first_value_diff_0_c1([[var-VAR]|_], VAR).

% 0: VARi = 0
% 1: VARi =\= 0
first_value_diff_0_a(FLAG, VAR, VARIABLES) :-
    check_type(dvar, VAR),
    VAR #\= 0,
    collection(VARIABLES, [dvar]),
    VARIABLES = [_|_],
    first_value_diff_0_signature(VARIABLES, SIGNATURE, VARS),
    automaton(VARS, VARi,
              SIGNATURE, 
              [source(s),sink(t)],
              [arc(s,0,s       ),
               arc(s,1,t,[VARi]),
               arc(t,0,t       ),
	       arc(t,1,t       )],
              [_C],[0],[COUNT]),
    COUNT #= VAR #<=> FLAG.

first_value_diff_0_signature([], [], []).
first_value_diff_0_signature([[var-VAR]|VARs], [S|Ss], [VAR|Ts]) :-
    VAR #\= 0 #<=> S,
    first_value_diff_0_signature(VARs, Ss, Ts).

first_value_diff_0_d(Density, _VAR, VARIABLES) :-
    get_attr1(VARIABLES, VARS),
    sort(VARS, SVARS),
    length(VARS, N),
    length(SVARS, S),
    Density is S / N.
