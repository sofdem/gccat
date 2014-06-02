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

ctr_date(permutation,['20111210']).

ctr_origin(permutation, 'Derived from %c.', [alldifferent_consecutive_values]).

ctr_arguments(permutation,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(permutation,
                 [items('VARIABLES',all),
		  vals(['VARIABLES'^var],int,=\=,all,in)]).

ctr_restrictions(permutation,
                 [required('VARIABLES',var)                  ,
		  minval('VARIABLES'^var) = 1                ,
		  maxval('VARIABLES'^var) = size('VARIABLES')]).

ctr_typical(permutation,
            [size('VARIABLES') > 2]).

ctr_graph(permutation,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['MAX_NSCC' =< 1],
          ['ONE_SUCC']).

ctr_example(permutation,
            permutation([[var-3],[var-2],[var-1],[var-4]])).

ctr_draw_example(permutation,
                 ['VARIABLES'],
                 [[[var-3],[var-2],[var-1],[var-4]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3,4-4],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 []).

ctr_cond_imply(permutation, balance,                    [], ['BALANCE' = 0],                                                             [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, change,                     [], ['NCHANGE' = size('VARIABLES')-1, in_list('CTR',[=\=])],                     [none, 'VARIABLES', none]).
ctr_cond_imply(permutation, circular_change,            [], ['NCHANGE' = size('VARIABLES'), in_list('CTR',[=\=])],                       [none, 'VARIABLES', none]).
ctr_cond_imply(permutation, length_last_sequence,       [], ['LEN'     = 1],                                                             [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, length_first_sequence,      [], ['LEN'     = 1],                                                             [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, longest_change,             [], ['SIZE'    = size('VARIABLES'), in_list('CTR',[=\=])],                       [none, 'VARIABLES', none]).
ctr_cond_imply(permutation, max_n,                      [], ['MAX'     = size('VARIABLES')-'RANK'],                                      [none, none, 'VARIABLES']).
ctr_cond_imply(permutation, min_n,                      [], ['MIN'     = 'RANK'+1],                                                      [none, none, 'VARIABLES']).
ctr_cond_imply(permutation, min_nvalue,                 [], ['MIN'     = 1],                                                             [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, min_size_full_zero_stretch, [], ['MINSIZE' = size('VARIABLES')],                                             [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, ninterval,                  [], ['NVAL'=(size('VARIABLES') + 'SIZE_INTERVAL') / 'SIZE_INTERVAL'],            [none, 'VARIABLES', none]).
ctr_cond_imply(permutation, range_ctr,                  [], [in_list('CTR',[=<]), 'R' = size('VARIABLES')],                              ['VARIABLES', none, none]).
ctr_cond_imply(permutation, soft_alldifferent_ctr,      [], [],                                                                          [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, soft_all_equal_max_var,     [], ['N'      =< size('VARIABLES')-1],                                           [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, soft_all_equal_min_var,     [], ['N'      >= size('VARIABLES')-1],                                           [none, 'VARIABLES']      ).
ctr_cond_imply(permutation, sum_ctr,                    [], [in_list('CTR',[=]), 'VAR' = (size('VARIABLES')*(size('VARIABLES')+1)) / 2], ['VARIABLES', none, none]).
ctr_cond_imply(permutation, deepest_valley,             [size('VARIABLES') > 2,
							 first('VARIABLES'^var) >  minval('VARIABLES'^var),
							 last('VARIABLES'^var)  >  minval('VARIABLES'^var)],
                                                        ['DEPTH' = minval('VARIABLES'^var)], [none, 'VARIABLES']).
ctr_cond_imply(permutation, deepest_valley,             [size('VARIABLES') > 2, first('VARIABLES'^var) = 1], ['DEPTH' = 2],              [none, 'VARIABLES']).
ctr_cond_imply(permutation, deepest_valley,             [size('VARIABLES') > 2, last('VARIABLES'^var)  = 1], ['DEPTH' = 2],              [none, 'VARIABLES']).
ctr_cond_imply(permutation, highest_peak,               [size('VARIABLES') > 2,
							 first('VARIABLES'^var) <  maxval('VARIABLES'^var),
							 last('VARIABLES'^var)  <  maxval('VARIABLES'^var)],
                                                        ['HEIGHT' = maxval('VARIABLES'^var)], [none, 'VARIABLES']).
ctr_cond_imply(permutation, highest_peak,               [size('VARIABLES') > 2, first('VARIABLES'^var) = size('VARIABLES')],
	                                                ['HEIGHT' = size('VARIABLES')-1], [none, 'VARIABLES']).
ctr_cond_imply(permutation, highest_peak,               [size('VARIABLES') > 2, last('VARIABLES'^var)  = size('VARIABLES')],
	                                                ['HEIGHT' = size('VARIABLES')-1], [none, 'VARIABLES']).

/*
ctr_cond_imply(permutation, sum_squares_ctr,            [], [in_list('CTR',[=]),
							 'VAR' = (size('VARIABLES')*(size('VARIABLES')+1)*(2*size('VARIABLES')+1)) / 6], ['VARIABLES', none, none]).
ctr_cond_imply(permutation, sum_cubes_ctr,              [], [in_list('CTR',[=]),
		                         'VAR' = (size('VARIABLES')*size('VARIABLES')*(size('VARIABLES')+1)*(size('VARIABLES')+1)) / 4], ['VARIABLES', none, none]).
ctr_cond_imply(permutation, sum_powers4_ctr, [], [in_list('CTR',[=]),
'VAR' = (size('VARIABLES')*(size('VARIABLES')+1)*(2*size('VARIABLES')+1)*(3*size('VARIABLES')*size('VARIABLES')+3*size('VARIABLES')-1)) / 30], ['VARIABLES', none, none]).
ctr_cond_imply(permutation, sum_powers5_ctr, [], [in_list('CTR',[=]),
'VAR' = (size('VARIABLES')*size('VARIABLES')*(size('VARIABLES')+1)*(size('VARIABLES')+1)*(2*size('VARIABLES')*size('VARIABLES')+2*size('VARIABLES')-1)) / 12], ['VARIABLES', none, none]).
ctr_cond_imply(permutation, sum_powers6_ctr, [], [in_list('CTR',[=]),
'VAR' = (size('VARIABLES')*(size('VARIABLES')+1)*(2*size('VARIABLES')+1)*(3*size('VARIABLES')*size('VARIABLES')*size('VARIABLES')*size('VARIABLES')+6*size('VARIABLES')*size('VARIABLES')*size('VARIABLES')-3*size('VARIABLES')+1)) / 42], ['VARIABLES', none, none]).
*/

ctr_see_also(permutation,
 [link('implies',    alldifferent_consecutive_values, '', []),
  link('implied by', proper_circuit,                  '', [])]).

ctr_key_words(permutation,['value constraint'        ,
                           'permutation'             ,
                           'all different'           ,
                           'disequality'             ,
                           'sort based reformulation',
                           'one\\_succ'              ]).

ctr_eval(permutation, [checker(permutation_c),
		       reformulation(permutation_r)]).

ctr_sol(permutation,2,0,2,2,-).
ctr_sol(permutation,3,0,3,6,-).
ctr_sol(permutation,4,0,4,24,-).
ctr_sol(permutation,5,0,5,120,-).
ctr_sol(permutation,6,0,6,720,-).
ctr_sol(permutation,7,0,7,5040,-).
ctr_sol(permutation,8,0,8,40320,-).
ctr_sol(permutation,9,0,9,362880,-).
ctr_sol(permutation,10,0,10,3628800,-).

permutation_c([V,V|_]) :-
	!,
	fail.
permutation_c([]) :- !.
permutation_c(VARIABLES) :-
    length(VARIABLES, N),
    collection(VARIABLES, [int(1,N)]),
    get_attr1(VARIABLES, VARS),
    sort(VARS, SVARS),
    length(SVARS, N).

permutation_r([]) :-
    !.
permutation_r(VARIABLES) :-
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    all_different(VARS),
    minimum(1, VARS),
    length(VARIABLES, N),
    maximum(N, VARS).
