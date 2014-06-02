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

ctr_date(sum_of_weights_of_distinct_values,['20030820','20040726','20060817']).

ctr_origin(sum_of_weights_of_distinct_values, '\\cite{BeldiceanuCarlssonThiel02}', []).

ctr_arguments(sum_of_weights_of_distinct_values,
              ['VARIABLES'-collection(var-dvar)        ,
               'VALUES'-collection(val-int, weight-int),
               'COST'-dvar                             ]).

ctr_exchangeable(sum_of_weights_of_distinct_values,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  items('VALUES',all),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_synonyms(sum_of_weights_of_distinct_values,[swdv]).

ctr_restrictions(sum_of_weights_of_distinct_values,
                 [required('VARIABLES',var)            ,
                  size('VALUES') > 0                   ,
                  required('VALUES',[val,weight])      ,
                  'VALUES'^weight >= 0                 ,
                  distinct('VALUES',val)               ,
                  in_attr('VARIABLES',var,'VALUES',val),
                  'COST' >= 0                          ]).

ctr_typical(sum_of_weights_of_distinct_values,
            [size('VARIABLES')       > 1,
             range('VARIABLES'^var)  > 1,
             size('VALUES')          > 1,
             range('VALUES'^weight)  > 1,
	     maxval('VALUES'^weight) > 0]).

ctr_pure_functional_dependency(sum_of_weights_of_distinct_values, []).
ctr_functional_dependency(sum_of_weights_of_distinct_values, 3, [1,2]).

ctr_graph(sum_of_weights_of_distinct_values,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var = values^val],
          ['NSOURCE' = size('VARIABLES')  ,
           'SUM'('VALUES',weight) = 'COST'],
          []).

ctr_example(sum_of_weights_of_distinct_values,
            sum_of_weights_of_distinct_values([[var-1],
                                               [var-6],
                                               [var-1]],
                                              [[val-1, weight-5],
                                               [val-2, weight-3],
                                               [val-6, weight-7]],
                                              12)).

ctr_draw_example(sum_of_weights_of_distinct_values,
                 ['VARIABLES','VALUES'],
                 [[[var-1],[var-6],[var-1]],
                  [[val-1, weight-5],[val-2, weight-3],[val-6, weight-7]]],
                 ['PRODUCT'],
                 [1-1,2-3,3-1],
                 ['NSOURCE'([1,2,3]),'SUM'([4,6])],
                 '','NSOURCE=3\\nSUM(VALUES,weight)=5+7=12',
                 [1.8,1.8,1.8,1.8]).

ctr_see_also(sum_of_weights_of_distinct_values,
 [link('attached to cost variant', nvalue,                        'all values have a weight of %e', [1]),
  link('common keyword',           minimum_weight_alldifferent,   '%k',                             ['weighted assignment']),
  link('common keyword',           global_cardinality_with_costs, '%k',                             ['weighted assignment']),
  link('common keyword',           weighted_partial_alldiff,      '%k',                             ['weighted assignment'])]).

ctr_key_words(sum_of_weights_of_distinct_values,['cost filtering constraint'  ,
                                                 'assignment'                 ,
                                                 'relaxation'                 ,
                                                 'domination'                 ,
                                                 'weighted assignment'        ,
                                                 'facilities location problem',
                                                 'functional dependency'      ,
    		                                 'pure functional dependency' ]).

ctr_persons(sum_of_weights_of_distinct_values,['Beldiceanu N.',
                                               'Carlsson M.'  ,
                                               'Thiel S.'     ]).

ctr_eval(sum_of_weights_of_distinct_values,
         [checker(sum_of_weights_of_distinct_values_c),
	  reformulation(sum_of_weights_of_distinct_values_r)]).

sum_of_weights_of_distinct_values_r(VARIABLES, VALUES, COST) :-
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int, int_gteq(0)]),
    check_type(dvar_gteq(0), COST),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),
    get_attr2(VALUES, WEIGHTS),
    all_different(VALS),
    (VARS = [] ->
	COST #= 0
    ;
	sum_of_weights_of_distinct_values1(VARS, VALS),
	sum_of_weights_of_distinct_values3(VALS, WEIGHTS, VARS, TERM),
	call(COST #= TERM)
    ).

sum_of_weights_of_distinct_values1([], _).
sum_of_weights_of_distinct_values1([VAR|RVAR], VALS) :-
    sum_of_weights_of_distinct_values2(VALS, VAR, OR_TERM),
    call(OR_TERM),
    sum_of_weights_of_distinct_values1(RVAR, VALS).

sum_of_weights_of_distinct_values2([], _, 0).
sum_of_weights_of_distinct_values2([VAL|RVAL], VAR, VAR#=VAL #\/ TERM) :-
    sum_of_weights_of_distinct_values2(RVAL, VAR, TERM).

sum_of_weights_of_distinct_values3([], [], _, 0).
sum_of_weights_of_distinct_values3([VAL|RVAL], [WEIGHT|RWEIGHT], VARS, WEIGHT*B+TERM) :-
    sum_of_weights_of_distinct_values4(VARS, VAL, OR_TERM),
    call(B #<=> OR_TERM),
    sum_of_weights_of_distinct_values3(RVAL, RWEIGHT, VARS, TERM).

sum_of_weights_of_distinct_values4([], _, 0).
sum_of_weights_of_distinct_values4([VAR|RVAR], VAL, VAL#=VAR #\/ TERM) :-
    sum_of_weights_of_distinct_values4(RVAR, VAL, TERM).

sum_of_weights_of_distinct_values_c(VARIABLES, VALUES, COST) :-
    collection(VARIABLES, [int]),
    collection(VALUES, [int, int_gteq(0)]),
    integer(COST),
    COST >= 0,
    get_attr1(VARIABLES, VARS),
    samsort(VARS, SVARS),
    get_attr12(VALUES, VAL_WEIGHT),
    keysort(VAL_WEIGHT, SVAL_WEIGHT),
    sum_of_weights_of_distinct_values_inc(SVAL_WEIGHT),
    sum_of_weights_of_distinct_values_check(SVARS, SVAL_WEIGHT, COST).

sum_of_weights_of_distinct_values_inc([]) :- !.
sum_of_weights_of_distinct_values_inc([_]) :- !.
sum_of_weights_of_distinct_values_inc([Val1-_,Val2-Weight2|R]) :-
    Val1 < Val2,
    sum_of_weights_of_distinct_values_inc([Val2-Weight2|R]).

sum_of_weights_of_distinct_values_check([], _, 0) :- !.
sum_of_weights_of_distinct_values_check([Val|R], [Val-Weight|S], Cost) :-
    !,
    Cost1 is Cost-Weight,
    sum_of_weights_of_distinct_values_check(R, [Val-0|S], Cost1).
sum_of_weights_of_distinct_values_check([Var|R], [Val-_Weight|S], Cost) :-
    Var > Val,
    sum_of_weights_of_distinct_values_check([Var|R], S, Cost).
