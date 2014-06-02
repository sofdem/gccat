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

ctr_date(weighted_partial_alldiff,['20040814','20060820','20090503']).

ctr_origin(weighted_partial_alldiff, '\\cite[page 71]{Thiel04}', []).

ctr_arguments(weighted_partial_alldiff,
              ['VARIABLES'-collection(var-dvar)        ,
               'UNDEFINED'-int                         ,
               'VALUES'-collection(val-int, weight-int),
               'COST'-dvar                             ]).

ctr_exchangeable(weighted_partial_alldiff,
                 [items('VARIABLES',all),
                  items('VALUES',all),
                  vals(['VARIABLES'^var,'VALUES'^val],int(=\=('UNDEFINED')),=\=,all,dontcare)]).

ctr_synonyms(weighted_partial_alldiff,[weighted_partial_alldifferent,
                                       weighted_partial_alldistinct ,
                                       wpa                          ]).

ctr_restrictions(weighted_partial_alldiff,
                 [required('VARIABLES',var)                ,
                  size('VALUES') > 0                       ,
                  required('VALUES',[val,weight])          ,
                  in_attr('VARIABLES',var,'VALUES',val)    ,
%                 item('VALUES',[val-'UNDEFINED',weight-0]),
                  distinct('VALUES',val)                   ]).

ctr_typical(weighted_partial_alldiff,
            [size('VARIABLES') > 0,
             atleast(1,'VARIABLES','UNDEFINED'),
             size('VARIABLES') =< size('VALUES')+2]).

ctr_functional_dependency(weighted_partial_alldiff, 4, [1,3]).

ctr_graph(weighted_partial_alldiff,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var =\= 'UNDEFINED',
           variables^var  =   values^val],
          ['MAX_ID'               =< 1    ,
           'SUM'('VALUES',weight) = 'COST'],
          []).

ctr_example(weighted_partial_alldiff,
            weighted_partial_alldiff([[var-4],[var-0],[var-1],[var-2],[var-0],[var-0]],
                                     0,
                                     [[val-0, weight-0   ],
                                      [val-1, weight-2   ],
                                      [val-2, weight-(-1)],
                                      [val-4, weight-7   ],
                                      [val-5, weight-(-8)],
                                      [val-6, weight-2   ]],
                                      8)).

ctr_draw_example(weighted_partial_alldiff,
                 ['VARIABLES','VALUES'],
                 [[[var-4],[var-0],[var-1],[var-2],[var-0],[var-0]],
                  [[val-0, weight-0   ],
                   [val-1, weight-2   ],
                   [val-2, weight-(-1)],
                   [val-4, weight-7   ],
                   [val-5, weight-(-8)],
                   [val-6, weight-2   ]]],
                 ['PRODUCT'],
                 [1-4,3-2,4-3],
                 ['SUM'([8,9,10])],
                 '','SUM(VALUES,weight)=2-1+7=8',
                 [2.5,2.145,1.8,1.8]).

ctr_see_also(weighted_partial_alldiff,
 [link('attached to cost variant', alldifferent,                      '',      []),
  link('attached to cost variant', alldifferent_except_0,             '',      []),
  link('common keyword',           global_cardinality_with_costs,     '%k',    ['weighted assignment']),
  link('common keyword',           minimum_weight_alldifferent,       '%k,%k', ['cost filtering constraint', 'weighted assignment']),
  link('common keyword',           sum_of_weights_of_distinct_values, '%k',    ['weighted assignment']),
  link('common keyword',           soft_alldifferent_var,             '%k',    ['soft constraint'],'\\\\ ')]).

ctr_key_words(weighted_partial_alldiff,['cost filtering constraint',
                                        'soft constraint'          ,
                                        'all different'            ,
                                        'assignment'               ,
                                        'relaxation'               ,
                                        'joker value'              ,
                                        'weighted assignment'      ,
                                        'subset sum'               ,
                                        'functional dependency'    ]).

ctr_persons(weighted_partial_alldiff,['Thiel S.']).

ctr_eval(weighted_partial_alldiff, [reformulation(weighted_partial_alldiff_r)]).

weighted_partial_alldiff_r(VARIABLES, UNDEFINED, VALUES, COST) :-
    collection(VARIABLES, [dvar]),
    integer(UNDEFINED),
    collection(VALUES, [int,int]),
    length(VALUES, N),
    N > 0,
	check_type(dvar, COST),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),
    get_attr2(VALUES, WEIGHTS),
    all_different(VALS),
    get_proj1(VALUES, CVALS),
    weighted_partial_alldiff0(VALS, WEIGHTS, UNDEFINED),
    weighted_partial_alldiff1(VARS, CVALS),
    weighted_partial_alldiff2(VARS, UNDEFINED),
    weighted_partial_alldiff4(VALS, WEIGHTS, VARS, TERM),
    call(COST #= TERM).

weighted_partial_alldiff0([UNDEFINED|_], [0|_], UNDEFINED) :-
    !.
weighted_partial_alldiff0([_V|R], [_W|S], UNDEFINED) :-
    weighted_partial_alldiff0(R, S, UNDEFINED).

weighted_partial_alldiff1([], _).
weighted_partial_alldiff1([VAR|R], VALUES) :-
    eval(in(VAR, VALUES)),
    weighted_partial_alldiff1(R, VALUES).

weighted_partial_alldiff2([], _).
weighted_partial_alldiff2([_], _) :- !.
weighted_partial_alldiff2([VAR|R], UNDEFINED) :-
    weighted_partial_alldiff3(R, VAR, UNDEFINED),
    weighted_partial_alldiff2(R, UNDEFINED).

weighted_partial_alldiff3([], _, _).
weighted_partial_alldiff3([UAR|R], VAR, UNDEFINED) :-
    UAR #\= VAR #\/ UAR #= UNDEFINED,
    weighted_partial_alldiff3(R, VAR, UNDEFINED).

weighted_partial_alldiff4([], [], _, 0).
weighted_partial_alldiff4([VAL|R], [WEIGHT|S], VARS, WEIGHT*B+T) :-
    weighted_partial_alldiff5(VARS, VAL, WEIGHT, TERM),
    call(B #<=> TERM),
    weighted_partial_alldiff4(R, S, VARS, T).

weighted_partial_alldiff5([], _, _, 0).
weighted_partial_alldiff5([VAR|R], VAL, WEIGHT, VAR #= VAL #\/ T) :-
    weighted_partial_alldiff5(R, VAL, WEIGHT, T).
