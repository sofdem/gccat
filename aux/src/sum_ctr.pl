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

ctr_date(sum_ctr,['20030820','20040807','20060817']).

ctr_origin(sum_ctr, 'Arithmetic constraint.', []).

ctr_arguments(sum_ctr,
              ['VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ,
               'VAR'-dvar                      ]).

ctr_exchangeable(sum_ctr,
                 [items('VARIABLES',all)]).

ctr_synonyms(sum_ctr,[constant_sum,sum,linear,scalar_product]).

ctr_restrictions(sum_ctr,
                 [required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(sum_ctr,
            [size('VARIABLES')      > 1  ,
             range('VARIABLES'^var) > 1  ,
             in_list('CTR',[=,<,>=,>,=<])]).

ctr_pure_functional_dependency(sum_ctr, [in_list('CTR',[=])]).

ctr_contractible(sum_ctr, [in_list('CTR',[<,=<]),minval('VARIABLES'^var)>=0], 'VARIABLES', any).
ctr_contractible(sum_ctr, [in_list('CTR',[>=,>]),maxval('VARIABLES'^var)=<0], 'VARIABLES', any).

ctr_extensible(sum_ctr, [in_list('CTR',[>=,>]),minval('VARIABLES'^var)>=0], 'VARIABLES', any).
ctr_extensible(sum_ctr, [in_list('CTR',[<,=<]),maxval('VARIABLES'^var)=<0], 'VARIABLES', any).

% sum_ctr('VARIABLES1', CTR, 'VAR1') and
% sum_ctr('VARIABLES2', CTR, 'VAR2') =>
% sum_ctr(union('VARIABLES1','VARIABLES2'), CTR, 'VAR1'+'VAR2')
ctr_aggregate(sum_ctr, [], [union, id, +]).

ctr_graph(sum_ctr,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          ['TRUE'],
          ['CTR'('SUM'('VARIABLES',var),'VAR')],
          []).

ctr_example(sum_ctr,
            sum_ctr([[var-1],[var-1],[var-4]], =, 6)).

ctr_draw_example(sum_ctr,
                 ['VARIABLES'],
                 [[[var-1],[var-1],[var-4]]],
                 ['SELF'],
                 [1-1,2-2,3-3],
                 ['SUM'([1,2,3])],
                 '','SUM(VARIABLES,var)=1+1+4=6',
                 [2.145,2.145,2,1.7]).

ctr_cond_imply(sum_ctr,
	       sum_squares_ctr,
               ['VARIABLES'^var >= 0, 'VARIABLES'^var =< 1],
               ['VARIABLES'^var >= 0, 'VARIABLES'^var =< 1],
               id).
ctr_cond_imply(sum_ctr,
	       sum_cubes_ctr,
               ['VARIABLES'^var >= -1, 'VARIABLES'^var =< 1],
               ['VARIABLES'^var >= -1, 'VARIABLES'^var =< 1],
               id).
ctr_cond_imply(sum_ctr,
	       sum_powers5_ctr,
               ['VARIABLES'^var >= -1, 'VARIABLES'^var =< 1],
               ['VARIABLES'^var >= -1, 'VARIABLES'^var =< 1],
               id).

ctr_cond_imply(sum_ctr, increasing_sum, [in_list('CTR',[=]), increasing('VARIABLES')], [], ['VARIABLES','VAR']).

ctr_see_also(sum_ctr,
 [link('implied by',                 arith_sliding,    '',                                                         []),
  link('generalisation',             scalar_product,   '%k where all coefficients are not necessarly equal to %e', ['arithmetic constraint',1]),
  link('assignment dimension added', interval_and_sum, '%k corresponding to intervals is added',                   ['assignment dimension']),
  link('system of constraints',      sliding_sum,      '',                                                         []),
  link('common keyword',             arith_sliding,    '%k',                                                       ['arithmetic constraint']),
  link('common keyword',             sum_set,          '%k',                                                       ['arithmetic constraint']),
  link('common keyword',             product_ctr,      '%k',                                                       ['arithmetic constraint']),
  link('common keyword',             range_ctr,        '%k',                                                       ['arithmetic constraint']),
  link('common keyword',             sum,              '%k',                                                       ['sum']),
  link('common keyword',             sum_squares_ctr,  '%k',                                                       ['sum']),
  link('common keyword',             sum_cubes_ctr,    '%k',                                                       ['sum']),
  link('common keyword',             sum_powers4_ctr,  '%k',                                                       ['sum']),
  link('common keyword',             sum_powers5_ctr,  '%k',                                                       ['sum']),
  link('common keyword',             sum_powers6_ctr,  '%k',                                                       ['sum']),
  link('common keyword',             increasing_sum,   '%k',                                                       ['sum'])]).

ctr_key_words(sum_ctr,['arithmetic constraint'                     ,
                       'sum'                                       ,
                       'regret based heuristics'                   ,
                       'regret based heuristics in matrix problems']).

ctr_eval(sum_ctr, [checker(sum_ctr_c),
		   reformulation(sum_ctr_r)]).

sum_ctr_r(VARIABLES, CTR, VAR) :-
    collection(VARIABLES, [dvar]),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    check_type(dvar, VAR),
    get_attr1(VARIABLES, VARS),
    build_sum_var(VARS, SUM),
    call_term_relop_value(SUM, CTR, VAR).

sum_ctr_c(VARIABLES, =, VAR) :- !,
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    sumlist(VARS, VAR).
sum_ctr_c(VARIABLES, =\=, VAR) :- !,
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    sumlist(VARS, SUM),
    SUM =\= VAR.
sum_ctr_c(VARIABLES, <, VAR) :- !,
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    sumlist(VARS, SUM),
    SUM < VAR.
sum_ctr_c(VARIABLES, >=, VAR) :- !,
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    sumlist(VARS, SUM),
    SUM >= VAR.
sum_ctr_c(VARIABLES, >, VAR) :- !,
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    sumlist(VARS, SUM),
    SUM > VAR.
sum_ctr_c(VARIABLES, =<, VAR) :-
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    sumlist(VARS, SUM),
    SUM =< VAR.
