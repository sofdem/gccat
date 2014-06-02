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

ctr_date(product_ctr,['20030820','20060813','20070902']).

ctr_origin(product_ctr, 'Arithmetic constraint.', []).

ctr_arguments(product_ctr,
              ['VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ,
               'VAR'-dvar                      ]).

ctr_exchangeable(product_ctr,
                 [items('VARIABLES',all)]).

ctr_restrictions(product_ctr,
                 [required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(product_ctr,
            [size('VARIABLES')      >   1,
             size('VARIABLES')      <  10,
             range('VARIABLES'^var) >   1,
             'VARIABLES'^var       =\=  0,
             in_list('CTR',[=,<,>=,>,=<]),
             'VAR'                 =\=  0]).

ctr_pure_functional_dependency(product_ctr, [in_list('CTR',[=])]).

ctr_contractible(product_ctr, [in_list('CTR',[<,=<]),minval('VARIABLES'^var)>0], 'VARIABLES', any).

% product_ctr('VARIABLES1', =, 'VAR1') and
% product_ctr('VARIABLES2', =, 'VAR2') =>
% product_ctr(union('VARIABLES1','VARIABLES2'), =, 'VAR1'*'VAR2')
ctr_aggregate(product_ctr, [in_list('CTR',[=])], [union, id, *]).

ctr_graph(product_ctr,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          ['TRUE'],
          ['CTR'('PROD'('VARIABLES',var),'VAR')],
          []).

ctr_example(product_ctr,
            product_ctr([[var-2],[var-1],[var-4]], =, 8)).

ctr_draw_example(product_ctr,
                 ['VARIABLES'],
                 [[[var-2],[var-1],[var-4]]],
                 ['SELF'],
                 [1-1,2-2,3-3],
                 ['PRODUCT'([1,2,3])],
                 '','PROD(VARIABLES,var)=2*1*4=8',
                 [2.145,2.145,2.2,2.2]).

ctr_see_also(product_ctr,
 [link('common keyword', sum_ctr,   '%k', ['arithmetic constraint']),
  link('common keyword', range_ctr, '%k', ['arithmetic constraint'])]).

ctr_key_words(product_ctr,['arithmetic constraint',
                           'product'              ]).

ctr_eval(product_ctr, [checker(product_ctr_c)      ,
		       reformulation(product_ctr_r)]).

product_ctr_r(VARIABLES, CTR, VAR) :-
    collection(VARIABLES, [dvar]),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    check_type(dvar, VAR),
    get_attr1(VARIABLES, VARS),
    build_prod_var(VARS, PROD),
    call_term_relop_value(PROD, CTR, VAR).

product_ctr_c(VARIABLES, =, VAR) :-
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    prodlist(VARS, VAR).
product_ctr_c(VARIABLES, =\=, VAR) :-
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    prodlist(VARS, PROD),
    PROD =\= VAR.
product_ctr_c(VARIABLES, <, VAR) :-
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    prodlist(VARS, PROD),
    PROD < VAR.
product_ctr_c(VARIABLES, >=, VAR) :-
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    prodlist(VARS, PROD),
    PROD >= VAR.
product_ctr_c(VARIABLES, >, VAR) :-
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    prodlist(VARS, PROD),
    PROD > VAR.
product_ctr_c(VARIABLES, =<, VAR) :-
    collection(VARIABLES, [int]),
    check_type(int, VAR),
    get_attr1(VARIABLES, VARS),
    prodlist(VARS, PROD),
    PROD =< VAR.

prodlist(Numbers, Prod) :-
    (   foreach(X, Numbers),
        fromto(1, P0, P, Prod)
    do  P is P0*X
    ).
