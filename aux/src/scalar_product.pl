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

ctr_date(scalar_product,['20090415']).

ctr_origin(scalar_product, 'Arithmetic constraint.', []).

ctr_arguments(scalar_product,
              ['LINEARTERM'-collection(coeff-int, var-dvar),
               'CTR'-atom                                  ,
               'VAL'-dvar                                  ]).

ctr_exchangeable(scalar_product,
                 [items('LINEARTERM',all),
                  attrs('LINEARTERM',[[coeff,var]])]).

ctr_synonyms(scalar_product,[equation   ,
                             linear     ,
                             sum_weight ,
                             weightedSum]).

ctr_restrictions(scalar_product,
                 [required('LINEARTERM',[coeff,var]),
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(scalar_product,
            [size('LINEARTERM')        >  1,
             range('LINEARTERM'^coeff) >  1,
             range('LINEARTERM'^var)   >  1,
             in_list('CTR',[=,<,>=,>,=<])  ]).

ctr_pure_functional_dependency(scalar_product, [in_list('CTR',[=])]).

ctr_contractible(scalar_product, [in_list('CTR',[<,=<]),minval('LINEARTERM'^coeff)>=0,minval('LINEARTERM'^var)>=0], 'LINEARTERM', any).

ctr_extensible(scalar_product, [in_list('CTR',[>=,>]),minval('LINEARTERM'^coeff)>=0,minval('LINEARTERM'^var)>=0], 'LINEARTERM', any).

% scalar_product('LINEARTERM1', CTR, 'VAL1') and
% scalar_product('LINEARTERM2', CTR, 'VAL2') =>
% scalar_product(union('LINEARTERM1','LINEARTERM2'), CTR, 'VAL1'+'VAL2')
ctr_aggregate(scalar_product, [], [union, id, +]).

ctr_predefined(scalar_product).

ctr_example(scalar_product,
            scalar_product([[coeff-1,var-1],[coeff-3,var-1],[coeff-1,var-4]], =, 8)).

ctr_see_also(scalar_product,
 [link('specialisation', sum_ctr, '%k where all coefficients are equal to %e', ['arithmetic constraint',1])]).

ctr_key_words(scalar_product,['predefined constraint',
                              'arithmetic constraint',
                              'sum'                  ,
                              'duplicated variables' ]).

ctr_persons(scalar_product,['Harvey W.' ,
                            'Schimpf J.']).

ctr_eval(scalar_product, [builtin(scalar_product_b)]).

scalar_product_b(LINEARTERM, =, VAR) :- !,
    collection(LINEARTERM, [int,dvar]),
    check_type(dvar, VAR),
    get_attr1(LINEARTERM, COEFFS),
    get_attr2(LINEARTERM, VARS),
    scalar_product(COEFFS, VARS, #=, VAR).
scalar_product_b(LINEARTERM, =\=, VAR) :- !,
    collection(LINEARTERM, [int,dvar]),
    check_type(dvar, VAR),
    get_attr1(LINEARTERM, COEFFS),
    get_attr2(LINEARTERM, VARS),
    scalar_product(COEFFS, VARS, #\=, VAR).
scalar_product_b(LINEARTERM, <, VAR) :- !,
    collection(LINEARTERM, [int,dvar]),
    check_type(dvar, VAR),
    get_attr1(LINEARTERM, COEFFS),
    get_attr2(LINEARTERM, VARS),
    scalar_product(COEFFS, VARS, #<, VAR).
scalar_product_b(LINEARTERM, >=, VAR) :- !,
    collection(LINEARTERM, [int,dvar]),
    check_type(dvar, VAR),
    get_attr1(LINEARTERM, COEFFS),
    get_attr2(LINEARTERM, VARS),
    scalar_product(COEFFS, VARS, #>=, VAR).
scalar_product_b(LINEARTERM, >, VAR) :- !,
    collection(LINEARTERM, [int,dvar]),
    check_type(dvar, VAR),
    get_attr1(LINEARTERM, COEFFS),
    get_attr2(LINEARTERM, VARS),
    scalar_product(COEFFS, VARS, #>, VAR).
scalar_product_b(LINEARTERM, =<, VAR) :-
    collection(LINEARTERM, [int,dvar]),
    check_type(dvar, VAR),
    get_attr1(LINEARTERM, COEFFS),
    get_attr2(LINEARTERM, VARS),
    scalar_product(COEFFS, VARS, #=<, VAR).
