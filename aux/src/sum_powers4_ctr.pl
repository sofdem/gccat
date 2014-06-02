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

ctr_date(sum_powers4_ctr,['20120403']).

ctr_origin(sum_powers4_ctr, 'Arithmetic constraint.', []).

ctr_arguments(sum_powers4_ctr,
              ['VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ,
               'VAR'-dvar                      ]).

ctr_exchangeable(sum_powers4_ctr,
                 [items('VARIABLES',all)]).

ctr_synonyms(sum_powers4_ctr,[sum_powers4, sum_of_powers4, sum_of_powers4_ctr]).

ctr_restrictions(sum_powers4_ctr,
                 [required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(sum_powers4_ctr,
            [size('VARIABLES')      > 1  ,
             range('VARIABLES'^var) > 1  ,
             in_list('CTR',[=,<,>=,>,=<])]).

ctr_pure_functional_dependency(sum_powers4_ctr, [in_list('CTR',[=])]).

ctr_contractible(sum_powers4_ctr, [in_list('CTR',[<,=<])], 'VARIABLES', any).

ctr_extensible(sum_powers4_ctr, [in_list('CTR',[>=,>])], 'VARIABLES', any).

% sum_powers4_ctr('VARIABLES1', CTR, 'VAR1') and
% sum_powers4_ctr('VARIABLES2', CTR, 'VAR2') =>
% sum_powers4_ctr(union('VARIABLES1','VARIABLES2'), CTR, 'VAR1'+'VAR2')
ctr_aggregate(sum_powers4_ctr, [], [union, id, +]).

ctr_predefined(sum_powers4_ctr).

ctr_example(sum_powers4_ctr,
            sum_powers4_ctr([[var-1],[var-1],[var-2]], =, 18)).

ctr_see_also(sum_powers4_ctr,
 [link('common keyword', sum_ctr,         '%k', ['sum']),
  link('common keyword', sum_squares_ctr, '%k', ['sum']),
  link('common keyword', sum_cubes_ctr,   '%k', ['sum']),
  link('common keyword', sum_powers5_ctr, '%k', ['sum']),
  link('common keyword', sum_powers6_ctr, '%k', ['sum'])]).

ctr_key_words(sum_powers4_ctr,['predefined constraint',
			       'arithmetic constraint',
                               'sum'                  ]).

ctr_eval(sum_powers4_ctr, [reformulation(sum_powers4_ctr_r)]).

sum_powers4_ctr_r(VARIABLES, CTR, VAR) :-
    collection(VARIABLES, [dvar]),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    check_type(dvar, VAR),
    get_attr1(VARIABLES, VARS),
    build_sum_powers4_var(VARS, SUM_POWERS4),
    call_term_relop_value(SUM_POWERS4, CTR, VAR).
