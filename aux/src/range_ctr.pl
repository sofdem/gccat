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

ctr_date(range_ctr,['20030820','20060813']).

ctr_origin(range_ctr, 'Arithmetic constraint.', []).

ctr_arguments(range_ctr,
              ['VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ,
               'R'-dvar                        ]).

ctr_exchangeable(range_ctr,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(range_ctr,
                 [size('VARIABLES') > 0           ,
                  required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(range_ctr,
            [size('VARIABLES')      > 1  ,
             range('VARIABLES'^var) > 1  ,
             in_list('CTR',[=,<,>=,>,=<])]).

ctr_pure_functional_dependency(range_ctr, [in_list('CTR',[=])]).

ctr_contractible(range_ctr, [in_list('CTR',[<,=<])], 'VARIABLES', any).

ctr_extensible(range_ctr, [in_list('CTR',[>=,>])], 'VARIABLES', any).

ctr_graph(range_ctr,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          ['TRUE'],
          ['CTR'('RANGE'('VARIABLES',var),'R')],
          []).

ctr_example(range_ctr,
            range_ctr([[var-1],[var-9],[var-4]], =, 9)).

ctr_draw_example(range_ctr,
                 ['VARIABLES'],
                 [[[var-1],[var-9],[var-4]]],
                 ['SELF'],
                 [1-1,2-2,3-3],
                 ['RANGE'([1,2,3])],
                 '','RANGE(VARIABLES,var)=9-1+1=9',
                 [2.145,2.145,1.9,1.9]).

ctr_see_also(range_ctr,
 [link('common keyword', sum_ctr,     '%k', ['arithmetic constraint']),
  link('common keyword', product_ctr, '%k', ['arithmetic constraint'])]).

ctr_key_words(range_ctr,['arithmetic constraint',
                         'range'                ]).

ctr_eval(range_ctr, [reformulation(range_ctr_r)]).

range_ctr_r(VARIABLES, CTR, R) :-
    collection(VARIABLES, [dvar]),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    check_type(dvar, R),
    length(VARIABLES, N),
    N > 0,
    get_attr1(VARIABLES, VARS),
    minimum(MIN, VARS),
    maximum(MAX, VARS),
    call_term_relop_value(MAX-MIN+1, CTR, R).
