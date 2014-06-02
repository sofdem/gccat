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
    ctr_pure_functional_dependency/2,
    ctr_functional_dependency/3,
    ctr_typical/2,
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

ctr_date(increasing_sum,['20110617']).

ctr_origin(increasing_sum, 'Conjoin %c and %c.', [increasing,sum_ctr]).

ctr_synonyms(increasing_sum, [increasing_sum_ctr,
                              increasing_sum_eq ]).

ctr_arguments(increasing_sum,
              ['VARIABLES'-collection(var-dvar),
               'S'-dvar                        ]).

ctr_restrictions(increasing_sum,
                 [required('VARIABLES',var),
                  increasing('VARIABLES')  ]).

ctr_typical(increasing_sum,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_functional_dependency(increasing_sum, 2, [1]).

ctr_predefined(increasing_sum).

ctr_example(increasing_sum,
            increasing_sum([[var-3],[var-3],[var-6],[var-8]], 20)).

ctr_cond_imply(increasing_sum, atmost_nvalue,     [minval('VARIABLES'^var) > 0], [], ['S','VARIABLES']).
ctr_cond_imply(increasing_sum, sum_of_increments, [minval('VARIABLES'^var) > 0], [], id               ).

ctr_see_also(increasing_sum,
 [link('implies',        increasing, '',   []),
  link('common keyword', sum_ctr,    '%k', ['sum'])]).

ctr_key_words(increasing_sum,['predefined constraint',
			      'order constraint'     ,
                              'arithmetic constraint',
                              'sum'                  ,
                              'bound-consistency'    ,
                              'symmetry'             ,
			      'functional dependency']).

ctr_persons(increasing_sum,['Petit T.'       ,
                            'R\\\'egin J.-C.',
                            'Beldiceanu N.'  ]).

ctr_eval(increasing_sum, [reformulation(increasing_sum_r)]).

ctr_sol(increasing_sum,2,0,2,6,[0-1,1-1,2-2,3-1,4-1]).
ctr_sol(increasing_sum,3,0,3,20,[0-1,1-1,2-2,3-3,4-3,5-3,6-3,7-2,8-1,9-1]).
ctr_sol(increasing_sum,4,0,4,70,[0-1,1-1,2-2,3-3,4-5,5-5,6-7,7-7,8-8,9-7,10-7,11-5,12-5,13-3,14-2,15-1,16-1]).
ctr_sol(increasing_sum,5,0,5,252,[0-1,1-1,2-2,3-3,4-5,5-7,6-9,7-11,8-14,9-16,10-18,11-19,12-20,13-20,14-19,15-18,16-16,17-14,18-11,19-9,20-7,21-5,22-3,23-2,24-1,25-1]).
ctr_sol(increasing_sum,6,0,6,924,[0-1,1-1,2-2,3-3,4-5,5-7,6-11,7-13,8-18,9-22,10-28,11-32,12-39,13-42,14-48,15-51,16-55,17-55,18-58,19-55,20-55,21-51,22-48,23-42,24-39,25-32,26-28,27-22,28-18,29-13,30-11,31-7,32-5,33-3,34-2,35-1,36-1]).
ctr_sol(increasing_sum,7,0,7,3432,[0-1,1-1,2-2,3-3,4-5,5-7,6-11,7-15,8-20,9-26,10-34,11-42,12-53,13-63,14-75,15-87,16-100,17-112,18-125,19-136,20-146,21-155,22-162,23-166,24-169,25-169,26-166,27-162,28-155,29-146,30-136,31-125,32-112,33-100,34-87,35-75,36-63,37-53,38-42,39-34,40-26,41-20,42-15,43-11,44-7,45-5,46-3,47-2,48-1,49-1]).
ctr_sol(increasing_sum,8,0,8,12870,[0-1,1-1,2-2,3-3,4-5,5-7,6-11,7-15,8-22,9-28,10-38,11-48,12-63,13-77,14-97,15-116,16-141,17-164,18-194,19-221,20-255,21-284,22-319,23-348,24-383,25-409,26-440,27-461,28-486,29-499,30-515,31-519,32-526,33-519,34-515,35-499,36-486,37-461,38-440,39-409,40-383,41-348,42-319,43-284,44-255,45-221,46-194,47-164,48-141,49-116,50-97,51-77,52-63,53-48,54-38,55-28,56-22,57-15,58-11,59-7,60-5,61-3,62-2,63-1,64-1]).

increasing_sum_r(VARIABLES, S) :-
    eval(increasing(VARIABLES)),
    eval(sum_ctr(VARIABLES, =, S)).
