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

ctr_date(sum_of_increments,['20111105']).

ctr_origin(sum_of_increments, '\\cite{Brand09}', []).

ctr_arguments(sum_of_increments,
              ['VARIABLES'-collection(var-dvar),
               'LIMIT'-dvar                    ]).

ctr_exchangeable(sum_of_increments,
                 [translate(['VARIABLES'^var,'LIMIT']),
		  items('VARIABLES',reverse),
		  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_synonyms(sum_of_increments,[increments_sum,
				incr_sum      ,
		                sum_incr      ,
		                sum_increments]).

ctr_restrictions(sum_of_increments,
                 [required('VARIABLES',var),
		  'VARIABLES'^var >= 0     ,
		  'LIMIT'         >= 0     ]).

ctr_typical(sum_of_increments,
            [size('VARIABLES')       > 2,
             range('VARIABLES'^var)  > 1,
	     maxval('VARIABLES'^var) > 0, 
	     'LIMIT'                 > 0,
	     'LIMIT'                =< (size('VARIABLES')*range('VARIABLES'^var)) / 2]).

ctr_contractible(sum_of_increments, [], 'VARIABLES', prefix).
ctr_contractible(sum_of_increments, [], 'VARIABLES', suffix).

ctr_predefined(sum_of_increments).

ctr_example(sum_of_increments,
            [sum_of_increments([[var-4],[var-4],[var-3],[var-4],[var-6]], 7)]).

ctr_key_words(sum_of_increments,['predefined constraint',
				 'difference'           ,
                                 'sum'                  ,
				 'bound-consistency'    ]).

ctr_persons(sum_of_increments,['Brand S.']).

ctr_eval(sum_of_increments, [reformulation(sum_of_increments_r)]).

ctr_sol(sum_of_increments,2,0,2,14,[0-1,1-4,2-9]).
ctr_sol(sum_of_increments,3,0,3,145,[0-1,1-7,2-23,3-54,4-60]).
ctr_sol(sum_of_increments,4,0,4,2875,[0-1,1-11,2-51,3-156,4-375,5-485,6-563,7-608,8-625]).
ctr_sol(sum_of_increments,5,0,5,51415,[0-1,1-16,2-101,3-396,4-1167,5-2848,6-4263,7-5568,8-6616,9-7314,10-7650,11-7720,12-7755]).
ctr_sol(sum_of_increments,6,0,6,1210104,[0-1,1-22,2-183,3-904,4-3235,5-9318,6-22981,7-38836,8-56703,9-74658,10-90639,11-102875,12-110425,13-113827,14-115857,15-116942,16-117437,17-117612,18-117649]).
ctr_sol(sum_of_increments,7,0,7,28573741,[0-1,1-29,2-309,3-1891,4-8135,5-27483,6-77947,7-193742,8-359880,9-578511,10-837441,11-1115687,12-1386029,13-1619993,14-1795694,15-1908968,16-1988222,17-2039616,18-2069933,19-2085763,20-2092817,21-2095436,22-2096360,23-2096822,24-2097032]).
ctr_sol(sum_of_increments,8,0,8,801944469,[0-1,1-37,2-493,3-3679,4-18835,5-74143,6-240751,7-675244,8-1688427,9-3369015,10-5865915,11-9220695,12-13354545,13-18051195,14-22965651,15-27670800,16-31755573,17-34989993,18-37574073,19-39526569,20-40912205,21-41827847,22-42386387,23-42700112,24-42865683,25-42953199,26-43002171,27-43027581,28-43039551,29-43044507,30-43046215,31-43046656,32-43046721]).

sum_of_increments_r([], _) :- !.
sum_of_increments_r(VARIABLES, LIMIT) :-
    collection(VARIABLES, [dvar_gteq(0)]),
    check_type(dvar_gteq(0), LIMIT),
    get_attr1(VARIABLES, VARS),
    fd_max(LIMIT, MaxL),
    sum_of_increments_r1([0|VARS], MaxL, SUM),
    call(SUM #=< LIMIT).

sum_of_increments_r1([_], _, 0) :- !.
sum_of_increments_r1([V1,V2|R], MaxL, S2+S) :-
    S2 in 0..MaxL,
    V2-V1 #=< S2,
    sum_of_increments_r1([V2|R], MaxL, S).
