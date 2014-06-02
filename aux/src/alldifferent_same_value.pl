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

ctr_date(alldifferent_same_value,['20000128','20030820','20060803']).

ctr_origin(alldifferent_same_value, 'Derived from %c.', [alldifferent]).

ctr_arguments(alldifferent_same_value,
              ['NSAME'-dvar                     ,
               'VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(alldifferent_same_value,
                 [items_sync('VARIABLES1','VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_synonyms(alldifferent_same_value,[alldiff_same_value    ,
                                      alldistinct_same_value]).

ctr_restrictions(alldifferent_same_value,
                 ['NSAME' >= 0                         ,
                  'NSAME' =< size('VARIABLES1')        ,
                  size('VARIABLES1')=size('VARIABLES2'),
                  required('VARIABLES1',var)           ,
                  required('VARIABLES2',var)           ]).

ctr_typical(alldifferent_same_value,
            ['NSAME' < size('VARIABLES1'),
             size('VARIABLES1') > 2      ]).

ctr_functional_dependency(alldifferent_same_value, 1, [2,3]).

ctr_graph(alldifferent_same_value,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'('CLIQUE','LOOP',=)>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['MAX_NSCC'     =< 1     ,
           'NARC_NO_LOOP' = 'NSAME'],
	  []).

ctr_example(alldifferent_same_value,
            alldifferent_same_value(2,
                                    [[var-7],[var-3],[var-1],[var-5]],
                                    [[var-1],[var-3],[var-1],[var-7]])).

ctr_cond_imply(alldifferent_same_value, differ_from_exactly_k_pos, [2*'NSAME' = size('VARIABLES1')], [], id).

ctr_see_also(alldifferent_same_value,
 [link('root concept', alldifferent, '', [])]).

ctr_key_words(alldifferent_same_value,['proximity constraint'            ,
                                       'sort based reformulation'        ,
                                       'automaton'                       ,
                                       'automaton with array of counters',
                                       'functional dependency'           ]).

ctr_eval(alldifferent_same_value, [reformulation(alldifferent_same_value_r)]).

alldifferent_same_value_r(NSAME,VARIABLES1, VARIABLES2) :-
	check_type(dvar, NSAME),
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
	NSAME #>= 0,
	NSAME #=< N1,
	N1 = N2,	
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
	all_different(VARS1),
	alldifferent_same_value1(VARS1, VARS2, SUMBOOLS),
	call(NSAME #= SUMBOOLS).

alldifferent_same_value1([], [], 0).
alldifferent_same_value1([V1|R1], [V2|R2], B+R) :-
	V1 #= V2 #<=> B,
	alldifferent_same_value1(R1, R2, R).
