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

ctr_date(compare_and_count,['20110628']).

ctr_origin(compare_and_count, 'Generalise %c', [discrepancy]).

ctr_arguments(compare_and_count,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'COMPARE'-atom                   ,
               'COUNT'-atom                     ,
               'LIMIT'-dvar                     ]).

ctr_restrictions(compare_and_count,
                 [size('VARIABLES1') = size('VARIABLES2'),
                  required('VARIABLES1',var)             ,
                  required('VARIABLES2',var)             ,   
                  in_list('COMPARE',[=,=\=,<,>=,>,=<])   ,
                  in_list('COUNT',  [=,=\=,<,>=,>,=<])   ,
                  'LIMIT' >= 0                           ]).

ctr_typical(compare_and_count,
            [size('VARIABLES1')      > 1   ,
             range('VARIABLES1'^var) > 1   ,
             range('VARIABLES2'^var) > 1   ,
             in_list('COMPARE',[=])        ,
             in_list('COUNT',[=,<,>=,>,=<]),
             'LIMIT' > 0                   ,
             'LIMIT' < size('VARIABLES1')  ]).

ctr_pure_functional_dependency(compare_and_count, [in_list('COUNT',[=])]).

ctr_contractible(compare_and_count, [in_list('COUNT',[<,=<])], ['VARIABLES1','VARIABLES2'], any).

ctr_extensible(compare_and_count, [in_list('COUNT',[>=,>])], ['VARIABLES1','VARIABLES2'], any).

ctr_predefined(compare_and_count).

ctr_example(compare_and_count,
            compare_and_count([[var-4],[var-5],[var-5],[var-4],[var-5]],
                              [[var-4],[var-2],[var-5],[var-1],[var-5]],=,=<,3)).

ctr_see_also(compare_and_count,
 [link('common keyword', count, '%k', ['counting constraint'])]).

ctr_key_words(compare_and_count,['predefined constraint',
				 'counting constraint'  ]).

ctr_eval(compare_and_count, [reformulation(compare_and_count_r)]).

compare_and_count_r(VARIABLES1, VARIABLES2, COMPARE, COUNT, LIMIT) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
	N1 = N2,
	memberchk(COMPARE, [=, =\=, <, >=, >, =<]),
	memberchk(COUNT,   [=, =\=, <, >=, >, =<]),
	check_type(dvar, LIMIT),
	LIMIT #>= 0,
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
	compare_and_count_r1(VARS1, VARS2, COMPARE, TERM),
	compare_and_count_r2(COUNT, TERM, LIMIT).

compare_and_count_r1([], [], _, 0).
compare_and_count_r1([V1|R1], [V2|R2], =, B+T) :-
	V1 #= V2 #<=> B,
	compare_and_count_r1(R1, R2, =, T).
compare_and_count_r1([V1|R1], [V2|R2], =\=, B+T) :-
	V1 #\= V2 #<=> B,
	compare_and_count_r1(R1, R2, =\=, T).
compare_and_count_r1([V1|R1], [V2|R2], <, B+T) :-
	V1 #< V2 #<=> B,
	compare_and_count_r1(R1, R2, <, T).
compare_and_count_r1([V1|R1], [V2|R2], >=, B+T) :-
	V1 #>= V2 #<=> B,
	compare_and_count_r1(R1, R2, >=, T).
compare_and_count_r1([V1|R1], [V2|R2], >, B+T) :-
	V1 #> V2 #<=> B,
	compare_and_count_r1(R1, R2, >, T).
compare_and_count_r1([V1|R1], [V2|R2], =<, B+T) :-
	V1 #=< V2 #<=> B,
	compare_and_count_r1(R1, R2, =<, T).

compare_and_count_r2(=  , TERM, LIMIT) :- call(TERM #=  LIMIT).
compare_and_count_r2(=\=, TERM, LIMIT) :- call(TERM #\= LIMIT).
compare_and_count_r2(<  , TERM, LIMIT) :- call(TERM #<  LIMIT).
compare_and_count_r2(>= , TERM, LIMIT) :- call(TERM #>= LIMIT).
compare_and_count_r2(>  , TERM, LIMIT) :- call(TERM #>  LIMIT).
compare_and_count_r2(=< , TERM, LIMIT) :- call(TERM #=< LIMIT).
