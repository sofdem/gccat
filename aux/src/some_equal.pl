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

ctr_date(some_equal,['20110604']).

ctr_origin(some_equal, 'Derived from %c', [alldifferent]).

ctr_arguments(some_equal,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(some_equal,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(some_equal,[some_eq         ,
                         not_alldifferent,
                         not_alldiff     ,
                         not_alldistinct ,
                         not_distinct    ]).

ctr_restrictions(some_equal,
                 [required('VARIABLES',var),
                  size('VARIABLES') > 1    ]).

ctr_typical(some_equal,
            [size('VARIABLES')     > 2,
	     nval('VARIABLES'^var) > 2]).

ctr_extensible(some_equal, [], 'VARIABLES', any).

ctr_graph(some_equal,
          ['VARIABLES'],
          2,
          ['CLIQUE'(<)>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NARC' > 0],
          []).

ctr_example(some_equal,
            some_equal([[var-1],[var-4],[var-1],[var-6]])).

ctr_draw_example(some_equal,
                 ['VARIABLES'],
                 [[[var-1],[var-4],[var-1],[var-6]]],
                 ['CLIQUE'(<)],
                 [1-3],
                 ['NARC'],
                 '','NARC=1',
                 []).

ctr_see_also(some_equal,
 [link(negation, alldifferent, '', [])]).

ctr_key_words(some_equal,['value constraint'        ,
                          'sort based reformulation']).

ctr_eval(some_equal, [checker(some_equal_c),
		      reformulation(some_equal_r)]).

ctr_sol(some_equal,2,0,2,3,-).
ctr_sol(some_equal,3,0,3,40,-).
ctr_sol(some_equal,4,0,4,505,-).
ctr_sol(some_equal,5,0,5,7056,-).
ctr_sol(some_equal,6,0,6,112609,-).
ctr_sol(some_equal,7,0,7,2056832,-).
ctr_sol(some_equal,8,0,8,42683841,-).

some_equal_c(VARIABLES) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	sort(VARS, S),
	length(VARS, M),
	length(S, N),
	N < M.

some_equal_r(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	length(VARS, M),
	M > 1,
	M1 is M-1,
	N in 1..M1,
	nvalue(N, VARS).
