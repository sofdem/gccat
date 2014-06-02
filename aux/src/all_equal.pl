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

ctr_date(all_equal,['20081005','20100418']).

ctr_origin(all_equal, 'Derived from %c', [soft_all_equal_min_ctr]).

ctr_arguments(all_equal,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(all_equal,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(all_equal,[rel]).

ctr_restrictions(all_equal,
                 [required('VARIABLES',var),
                  size('VARIABLES') > 0    ]).

ctr_typical(all_equal,
            [size('VARIABLES')        >  2,
	     minval('VARIABLES'^var) =\= 0]).

ctr_contractible(all_equal, [], 'VARIABLES', any).

ctr_graph(all_equal,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NARC' = size('VARIABLES')-1],
	  []).

ctr_example(all_equal,
            all_equal([[var-5],[var-5],[var-5],[var-5]])).

ctr_draw_example(all_equal,
                 ['VARIABLES'],
                 [[[var-5],[var-5],[var-5],[var-5]]],
                 ['PATH'],
                 [1-2,2-3,3-4],
                 ['NARC'],
                 '','NARC=3',
                 []).

ctr_cond_imply(all_equal, some_equal, [size('VARIABLES') > 1], [], id).

ctr_see_also(all_equal,
 [link(implies,          consecutive_values,      '',                                                                []),
  link(implies,          decreasing,              '',                                                                []),
  link(implies,          increasing,              '',                                                                []),
  link(implies,          multi_global_contiguity, '',                                                                []),
  link(negation,         not_all_equal,           '',                                                                []),
  link('soft variant',   soft_all_equal_min_var,  '%k',                                                              ['variable-based violation measure']),
  link('soft variant',   soft_all_equal_max_var,  '',                                                                [],'\\\\ '),
  link('soft variant',   soft_all_equal_min_ctr,  '%k',                                                              ['decomposition-based violation measure']),
  link('generalisation', nvalue,                  'a variable counting the number of distinct values is introduced', []),
  link('specialisation', eq,                      'equality between just two variables',                             [])]).

ctr_key_words(all_equal,['value constraint']).

ctr_eval(all_equal, [checker(all_equal_c),
		     reformulation(all_equal_r)]).

ctr_sol(all_equal,2,0,2,3,-).
ctr_sol(all_equal,3,0,3,4,-).
ctr_sol(all_equal,4,0,4,5,-).
ctr_sol(all_equal,5,0,5,6,-).
ctr_sol(all_equal,6,0,6,7,-).
ctr_sol(all_equal,7,0,7,8,-).
ctr_sol(all_equal,8,0,8,9,-).

all_equal_c(VARIABLES) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	all_equal2(VARS).

all_equal_r(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	all_equal1(VARS).

all_equal1([]).
all_equal1([_]) :- !.
all_equal1([V1,V2|R]) :-
	V1 #= V2,
	all_equal1([V2|R]).

all_equal2([V,V|R]) :-
	!,
	all_equal2([V|R]).
all_equal2([_]) :- !.
all_equal2([]).
