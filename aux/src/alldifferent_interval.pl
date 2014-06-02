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

ctr_date(alldifferent_interval,['20030820','20060803']).

ctr_origin(alldifferent_interval, 'Derived from %c.', [alldifferent]).

ctr_arguments(alldifferent_interval,
              ['VARIABLES'-collection(var-dvar),
               'SIZE_INTERVAL'-int             ]).

ctr_exchangeable(alldifferent_interval,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],intervals('SIZE_INTERVAL'),=,all,dontcare),
                  vals(['VARIABLES'^var],intervals('SIZE_INTERVAL'),=\=,all,in)]).

ctr_synonyms(alldifferent_interval,[alldiff_interval    ,
                                    alldistinct_interval]).

ctr_restrictions(alldifferent_interval,
                 [required('VARIABLES',var),
                  'SIZE_INTERVAL' > 0      ]).

ctr_typical(alldifferent_interval,
            [size('VARIABLES') > 1                     ,
             'SIZE_INTERVAL'   > 1                     ,
             'SIZE_INTERVAL'   < range('VARIABLES'^var)]).

ctr_contractible(alldifferent_interval, [], 'VARIABLES', any).

ctr_graph(alldifferent_interval,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var / 'SIZE_INTERVAL' = variables2^var / 'SIZE_INTERVAL'],
          ['MAX_NSCC' =< 1],
	    ['ONE_SUCC']).

ctr_example(alldifferent_interval,
            alldifferent_interval([[var-2],[var-4],[var-10]],3)).

ctr_draw_example(alldifferent_interval,
                 ['VARIABLES'],
                 [[[var-2],[var-4],[var-10]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 []).

% ctr_cond_imply(alldifferent_interval, alldifferent, [], [], ['VARIABLES']).

ctr_see_also(alldifferent_interval,
 [link('implied by',   all_min_dist, '',                  []),
  link(specialisation, alldifferent, '%e replaced by %e', [variable/constant,variable])]).

ctr_key_words(alldifferent_interval,['value constraint'                ,
                                     'interval'                        ,
                                     'all different'                   ,
                                     'sort based reformulation'        ,
                                     'automaton'                       ,
                                     'automaton with array of counters',
                                     'one\\_succ'                      ,
                                     'arc-consistency'                 ]).

ctr_eval(alldifferent_interval, [checker(alldifferent_interval_c),
				 reformulation(alldifferent_interval_r),
				 density(alldifferent_interval_d)]).

ctr_sol(alldifferent_interval,2,0,2,10,[1-6,2-4]).
ctr_sol(alldifferent_interval,3,0,3,24,[1-24]).
ctr_sol(alldifferent_interval,4,0,4,120,[1-120]).
ctr_sol(alldifferent_interval,5,0,5,720,[1-720]).
ctr_sol(alldifferent_interval,6,0,6,5040,[1-5040]).
ctr_sol(alldifferent_interval,7,0,7,40320,[1-40320]).
ctr_sol(alldifferent_interval,8,0,8,362880,[1-362880]).

alldifferent_interval_c(VARIABLES, SIZE_INTERVAL) :-
	collection(VARIABLES, [int]),
	integer(SIZE_INTERVAL),
	SIZE_INTERVAL > 0,
	get_attr1(VARIABLES, VARS),
	gen_quotient_fix(VARS, SIZE_INTERVAL, QUOTIENT),
	(QUOTIENT = [V,V|_] ->
	    fail
	;
	    sort(QUOTIENT, SORTED),
	    length(QUOTIENT, N),
	    length(SORTED, N)
	).

alldifferent_interval_r(VARIABLES, SIZE_INTERVAL) :-
	collection(VARIABLES, [dvar]),
	integer(SIZE_INTERVAL),
	SIZE_INTERVAL > 0,
	get_attr1(VARIABLES, VARS),
	gen_quotient(VARS, SIZE_INTERVAL, QUOTVARS),
	all_different(QUOTVARS).

alldifferent_interval_d(Density, VARIABLES, SIZE_INTERVAL) :-
	get_attr1(VARIABLES, VARS),
	min_member(Min, VARS),
	max_member(Max, VARS),
	NormalizedMin is (Min // SIZE_INTERVAL),
	NormalizedMax is (Max // SIZE_INTERVAL),
	Available is (NormalizedMax-NormalizedMin+1),
	length(VARS, Needed),
	Density is Needed/Available.
