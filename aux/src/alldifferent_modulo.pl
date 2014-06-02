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

ctr_date(alldifferent_modulo,['20030820','20060803']).

ctr_origin(alldifferent_modulo, 'Derived from %c.', [alldifferent]).

ctr_arguments(alldifferent_modulo,
              ['VARIABLES'-collection(var-dvar),
               'M'-int                         ]).

ctr_exchangeable(alldifferent_modulo,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],mod('M'),=,all,dontcare),
                  vals(['VARIABLES'^var],mod('M'),=\=,all,in)]).

ctr_synonyms(alldifferent_modulo,[alldiff_modulo    ,
                                  alldistinct_modulo]).

ctr_restrictions(alldifferent_modulo,
                 [required('VARIABLES',var),
                  'M' > 0                  ,
                  'M' >= size('VARIABLES') ]).

ctr_typical(alldifferent_modulo,
            [size('VARIABLES') > 2,
             'M'               > 1]).

ctr_contractible(alldifferent_modulo, [], 'VARIABLES', any).

ctr_graph(alldifferent_modulo,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var mod 'M' = variables2^var mod 'M'],
          ['MAX_NSCC' =< 1],
	  ['ONE_SUCC']).

ctr_example(alldifferent_modulo,
            alldifferent_modulo([[var-25],[var-1],[var-14],[var-3]],
                                5)).

ctr_draw_example(alldifferent_modulo,
                 ['VARIABLES'],
                 [[[var-25],[var-1],[var-14],[var-3]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3,4-4],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 []).

ctr_see_also(alldifferent_modulo,
 [link(implies,        soft_alldifferent_var, '',                  []),
  link(specialisation, alldifferent,          '%e replaced by %e', [variable mod constant,variable])]).

ctr_key_words(alldifferent_modulo,['value constraint'                ,
                                   'modulo'                          ,
                                   'all different'                   ,
                                   'sort based reformulation'        ,
                                   'automaton'                       ,
                                   'automaton with array of counters',
                                   'one\\_succ'                      ,
                                   'arc-consistency'                 ]).

ctr_eval(alldifferent_modulo, [reformulation(alldifferent_modulo_r)]).

ctr_sol(alldifferent_modulo,2,0,2,4,[2-4]).
ctr_sol(alldifferent_modulo,3,0,3,12,[3-12]).
ctr_sol(alldifferent_modulo,4,0,4,48,[4-48]).
ctr_sol(alldifferent_modulo,5,0,5,240,[5-240]).
ctr_sol(alldifferent_modulo,6,0,6,1440,[6-1440]).
ctr_sol(alldifferent_modulo,7,0,7,10080,[7-10080]).
ctr_sol(alldifferent_modulo,8,0,8,80640,[8-80640]).

alldifferent_modulo_r(VARIABLES, M) :-
	collection(VARIABLES, [dvar]),
	integer(M),
	M > 0,
	length(VARIABLES, N),
	M >= N,
	get_attr1(VARIABLES, VARS),
	gen_remainder(VARS, M, REMVARS),
	all_different(REMVARS).
