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

ctr_date(alldifferent_between_sets,['20030820','20051008','20060803']).

ctr_origin(alldifferent_between_sets, 'ILOG', []).

ctr_arguments(alldifferent_between_sets,
              ['VARIABLES'-collection(var-svar)]).

ctr_exchangeable(alldifferent_between_sets,
                 [items('VARIABLES',all)]).

ctr_synonyms(alldifferent_between_sets,[all_null_intersect      ,
                                        alldiff_between_sets    ,
                                        alldistinct_between_sets,
                                        alldiff_on_sets         ,
                                        alldistinct_on_sets     ,
                                        alldifferent_on_sets    ]).

ctr_restrictions(alldifferent_between_sets,
                 [required('VARIABLES',var)]).

ctr_typical(alldifferent_between_sets,
            [size('VARIABLES') > 2]).

ctr_contractible(alldifferent_between_sets, [], 'VARIABLES', any).

ctr_graph(alldifferent_between_sets,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [eq_set(variables1^var,variables2^var)],
          ['MAX_NSCC' =< 1],
	  ['ONE_SUCC']).

ctr_example(alldifferent_between_sets,
            alldifferent_between_sets([[var-{3,5}],[var-{}],[var-{3}],[var-{3,5,7}]])).

ctr_draw_example(alldifferent_between_sets,
                ['VARIABLES'],
                [[[var-{3,5}],[var-{}],[var-{3}],[var-{3,5,7}]]],
                ['CLIQUE'],
                [1-1,2-2,3-3,4-4],
                ['MAX_NSCC'([1])],
                '','MAX_NSCC=1',
                [2.145,2.145,3,2.145]).

ctr_see_also(alldifferent_between_sets,
 [link(specialisation,              alldifferent,         '%e replaced by %e', ['set~variable','variable']),
  link('common keyword',            link_set_to_booleans, '%k',                ['constraint involving set variables']),
  link('used in graph description', eq_set,               '',                  [])]).

ctr_key_words(alldifferent_between_sets,['all different'                     ,
                                         'disequality'                       ,
                                         'bipartite matching'                ,
                                         'constraint involving set variables',
                                         'one\\_succ'                        ]).

ctr_persons(alldifferent_between_sets,['Quimper C.-G.',
                                       'Walsh T.'     ]).
