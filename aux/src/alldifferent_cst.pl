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

ctr_date(alldifferent_cst,['20051104','20060803']).

ctr_origin(alldifferent_cst, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(alldifferent_cst,
              ['VARIABLES'-collection(var-dvar,cst-int)]).

ctr_exchangeable(alldifferent_cst,
                 [items('VARIABLES',all),
                  attrs('VARIABLES',[[var,cst]]),
                  translate(['VARIABLES'^var]),
                  translate(['VARIABLES'^cst])]).

ctr_synonyms(alldifferent_cst,[alldiff_cst    ,
                               alldistinct_cst]).

ctr_restrictions(alldifferent_cst,
                 [required('VARIABLES',[var,cst])]).

ctr_typical(alldifferent_cst,
            [size('VARIABLES')        > 2                  ,
             range('VARIABLES'^var)   > 1                  ,
             2*range('VARIABLES'^var) < 3*size('VARIABLES'),
             range('VARIABLES'^cst)   > 1                  ]).

ctr_contractible(alldifferent_cst, [], 'VARIABLES', any).

ctr_graph(alldifferent_cst,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var+variables1^cst = variables2^var+variables2^cst],
          ['MAX_NSCC' =< 1],
	     ['ONE_SUCC']).

ctr_example(alldifferent_cst,
            alldifferent_cst([[var-5, cst-0],
                              [var-1, cst-1],
                              [var-9, cst-0],
                              [var-3, cst-4]])).

ctr_draw_example(alldifferent_cst,
                 ['VARIABLES'],
                 [[[var-5, cst-0],[var-1, cst-1],[var-9, cst-0],[var-3, cst-4]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3,4-4],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 []).

ctr_see_also(alldifferent_cst,
 [link('implies (items to collection)', lex_alldifferent, '',                  []                          ),
  link('specialisation',                alldifferent,     '%e replaced by %e', [variable+constant,variable])]).

ctr_key_words(alldifferent_cst,['value constraint'                             ,
                                'all different'                                ,
                                'disequality'                                  ,
                                'sort based reformulation'                     ,
                                'bipartite matching'                           ,
                                'bipartite matching in convex bipartite graphs',
                                'convex bipartite graph'                       ,
                                'n-Amazons'                                    ,
                                'n-queens'                                     ,
                                'arc-consistency'                              ,
                                'one\\_succ'                                   ]).

ctr_eval(alldifferent_cst, [checker(alldifferent_cst_c),
			    reformulation(alldifferent_cst_r)]).

alldifferent_cst_r(VARIABLES) :-
	collection(VARIABLES, [dvar,int]),
	get_attr1(VARIABLES, VARS),
	get_attr2(VARIABLES, CSTS),
	gen_varcst(VARS, CSTS, VARCSTS),
	all_different(VARCSTS).

alldifferent_cst_c(VARIABLES) :-
	collection(VARIABLES, [int,int]),
	get_attr12_sum(VARIABLES, SUMS),
	sort(SUMS, SORTED),
	length(SUMS, N),
	length(SORTED, N).
