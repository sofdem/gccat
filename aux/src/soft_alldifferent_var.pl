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
    ctr_total_relation/1,
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

ctr_date(soft_alldifferent_var,['20030820','20060815','20090926']).

ctr_origin(soft_alldifferent_var, '\\cite{PetitReginBessiere01}', []).

ctr_arguments(soft_alldifferent_var,
              ['C'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(soft_alldifferent_var,
                 [vals(['C'],int,<,dontcare,dontcare),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(soft_alldifferent_var,[soft_alldiff_var         ,
                                    soft_alldistinct_var     ,
                                    soft_alldiff_min_var     ,
                                    soft_alldifferent_min_var,
                                    soft_alldistinct_min_var ]).

ctr_restrictions(soft_alldifferent_var,
                 ['C' >= 0                 ,
                  required('VARIABLES',var)]).

ctr_typical(soft_alldifferent_var,
            ['C'               > 0                ,
             2*'C'            =< size('VARIABLES'),
             size('VARIABLES') > 1                ,
	     some_equal('VARIABLES')              ]).

ctr_contractible(soft_alldifferent_var, [], 'VARIABLES', any).

ctr_total_relation(soft_alldifferent_var).

ctr_graph(soft_alldifferent_var,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSCC' >= size('VARIABLES') - 'C'],
          []).

ctr_example(soft_alldifferent_var,
            [soft_alldifferent_var(3,[[var-5],[var-1],[var-9],[var-1],[var-5],[var-5]]),
             soft_alldifferent_var(1,[[var-5],[var-1],[var-9],[var-6],[var-5],[var-3]]),
	     soft_alldifferent_var(0,[[var-8],[var-1],[var-9],[var-6],[var-5],[var-3]])]).

ctr_draw_example(soft_alldifferent_var,
                 ['VARIABLES'],
                 [[[var-5],[var-1],[var-9],[var-1],[var-5],[var-5]]],
                 ['CLIQUE'],
                 [1-[1,5,6],
                  2-[2,4],
                  3-3,
                  4-[2,4],
                  5-[1,5,6],
                  6-[1,5,6]],
                 ['NSCC'([[1,5,6],[2,4],[3]])],
                 '','NSCC=3',
                 [2.145,2.4,2.3,2.43]).

ctr_see_also(soft_alldifferent_var,
 [link('implied by',     all_min_dist,             '',   []),
  link('implied by',     alldifferent_modulo,      '',   []),
  link('implied by',     soft_alldifferent_ctr,    '',   []),
  link('hard version',   alldifferent,             '',   []),
  link('common keyword', soft_all_equal_min_ctr,   '%k', ['soft constraint']),
  link('common keyword', soft_all_equal_max_var,   '%k', ['soft constraint']),
  link('common keyword', soft_all_equal_min_var,   '%k', ['soft constraint']),
  link('common keyword', soft_alldifferent_ctr,    '%k', ['soft constraint']),
  link('common keyword', weighted_partial_alldiff, '%k', ['soft constraint']),
  link('related',        atmost_nvalue,            '',   []),
  link('related',        nvalue,                   '',   [])]).

ctr_key_words(soft_alldifferent_var,['soft constraint'                 ,
                                     'value constraint'                ,
                                     'relaxation'                      ,
                                     'variable-based violation measure',
                                     'all different'                   ,
				     'bipartite matching'              ,
                                     'disequality'                     ,
                                     'strongly connected component'    ,
                                     'equivalence'                     ]).

ctr_persons(soft_alldifferent_var,['Petit T.'       ,
                                   'R\\\'egin J.-C.',
                                   'Bessi\\`ere C.' ,
                                   'Hebrard E.'     ,
                                   'O\'Sullivan B.' ,
                                   'Razgon I.'      ,
                                   'Marx D.'        ,
				   'Cymer R.'       ]).

ctr_eval(soft_alldifferent_var, [checker(soft_alldifferent_var_c)      ,
				 reformulation(soft_alldifferent_var_r)]).

ctr_sol(soft_alldifferent_var,2,0,2,24,[0-6,1-9,2-9]).
ctr_sol(soft_alldifferent_var,3,0,3,212,[0-24,1-60,2-64,3-64]).
ctr_sol(soft_alldifferent_var,4,0,4,2470,[0-120,1-480,2-620,3-625,4-625]).
ctr_sol(soft_alldifferent_var,5,0,5,35682,[0-720,1-4320,2-7320,3-7770,4-7776,5-7776]).
ctr_sol(soft_alldifferent_var,6,0,6,614600,[0-5040,1-42840,2-97440,3-116340,4-117642,5-117649,6-117649]).
ctr_sol(soft_alldifferent_var,7,0,7,12286024,[0-40320,1-463680,2-1404480,3-1992480,4-2093616,5-2097144,6-2097152,7-2097152]).
ctr_sol(soft_alldifferent_var,8,0,8,279472266,[0-362880,1-5443200,2-21530880,3-37406880,4-42550704,5-43037568,6-43046712,7-43046721,8-43046721]).

soft_alldifferent_var_r(C, []) :- !,
    check_type(dvar_gteq(0), C).
soft_alldifferent_var_r(C, VARIABLES) :-
    check_type(dvar_gteq(0), C),
    collection(VARIABLES,[dvar]),
    length(VARIABLES, N),
    eval(in_interval(M, 1, N)),
    eval(nvalue(M, VARIABLES)),
    C #>= N - M.

soft_alldifferent_var_c(C, []) :- !,
    check_type(dvar_gteq(0), C).
soft_alldifferent_var_c(C, VARIABLES) :-
    check_type(dvar_gteq(0), C),
    collection(VARIABLES,[int]),
    length(VARIABLES, N),
    get_attr1(VARIABLES, VARS),
    sort(VARS, SVARS),
    length(SVARS, M),
    NM is N - M,
    C #>= NM.
