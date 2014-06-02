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

ctr_date(atleast_nvalue,['20050618','20060804']).

ctr_origin(atleast_nvalue, '\\cite{Regin95}', []).

ctr_arguments(atleast_nvalue,
              ['NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(atleast_nvalue,
                 [vals(['NVAL'],int(>=(0)),>,dontcare,dontcare),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(atleast_nvalue,[k_diff]).

ctr_restrictions(atleast_nvalue,
                 [required('VARIABLES',var)       ,
                  'NVAL' >= 0                     ,
                  'NVAL' =< size('VARIABLES')     ,
                  'NVAL' =< range('VARIABLES'^var)]).

ctr_typical(atleast_nvalue,
            ['NVAL' > 0                     ,
             'NVAL' < size('VARIABLES')     ,
             'NVAL' < range('VARIABLES'^var),
             size('VARIABLES') > 1          ]).

ctr_extensible(atleast_nvalue, [], 'VARIABLES', any).

ctr_total_relation(atleast_nvalue).

ctr_graph(atleast_nvalue,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSCC' >= 'NVAL'],
          ['EQUIVALENCE']).

ctr_example(atleast_nvalue,
            [atleast_nvalue(2,[[var-3],[var-1],[var-7],[var-1],[var-6]]),
	     atleast_nvalue(4,[[var-3],[var-1],[var-7],[var-1],[var-6]]),
	     atleast_nvalue(5,[[var-3],[var-1],[var-7],[var-0],[var-6]])]).

ctr_draw_example(atleast_nvalue,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-7],[var-1],[var-6]]],
                 ['CLIQUE'],
                 [1-1,
                  2-[2,4],
                  3-3,
                  4-[2,4],
                  5-5],
                 ['NSCC'([[1],[2,4],[3],[5]])],
                 '','NSCC=4',
                 [2.145,2.3,2.8,2.33]).

ctr_see_also(atleast_nvalue,
 [link('implied by',                and,                                '',                              []),
  link('implied by',                equivalent,                         '',                              []),
  link('implied by',                imply,                              '',                              []),
  link('implied by',                nand,                               '',                              []),
  link('implied by',                nor,                                '',                              []),
  link('implied by',                or,                                 '',                              []),
  link('implied by',                nvisible_from_end,                  '',                              []),
  link('implied by',                nvisible_from_start,                '',                              []),
  link('implied by',                size_max_seq_alldifferent,          '',                              []),
  link('implied by',                size_max_starting_seq_alldifferent, '',                              []),
  link('implied by',                nvalue,                             '$\\geq$ %e replaced by $=$ %e', ['NVAL','NVAL']),
  link('implied by',                xor,                                '',                              []),
  link('uses in its reformulation', not_all_equal,                      '',                              []),
  link('comparison swapped',        atmost_nvalue,                      '',                              [])]).

ctr_key_words(atleast_nvalue,['counting constraint'                   ,
                              'value partitioning constraint'         ,
                              'number of distinct equivalence classes',
                              'number of distinct values'             ,
                              'strongly connected component'          ,
                              'equivalence'                           ,
                              'bipartite matching'                    ,
                              'arc-consistency'                       ]).

ctr_persons(atleast_nvalue,['R\\\'egin J.-C.'    ,
                            'Bessi\\`ere C.'     ,
                            'Hebrard E.'         ,
                            'Hnich B.'           ,
                            'K{\\i}z{\\i}ltan Z.',
                            'Walsh T.'           ]).

ctr_eval(atleast_nvalue, [      checker(atleast_nvalue_c),
			  reformulation(atleast_nvalue_r)]).

ctr_sol(atleast_nvalue,2,0,2,24,[0-9,1-9,2-6]).
ctr_sol(atleast_nvalue,3,0,3,212,[0-64,1-64,2-60,3-24]).
ctr_sol(atleast_nvalue,4,0,4,2470,[0-625,1-625,2-620,3-480,4-120]).
ctr_sol(atleast_nvalue,5,0,5,35682,[0-7776,1-7776,2-7770,3-7320,4-4320,5-720]).
ctr_sol(atleast_nvalue,6,0,6,614600,[0-117649,1-117649,2-117642,3-116340,4-97440,5-42840,6-5040]).
ctr_sol(atleast_nvalue,7,0,7,12286024,[0-2097152,1-2097152,2-2097144,3-2093616,4-1992480,5-1404480,6-463680,7-40320]).
ctr_sol(atleast_nvalue,8,0,8,279472266,[0-43046721,1-43046721,2-43046712,3-43037568,4-42550704,5-37406880,6-21530880,7-5443200,8-362880]).

atleast_nvalue_r(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    NVAL #>= 0,
    NVAL #=< N,
    list_dvar_range(VARS, R),
    NVAL #=< R,
    V in 0..N,
    V #>= NVAL,
    nvalue(V, VARS).

atleast_nvalue_c(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARS, N),
    (integer(NVAL) ->
	NVAL >= 0,
	NVAL =< N,
        sort(VARS, SVARS),
        length(SVARS, M),
        M >= NVAL
    ;
	NVAL #>= 0,
	NVAL #=< N,
        sort(VARS, SVARS),
        length(SVARS, M),
        M #>= NVAL
    ).
