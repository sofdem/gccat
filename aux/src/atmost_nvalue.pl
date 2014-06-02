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

ctr_date(atmost_nvalue,['20050618','20060804','20090926']).

ctr_origin(atmost_nvalue, '\\cite{BessiereHebrardHnichKiziltanWalsh05}', []).

ctr_arguments(atmost_nvalue,
              ['NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(atmost_nvalue,
                 [vals(['NVAL'],int,<,dontcare,dontcare),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare),
                  vals(['VARIABLES'^var],int,=\=,dontcare,in)]).

ctr_synonyms(atmost_nvalue,[soft_alldiff_max_var     ,
                            soft_alldifferent_max_var,
                            soft_alldistinct_max_var ]).

ctr_restrictions(atmost_nvalue,
                 ['NVAL' >= min(1,size('VARIABLES')),
                  required('VARIABLES',var)         ]).

ctr_typical(atmost_nvalue,
            ['NVAL' > 1                ,
             'NVAL' < size('VARIABLES'),
             size('VARIABLES') > 1     ]).

ctr_contractible(atmost_nvalue, [], 'VARIABLES', any).

ctr_total_relation(atmost_nvalue).

ctr_graph(atmost_nvalue,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSCC' =< 'NVAL'],
          ['EQUIVALENCE']).

ctr_example(atmost_nvalue,
            [atmost_nvalue(4,[[var-3],[var-1],[var-3],[var-1],[var-6]]),
	     atmost_nvalue(3,[[var-3],[var-1],[var-3],[var-1],[var-6]]),
	     atmost_nvalue(1,[[var-3],[var-3],[var-3],[var-3],[var-3]])]).

ctr_draw_example(atmost_nvalue,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-3],[var-1],[var-6]]],
                 ['CLIQUE'],
                 [1-[1,3],
                  2-[2,4],
                  3-[1,3],
                  4-[2,4],
                  5-5],
                 ['NSCC'([[1,3],[2,4],[5]])],
                 '','NSCC=3',
                 [2.145,2.3,2.8,2.33]).

ctr_see_also(atmost_nvalue,
 [link('implied by',         nvalue,                 '$\\leq$ %e replaced by $=$ %e', ['NVAL','NVAL']),
  link('comparison swapped', atleast_nvalue,         '',                              []),
  link('related',            soft_all_equal_max_var, '',                              []),
  link('related',            soft_all_equal_min_ctr, '',                              []),
  link('related',            soft_all_equal_min_var, '',                              []),
  link('related',            soft_alldifferent_ctr,  '',                              []),
  link('related',            soft_alldifferent_var,  '',                              [])]).

ctr_key_words(atmost_nvalue,['counting constraint'                   ,
                             'value partitioning constraint'         ,
                             'number of distinct equivalence classes',
                             'number of distinct values'             ,
                             '3-SAT'                                 ,
                             'strongly connected component'          ,
                             'equivalence'                           ,
                             'bound-consistency'                     ]).

ctr_persons(atmost_nvalue,['Bessi\\`ere C.'     ,
                           'Hebrard E.'         ,
                           'Hnich B.'           ,
                           'K{\\i}z{\\i}ltan Z.',
                           'Walsh T.'           ,
                           'Beldiceanu N.'      ]).

ctr_eval(atmost_nvalue, [reformulation(atmost_nvalue_r),
			 checker(atmost_nvalue_c)]).

ctr_sol(atmost_nvalue,2,0,2,12,[1-3,2-9]).
ctr_sol(atmost_nvalue,3,0,3,108,[1-4,2-40,3-64]).
ctr_sol(atmost_nvalue,4,0,4,1280,[1-5,2-145,3-505,4-625]).
ctr_sol(atmost_nvalue,5,0,5,18750,[1-6,2-456,3-3456,4-7056,5-7776]).
ctr_sol(atmost_nvalue,6,0,6,326592,[1-7,2-1309,3-20209,4-74809,5-112609,6-117649]).
ctr_sol(atmost_nvalue,7,0,7,6588344,[1-8,2-3536,3-104672,4-692672,5-1633472,6-2056832,7-2097152]).
ctr_sol(atmost_nvalue,8,0,8,150994944,[1-9,2-9153,3-496017,4-5639841,5-21515841,6-37603521,7-42683841,8-43046721]).

atmost_nvalue_r(NVAL, VARIABLES) :-
	check_type(dvar, NVAL),
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	length(VARIABLES, N),
    NVAL #=< N,
    V in 0..N,
    V #=< NVAL,
    nvalue(V, VARS).

atmost_nvalue_c(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    (integer(NVAL) ->
	MIN is min(1,N),
	NVAL >= MIN,
        sort(VARS, SVARS),
        length(SVARS, M),
        M =< NVAL
    ;
	NVAL #>= min(1,N),
        sort(VARS, SVARS),
        length(SVARS, M),
        M #=< NVAL
    ).
