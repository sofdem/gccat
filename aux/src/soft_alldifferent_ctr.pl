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

ctr_date(soft_alldifferent_ctr,['20030820','20060815','20090926']).

ctr_origin(soft_alldifferent_ctr, '\\cite{PetitReginBessiere01}', []).

ctr_arguments(soft_alldifferent_ctr,
              ['C'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(soft_alldifferent_ctr,
                 [vals(['C'],int,<,dontcare,dontcare),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(soft_alldifferent_ctr,[soft_alldiff_ctr         ,
                                    soft_alldistinct_ctr     ,
                                    soft_alldiff_min_ctr     ,
                                    soft_alldifferent_min_ctr,
                                    soft_alldistinct_min_ctr ,
                                    soft_all_equal_max_ctr   ]).

ctr_restrictions(soft_alldifferent_ctr,
    ['C' >= 0                 ,
     required('VARIABLES',var)]).

ctr_typical(soft_alldifferent_ctr,
            ['C'               >  0                                            ,
	     'C'               =< (size('VARIABLES')*(size('VARIABLES')-1)) / 2,
             size('VARIABLES') >  1                                            ]).

ctr_contractible(soft_alldifferent_ctr, [], 'VARIABLES', any).

ctr_total_relation(soft_alldifferent_ctr).

ctr_graph(soft_alldifferent_ctr,
          ['VARIABLES'],
          2,
          ['CLIQUE'(<)>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NARC' =< 'C'],
          []).

ctr_example(soft_alldifferent_ctr,
            [soft_alldifferent_ctr(4,[[var-5],[var-1],[var-9],[var-1],[var-5],[var-5]]),
	     soft_alldifferent_ctr(1,[[var-5],[var-1],[var-9],[var-1],[var-2],[var-6]]),
	     soft_alldifferent_ctr(0,[[var-5],[var-1],[var-9],[var-0],[var-2],[var-6]])]).

ctr_draw_example(soft_alldifferent_ctr,
                 ['VARIABLES'],
                 [[[var-5],[var-1],[var-9],[var-1],[var-5],[var-5]]],
                 ['CLIQUE'(<)],
                 [1-[5,6],
                  2-4,
                  5-6],
                 ['NARC'],
                 '','NARC=4',
                 [2.5,2.145,1.5,1.5]).

ctr_see_also(soft_alldifferent_ctr,
 [link('implies',        soft_alldifferent_var,  '',   []),
  link('implied by',     equivalent,             '',   []),
  link('implied by',     imply,                  '',   []),
  link('hard version',   alldifferent,           '',   []),
  link('common keyword', soft_all_equal_min_ctr, '%k', ['soft constraint']),
  link('common keyword', soft_all_equal_max_var, '%k', ['soft constraint']),
  link('common keyword', soft_all_equal_min_var, '%k', ['soft constraint']),
  link('common keyword', soft_alldifferent_var,  '%k', ['soft constraint']),
  link('related',        atmost_nvalue,          '',   [])]).

ctr_key_words(soft_alldifferent_ctr,['soft constraint'                          ,
                                     'value constraint'                         ,
                                     'relaxation'                               ,
                                     'decomposition-based violation measure'    ,
                                     'all different'                            ,
                                     'disequality'                              ,
                                     'minimum cost flow'                        ,
                                     'degree of diversity of a set of solutions']).

ctr_persons(soft_alldifferent_ctr,['Petit T.'       ,
                                   'R\\\'egin J.-C.',
                                   'Bessi\\`ere C.' ,
                                   'van Hoeve W.-J.',
                                   'Hebrard E.'     ,
                                   'O\'Sullivan B.' ,
                                   'Razgon I.'      ,
                                   'Marx D.'        ]).

ctr_eval(soft_alldifferent_ctr, [checker(soft_alldifferent_ctr_c)      ,
				 reformulation(soft_alldifferent_ctr_r)]).

ctr_sol(soft_alldifferent_ctr,2,0,2,15,[0-6,1-9]).
ctr_sol(soft_alldifferent_ctr,3,0,3,208,[0-24,1-60,2-60,3-64]).
ctr_sol(soft_alldifferent_ctr,4,0,4,3625,[0-120,1-480,2-540,3-620,4-620,5-620,6-625]).
ctr_sol(soft_alldifferent_ctr,5,0,5,72576,[0-720,1-4320,2-6120,3-7320,4-7620,5-7620,6-7770,7-7770,8-7770,9-7770,10-7776]).
ctr_sol(soft_alldifferent_ctr,6,0,6,1630279,[0-5040,1-42840,2-80640,3-100590,4-113190,5-113190,6-116760,7-117390,8-117390,9-117390,10-117642,11-117642,12-117642,13-117642,14-117642,15-117649]).
ctr_sol(soft_alldifferent_ctr,7,0,7,40632320,[0-40320,1-463680,2-1169280,3-1580880,4-1933680,5-1968960,6-2051280,7-2086560,8-2086560,9-2088520,10-2095576,11-2096752,12-2096752,13-2096752,14-2096752,15-2097144,16-2097144,17-2097144,18-2097144,19-2097144,20-2097144,21-2097152]).
ctr_sol(soft_alldifferent_ctr,8,0,8,1114431777,[0-362880,1-5443200,2-18144000,3-27881280,4-36666000,5-39206160,6-41111280,7-42522480,8-42628320,9-42769440,10-42938784,11-43023456,12-43025976,13-43030008,14-43030008,15-43044120,16-43046136,17-43046136,18-43046136,19-43046136,20-43046136,21-43046712,22-43046712,23-43046712,24-43046712,25-43046712,26-43046712,27-43046712,28-43046721]).

soft_alldifferent_ctr_r(C, []) :- !,
    check_type(dvar_gteq(0), C).
soft_alldifferent_ctr_r(C, VARIABLES) :-
    length(VARIABLES, N),
    N2 is (N*N-N) // 2,
    check_type(dvar(0,N2), C),
    collection(VARIABLES,[dvar]),
    get_attr1(VARIABLES, VARS),
    soft_alldifferent_ctr1(VARS, TERM),
    call(C #>= TERM).

soft_alldifferent_ctr1([], 0).
soft_alldifferent_ctr1([V|R], S+T) :-
    soft_alldifferent_ctr2(R, V, S),
    soft_alldifferent_ctr1(R, T).

soft_alldifferent_ctr2([], _, 0).
soft_alldifferent_ctr2([U|R], V, B+T) :-
    B #<=> U #= V,
    soft_alldifferent_ctr2(R, V, T).

soft_alldifferent_ctr_c(C, []) :- !,
    check_type(dvar_gteq(0), C).
soft_alldifferent_ctr_c(C, VARIABLES) :-
    length(VARIABLES, N),
    N2 is (N*N-N) // 2,
    check_type(dvar(0,N2), C),
    collection(VARIABLES,[int]),
    get_attr1(VARIABLES, VARS),
    create_pairs(VARS, KVARS),
    keysort(KVARS, SVARS),
    SVARS = [VAL-_|REST],
    soft_alldifferent_ctr3(REST, VAL, 1, 0, COST),
    C #>= COST.

soft_alldifferent_ctr3([], _, CPT, CUR, RES) :- !,
    (CPT=1 -> RES is CUR ; RES is CUR + (CPT*(CPT-1)) // 2).
soft_alldifferent_ctr3([V-V|R], V, CPT, CUR, RES) :- !,
    CPT1 is CPT+1,
    soft_alldifferent_ctr3(R, V, CPT1, CUR, RES).
soft_alldifferent_ctr3([V-V|R], _, CPT, CUR, RES) :-
    (CPT=1 -> NEXT is CUR ; NEXT is CUR + (CPT*(CPT-1)) // 2),
    soft_alldifferent_ctr3(R, V, 1, NEXT, RES).
