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

ctr_date(nvalue,['20000128','20030820','20040530','20051001','20060812','20091105']).

ctr_origin(nvalue, '\\cite{PachetRoy99}', []).

ctr_arguments(nvalue,
              ['NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(nvalue,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(nvalue,[cardinality_on_attributes_values,values]).

ctr_restrictions(nvalue,
                 [required('VARIABLES',var)         ,
                  'NVAL' >= min(1,size('VARIABLES')),
                  'NVAL' =< size('VARIABLES')       ,
                  'NVAL' =< range('VARIABLES'^var)  ]).

ctr_typical(nvalue,
            ['NVAL'            > 1                ,
             'NVAL'            < size('VARIABLES'),
             size('VARIABLES') > 1                ]).

ctr_pure_functional_dependency(nvalue, []).
ctr_functional_dependency(nvalue, 1, [2]).

ctr_contractible(nvalue, ['NVAL'=1,size('VARIABLES')>0], 'VARIABLES', any).
ctr_contractible(nvalue, ['NVAL'=size('VARIABLES')], 'VARIABLES', any).

ctr_graph(nvalue,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSCC' = 'NVAL'],
          ['EQUIVALENCE']).

ctr_example(nvalue,
            [nvalue(4,[[var-3],[var-1],[var-7],[var-1],[var-6]]),
	     nvalue(1,[[var-6],[var-6],[var-6],[var-6],[var-6]]),
	     nvalue(5,[[var-6],[var-3],[var-0],[var-2],[var-9]])]).

ctr_draw_example(nvalue,
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

ctr_cond_imply(nvalue, increasing_nvalue, [increasing('VARIABLES')], [], id).

ctr_see_also(nvalue,
 [link('specialisation',                                                        alldifferent,
        'enforce a number of distinct values equal to the number of variables', []),
  link('specialisation',                                                        not_all_equal,
       'enforce to have at least two distinct values',                          []),
  link('specialisation',                                                        all_equal,
       'enforce to have one single value',                                      []),
  link('implies',                                                               atleast_nvalue,
       '$=\\argument{NVAL}$ replaced by $\\geq\\argument{NVAL}$',               []),
  link('implies',                                                               atmost_nvalue,
       '$=\\argument{NVAL}$ replaced by $\\leq\\argument{NVAL}$',               []),
  link('implied by',                                                            increasing_nvalue,
       '',                                                                      []),
  link('related',                                                               k_alldifferent,
       'necessary condition for two overlapping %c constraints',                [alldifferent]),
  link('related',                                                               increasing_nvalue_chain,
       '',                                                                      []),
  link('generalisation',                                                        nvalues,
       'replace an equality with the number of distinct values by a comparison with the number of distinct values', []),
  link('generalisation',                                                        npair,
       '%e replaced by %e of %e',                                               [variable,pair,variables]),
  link('generalisation',                                                        nequivalence,
       '%e replaced by %e',                                                     [variable,variable mod constant]),
  link('generalisation',                                                        nclass,
       '%e replaced by %e',                                                     [variable,in_list(variable,partition)]),
  link('generalisation',                                                        ninterval,
       '%e replaced by %e',                                                     [variable,variable/constant]),
  link('generalisation',                                                        nvector,
       'variable replaced by %k',                                               ['vector']),
  link('cost variant',                                                          sum_of_weights_of_distinct_values,
       'introduce a weight for each value and replace number of distinct values by sum of weights associated with distinct values', []),
  link('soft variant',                                                          nvalues_except_0,
       'value %e is ignored',                                                   [0]),
  link('shift of concept',                                                      nvalue_on_intersection,
       '',                                                                      []),
  link('common keyword',                                                        among,
       '%k',                                                                    ['counting constraint']),
  link('common keyword',                                                        among_diff_0,
       '%k',                                                                    ['counting constraint']),
  link('common keyword',                                                        count,
       '%k',                                                                    ['counting constraint']),
  link('common keyword',                                                        global_cardinality,
       '%k',                                                                    ['counting constraint']),
  link('common keyword',                                                        max_nvalue,
       '%k',                                                                    ['counting constraint']),
  link('common keyword',                                                        min_nvalue,
       '%k',                                                                    ['counting constraint']),
  link('common keyword',                                                        nvalues_except_0,
       '%k,%k',                                                                 ['counting constraint','number of distinct values']),
  link('assignment dimension added',                                            assign_and_nvalues,
       '',                                                                      []),
  link('related',                                                               coloured_cumulative,
       'restrict number of distinct colours on each maximum clique of the interval graph associated with the tasks', []),
  link('related',                                                               coloured_cumulatives,
       'restrict number of distinct colours on each maximum clique of the interval graph associated with the tasks assigned to the same machine', []),
  link('related',                                                               soft_alldifferent_var,
       '',                                                                      []),
  link('related',                                                               balance,
       'restriction on how balanced an assignment is',                          []),
  link('uses in its reformulation',                                             cycle,
       '',                                                                      []),
  link('uses in its reformulation',                                             consecutive_values,
       '',                                                                      []),
  link('uses in its reformulation',                                             min_n,
       '',                                                                      [])]).

ctr_key_words(nvalue,['counting constraint'                   ,
                      'value partitioning constraint'         ,
                      'number of distinct equivalence classes',
                      'number of distinct values'             ,
                      'core'                                  ,
                      '3-SAT'                                 ,
                      'minimum hitting set cardinality'       ,
                      'bound-consistency'                     ,
                      'strongly connected component'          ,
                      'domination'                            ,
                      'convex bipartite graph'                ,
                      'dominating queens'                     ,
                      'automaton'                             ,
                      'automaton with array of counters'      ,
                      'equivalence'                           ,
                      'functional dependency'                 ,
		      'pure functional dependency'            ]).

ctr_persons(nvalue,['Pachet F.'          ,
                    'Roy P.'             ,
                    'Beldiceanu N.'      ,
                    'Carlsson M.'        ,
                    'Thiel S.'           ,
                    'R\\\'egin J.-C.'    ,
                    'Bessi\\`ere C.'     ,
                    'Hebrard E.'         ,
                    'Hnich B.'           ,
                    'K{\\i}z{\\i}ltan Z.',
                    'Walsh T.'           ,
                    'Katsirelos G.'      ,
                    'Narodytska N.'      ,
                    'Quimper C.-G.'      ]).

ctr_eval(nvalue, [checker(nvalue_c),
		  builtin(nvalue_b)]).

ctr_sol(nvalue,2,0,2,9,[1-3,2-6]).
ctr_sol(nvalue,3,0,3,64,[1-4,2-36,3-24]).
ctr_sol(nvalue,4,0,4,625,[1-5,2-140,3-360,4-120]).
ctr_sol(nvalue,5,0,5,7776,[1-6,2-450,3-3000,4-3600,5-720]).
ctr_sol(nvalue,6,0,6,117649,[1-7,2-1302,3-18900,4-54600,5-37800,6-5040]).
ctr_sol(nvalue,7,0,7,2097152,[1-8,2-3528,3-101136,4-588000,5-940800,6-423360,7-40320]).
ctr_sol(nvalue,8,0,8,43046721,[1-9,2-9144,3-486864,4-5143824,5-15876000,6-16087680,7-5080320,8-362880]).

nvalue_b(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    NVAL #>= min(1,N),
    NVAL #=< N,
    list_dvar_range(VARS, R),
    NVAL #=< R,
    nvalue(NVAL, VARS).

nvalue_c(NVAL, VARIABLES) :-
    check_type(dvar, NVAL),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    (integer(NVAL) ->
	MIN is min(1,N),
	NVAL >= MIN,
	NVAL =< N
    ;
	NVAL #>= min(1,N),
	NVAL #=< N
    ),
    sort(VARS, SVARS),
    length(SVARS, M),
    NVAL #= M.
