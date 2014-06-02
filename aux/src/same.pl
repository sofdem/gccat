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

ctr_date(same,['20000128','20030820','20040530','20060813']).

ctr_origin(same, 'N.~Beldiceanu', []).

ctr_arguments(same,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(same,
                 [args([['VARIABLES1','VARIABLES2']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_restrictions(same,
                 [size('VARIABLES1') = size('VARIABLES2'),
                  required('VARIABLES1',var)             ,
                  required('VARIABLES2',var)             ]).

ctr_typical(same,
            [size('VARIABLES1')      > 1,
             range('VARIABLES1'^var) > 1,
             range('VARIABLES2'^var) > 1]).

% same('VARIABLES11', 'VARIABLES12') and
% same('VARIABLES21', 'VARIABLES22') =>
% same(union('VARIABLES11','VARIABLES21'), union('VARIABLES12','VARIABLES22'))
ctr_aggregate(same, [], [union, union]).

ctr_graph(same,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          [for_all('CC','NSOURCE' = 'NSINK'),
           'NSOURCE' = size('VARIABLES1'),
           'NSINK'   = size('VARIABLES2')],
          []).

ctr_example(same,
            same([[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                 [[var-9],[var-1],[var-1],[var-1],[var-2],[var-5]])).

ctr_draw_example(same,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                  [[var-9],[var-1],[var-1],[var-1],[var-2],[var-5]]],
                 ['PRODUCT'],
                 [1-[2,3,4],
                  2-1,
                  3-[2,3,4],
                  4-6,
                  5-5,
                  6-[2,3,4]],
                 ['NSOURCE'([1,2,3,4,5,6]),
                  'NSINK'([7,8,9,10,11,12]),
                  'NCC'([[1,3,6,8,9,10],[2,7],[4,12],[5,11]])],
                  '','CC#1:NSOURCE=3,NSINK=3\\nCC#2:NSOURCE=1,NSINK=1\\nCC#3:NSOURCE=1,NSINK=1\\nCC#4:NSOURCE=1,NSINK=1',
                 [3,2.3,3.8,2.27]).

ctr_see_also(same,
 [link('system of constraints',       k_same,                             '',                              []),
  link('soft variant',                soft_same_var,                      '%k',                            ['variable-based violation measure']),
  link('related to a common problem', colored_matrix,                     'matrix reconstruction problem', []),
  link('generalisation',              correspondence,                     '%e parameter added',            ['PERMUTATION']),
  link('generalisation',              same_interval,                      '%e replaced by %e',             [variable,variable/constant]),
  link('generalisation',              same_modulo,                        '%e replaced by %e',             [variable,variable mod constant]),
  link('generalisation',              same_partition,                     '%e replaced by %e',             [variable,in_list(variable,partition)]),
  link('implied by',                  lex_equal,                          '',                              []),
  link('implied by',                  same_and_global_cardinality,        '',                              []),
  link('implied by',                  same_and_global_cardinality_low_up, '',                              []),
  link('implied by',                  sort,                               '',                              []),
  link('implies',                     used_by,                            '',                              []),
  link('implies',                     same_intersection,                  '',                              []),
  link('used in reformulation',       sort,                               '',                              [])]).

ctr_key_words(same,['constraint between two collections of variables',
                    'channelling constraint'                         ,
                    'permutation'                                    ,
		    'bipartite matching'                             ,
		    'sort based reformulation'                       ,
                    'multiset'                                       ,
                    'equality between multisets'                     ,
                    'flow'                                           ,
                    'arc-consistency'                                ,
                    'bound-consistency'                              ,
                    'automaton'                                      ,
                    'automaton with array of counters'               ,
                    'DFS-bottleneck'                                 ]).

ctr_persons(same,['Older W. J.'        ,
                  'Swinkels G. M.'     ,
                  'van Emden M. H.'    ,
                  'K{\\i}z{\\i}ltan Z.',
                  'Walsh T.'           ,
                  'Beldiceanu N.'      ,
                  'Katriel I.'         ,
                  'Thiel S.'           ,
                  'van Hoeve W.-J.'    ,
		  'Cymer R.'           ]).

ctr_eval(same, [reformulation(same_r),
		checker(same_c)]).

same_r(VARIABLES1, VARIABLES2) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
	N1 = N2,
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
	same1(VARS1, VARS2).

same1(VARS1, VARS2) :-
	length(VARS1, N),
	length(PERMUTATION1, N),
	domain(PERMUTATION1, 1, N),
	length(PERMUTATION2, N),
	domain(PERMUTATION2, 1, N),
	length(SVARS, N),
	get_minimum(VARS1, MIN1),
	get_maximum(VARS1, MAX1),
	domain(SVARS, MIN1, MAX1),
	sorting(VARS1, PERMUTATION1, SVARS),
	sorting(VARS2, PERMUTATION2, SVARS),
	append(VARS1, VARS2, VARS12),
	append(PERMUTATION1, PERMUTATION2, PERMUTATION12),
	when(ground(VARS12), once(labeling([], PERMUTATION12))).

same_c(VARIABLES1, VARIABLES2) :-
	collection(VARIABLES1, [int]),
	collection(VARIABLES2, [int]),
	length(VARIABLES1, N),
	length(VARIABLES2, N),
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
	create_pairs(VARS1, PVARS1),
	create_pairs(VARS2, PVARS2),
	keysort(PVARS1, SORTED),
	keysort(PVARS2, SORTED).
