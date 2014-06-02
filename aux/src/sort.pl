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

ctr_date(sort,['20030820','20060816']).

ctr_origin(sort, '\\cite{OlderSwinkelsEmden95}', []).

ctr_arguments(sort,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(sort,
                 [items('VARIABLES1',all),
                  translate(['VARIABLES1'^var,'VARIABLES2'^var])]).

ctr_synonyms(sort,[sortedness, sorted, sorting]).

ctr_restrictions(sort,
                 [size('VARIABLES1') = size('VARIABLES2'),
                  required('VARIABLES1',var)             ,
                  required('VARIABLES2',var)             ]).

ctr_typical(sort,
            [size('VARIABLES1')      > 1,
             range('VARIABLES1'^var) > 1]).

ctr_pure_functional_dependency(sort, []).
ctr_functional_dependency(sort, 2, [1]).

ctr_graph(sort,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          [for_all('CC','NSOURCE' = 'NSINK'),
           'NSOURCE' = size('VARIABLES1'),
           'NSINK'   = size('VARIABLES2')],
          []).

ctr_graph(sort,
          ['VARIABLES2'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables1^var =< variables2^var],
          ['NARC' = size('VARIABLES2')-1],
          []).

ctr_example(sort,
            sort([[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                 [[var-1],[var-1],[var-1],[var-2],[var-5],[var-9]])).

ctr_draw_example(sort,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                  [[var-1],[var-1],[var-1],[var-2],[var-5],[var-9]]],
                 ['PRODUCT'],
                 [1-[1,2,3],
                  2-6,
                  3-[1,2,3],
                  4-5,
                  5-4,
                  6-[1,2,3]],
                 ['NSOURCE'([1,2,3,4,5,6]),
                  'NSINK'([7,8,9,10,11,12]),
                  'NCC'([[1,3,6,7,8,9],[2,12],[4,11],[5,10]])],
                 '','CC#1:NSOURCE=3,NSINK=3\\nCC#2:NSOURCE=1,NSINK=1\\nCC#3:NSOURCE=1,NSINK=1\\nCC#4:NSOURCE=1,NSINK=1\\nNSOURCE=6,NSINK=6',
                 [3,2.3,4,2.27]).

ctr_see_also(sort,
 [link('implies',                   lex_greatereq,    '',                   []),
  link('implies',                   same,             '',                   []),
  link('generalisation',            sort_permutation, '%e parameter added', ['PERMUTATION']),
  link('uses in its reformulation', alldifferent,     '',                   []),
  link('uses in its reformulation', same,             '',                   [])]).

ctr_key_words(sort,['core'                                           ,
		    'constraint between two collections of variables',
                    'sort'                                           ,
                    'permutation'                                    ,
                    'functional dependency'                          ,
		    'pure functional dependency'                     ,
                    'bound-consistency'                              ]).

ctr_persons(sort,['Older W. J.'         ,
                  'Swinkels G. M.'      ,
                  'van Emden M. H.'     ,
                  'Zhou J.'             ,
                  'Bleuzen-Guernalec N.',
                  'Colmerauer A.'       ,
                  'Mehlhorn K.'         ,
                  'Thiel S.'            ]).

ctr_eval(sort, [reformulation(sort_r),
		checker(sort_c)]).

sort_r(VARIABLES1, VARIABLES2) :-
    collection(VARIABLES1,[dvar]),
    collection(VARIABLES2,[dvar]),
    length(VARIABLES1, L1),
    length(VARIABLES2, L2),
    L1 = L2,
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    length(P, L1),
    domain(P, 1, L1),
    sorting(VARS1, P, VARS2),
    when(ground(VARS1), once(labeling([], P))).

sort_c(VARIABLES1, VARIABLES2) :-
    collection(VARIABLES1,[int]),
    collection(VARIABLES2,[int]),
    length(VARIABLES1, L),
    length(VARIABLES2, L),
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    create_pairs(VARS1, PVARS1),
    create_pairs(VARS2, PVARS2),
    keysort(PVARS1, PVARS2).
