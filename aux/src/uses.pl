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

ctr_date(uses,['20050917','20060820']).

ctr_origin(uses, '\\cite{BessiereHebrardHnichKiziltanWalsh05IJCAI}', []).

ctr_arguments(uses,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(uses,
                 [items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_restrictions(uses,
                 [min(1,size('VARIABLES1')) >= min(1,size('VARIABLES2')),
                  required('VARIABLES1',var)                            ,
                  required('VARIABLES2',var)                            ]).

ctr_typical(uses,
            [size('VARIABLES1')       > 1                 ,
             range('VARIABLES1'^var)  > 1                 ,
             size('VARIABLES2')       > 1                 ,
             range('VARIABLES2'^var)  > 1                 ,
             size('VARIABLES1')      =< size('VARIABLES2')]).

ctr_contractible(uses, [], 'VARIABLES2', any).

ctr_extensible(uses, [], 'VARIABLES1', any).

% uses('VARIABLES11', 'VARIABLES12') and
% uses('VARIABLES21', 'VARIABLES22') =>
% uses(union('VARIABLES11','VARIABLES21'), union('VARIABLES12','VARIABLES22'))
ctr_aggregate(uses , [], [union, union]).

ctr_graph(uses,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NSINK' = size('VARIABLES2')],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(uses,
            uses([[var-3],[var-3],[var-4],[var-6]],
                 [[var-3],[var-4],[var-4],[var-4],[var-4]])).

ctr_draw_example(uses,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-3],[var-3],[var-4],[var-6]],
                  [[var-3],[var-4],[var-4],[var-4],[var-4]]],
                 ['PRODUCT'],
                 [1-1,
                  2-1,
                  3-[2,3,4,5]],
                 ['NSINK'([5,6,7,8,9])],
                 '','NSINK=5',
                 [2.5,2.145,3.1,2]).

ctr_see_also(uses,
 [link('implied by',     used_by, '', []),
  link('generalisation', common,  '', []),
  link('related',        roots,   '', [])]).

ctr_key_words(uses,['constraint between two collections of variables',
                    'inclusion'                                      ,
                    '3-SAT'                                          ,
                    'acyclic'                                        ,
                    'bipartite'                                      ,
                    'no loop'                                        ]).

ctr_persons(uses,['Bessi\\`ere C.'     ,
                  'Hebrard E.'         ,
                  'Hnich B.'           ,
                  'K{\\i}z{\\i}ltan Z.',
                  'Walsh T.'           ]).

ctr_eval(uses, [reformulation(uses_r)]).

uses_r(VARIABLES1, VARIABLES2) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    length(VARIABLES1, L1),
    length(VARIABLES2, L2),
    M1 is min(1,L1),
    M2 is min(1,L2),
    M1 >= M2,
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
    uses1(VARS2, VARS1).

uses1([], _).
uses1([VAR2|R], VARS1) :-
    uses2(VARS1, VAR2, TERM),
    call(TERM),
    uses1(R, VARS1).

uses2([], _, 0).
uses2([VAR1|R], VAR2, VAR2 #= VAR1 #\/ S) :-
    uses2(R, VAR2, S).
