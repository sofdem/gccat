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

ctr_date(same_and_global_cardinality_low_up,['20051104','20060813']).

ctr_origin(same_and_global_cardinality_low_up, 'Derived from %c and %c', [same,global_cardinality_low_up]).

ctr_arguments(same_and_global_cardinality_low_up,
              ['VARIABLES1'-collection(var-dvar)               ,
               'VARIABLES2'-collection(var-dvar)               ,
               'VALUES'-collection(val-int, omin-int, omax-int)]).

ctr_exchangeable(same_and_global_cardinality_low_up,
                 [args([['VARIABLES1','VARIABLES2'],['VALUES']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],all(notin('VALUES'^val)),=,dontcare,dontcare),
                  items('VALUES',all),
                  vals(['VALUES'^omin],int(>=(0)),>,dontcare,dontcare),
                  vals(['VALUES'^omax],int(=<(size('VARIABLES1'))),<,dontcare,dontcare),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(same_and_global_cardinality_low_up,
                 [size('VARIABLES1') = size('VARIABLES2'),
                  required('VARIABLES1',var)             ,
                  required('VARIABLES2',var)             ,
                  required('VALUES',[val,omin,omax])     ,
                  distinct('VALUES',val)                 ,
                  'VALUES'^omin >= 0                     ,
                  'VALUES'^omax =< size('VARIABLES1')    ,
                  'VALUES'^omin =< 'VALUES'^omax         ]).

ctr_typical(same_and_global_cardinality_low_up,
            [size('VARIABLES1')      > 1                 ,
             range('VARIABLES1'^var) > 1                 ,
             range('VARIABLES2'^var) > 1                 ,
             size('VALUES')          > 1                 ,
             'VALUES'^omin          =< size('VARIABLES1'),
             'VALUES'^omax           > 0                 ,
             'VALUES'^omax           < size('VARIABLES1'),
             size('VARIABLES1')      > size('VALUES')    ]).

ctr_contractible(same_and_global_cardinality_low_up, [], 'VALUES', any).

ctr_graph(same_and_global_cardinality_low_up,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          [for_all('CC','NSOURCE' = 'NSINK'),
           'NSOURCE' = size('VARIABLES1'),
           'NSINK'   = size('VARIABLES2')],
          []).

ctr_graph(same_and_global_cardinality_low_up,
          ['VARIABLES1'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var = 'VALUES'^val],
          ['NVERTEX' >= 'VALUES'^omin,
           'NVERTEX' =< 'VALUES'^omax],
          []).

ctr_example(same_and_global_cardinality_low_up,
            same_and_global_cardinality_low_up([[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                                               [[var-9],[var-1],[var-1],[var-1],[var-2],[var-5]],
                                               [[val-1, omin-2, omax-3],
                                                [val-2, omin-1, omax-1],
                                                [val-5, omin-1, omax-1],
                                                [val-7, omin-0, omax-2],
                                                [val-9, omin-1, omax-1]])).

ctr_draw_example(same_and_global_cardinality_low_up,
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

ctr_see_also(same_and_global_cardinality_low_up,
 [link('generalisation', same_and_global_cardinality,       '%e %e replaced by %e', [fixed,interval,variable]),
  link('implies',        same,                              '',                     []),
  link('implies',        global_cardinality_low_up,         '',                     []),
  link('implies',        global_cardinality_low_up_no_loop, '',                     [])]).

ctr_key_words(same_and_global_cardinality_low_up,['constraint between two collections of variables',
                                                  'value constraint'                               ,
                                                  'permutation'                                    ,
                                                  'multiset'                                       ,
                                                  'equality between multisets'                     ,
                                                  'assignment'                                     ,
                                                  'demand profile'                                 ,
                                                  'bound-consistency'                              ,
                                                  'arc-consistency'                                ,
                                                  'flow'                                           ]).

ctr_persons(same_and_global_cardinality_low_up,['Beldiceanu N.',
                                                'Katriel I.'   ,
                                                'Thiel S.'     ]).

ctr_eval(same_and_global_cardinality_low_up, [reformulation(same_and_global_cardinality_low_up_r)]).

same_and_global_cardinality_low_up_r(VARIABLES1, VARIABLES2, VALUES) :-
    eval(same(VARIABLES1, VARIABLES2)),
    eval(global_cardinality_low_up(VARIABLES1, VALUES)).
