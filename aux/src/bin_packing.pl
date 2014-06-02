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

ctr_date(bin_packing,['20000128','20030820','20040530','20060804']).

ctr_origin(bin_packing, 'Derived from %c.', [cumulative]).

ctr_arguments(bin_packing,
              ['CAPACITY'-int                          ,
               'ITEMS'-collection(bin-dvar, weight-int)]).

ctr_exchangeable(bin_packing,
                 [vals(['CAPACITY'],int,<,dontcare,dontcare),
                  items('ITEMS',all),
                  vals(['ITEMS'^weight],int(>=(0)),>,dontcare,dontcare),
                  vals(['ITEMS'^bin],int,=\=,all,dontcare)]).

ctr_restrictions(bin_packing,
                 ['CAPACITY'     >= 0           ,
                  required('ITEMS',[bin,weight]),
                  'ITEMS'^weight >= 0           ,
                  'ITEMS'^weight =< 'CAPACITY'  ]).

ctr_typical(bin_packing,
            ['CAPACITY'  > maxval('ITEMS'^weight),
             'CAPACITY' =< sum('ITEMS'^weight)   ,
             size('ITEMS')         >  1          ,
             range('ITEMS'^bin)    >  1          ,
             range('ITEMS'^weight) >  1          ,
             'ITEMS'^bin           >= 0          ,
             'ITEMS'^weight        >  0          ]).

ctr_contractible(bin_packing, [], 'ITEMS', any).

ctr_graph(bin_packing,
          ['ITEMS','ITEMS'],
          2,
          ['PRODUCT'>>collection(items1,items2)],
          [items1^bin = items2^bin],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'ITEMS'^weight)])]],
          [sum_ctr(variables,=<,'CAPACITY')]).

ctr_example(bin_packing,
            bin_packing(5,
                        [[bin-3, weight-4],
                         [bin-1, weight-3],
                         [bin-3, weight-1]])).

ctr_draw_example(bin_packing,
                ['ITEMS','ITEMS'],
                [[[bin-3, weight-4],
                  [bin-1, weight-3],
                  [bin-3, weight-1]]],
                ['PRODUCT'],
                [1-[1,3],2-2,3-[1,3]],
                ['COLLECTIONS'(['ITEMS'-[1,2,3],'ITEMS'-[4,5,6]])],
                '','',
                [1.9,1.9,1.9,1.9]).

ctr_cond_imply(bin_packing, atmost_nvector, ['CAPACITY' >= size('ITEMS')], [], [same('CAPACITY'),same('ITEMS')]).

ctr_see_also(bin_packing,
 [link(generalisation,              cumulative,       'task of %e %e replaced by task of given %e',                                        [duration,1,duration]),
  link(generalisation,              cumulative_two_d, '%e of %e %e replaced by %e of size %e with a %e',                                   [task,duration,1,square,1,height]),
  link(generalisation,              bin_packing_capa, 'fixed overall capacity replaced by non-fixed capacity',                             []),
  link(generalisation,              indexed_sum,      'negative contribution also allowed, fixed capacity replaced by a set of variables', []),
  link('used in graph description', sum_ctr,          '',                                                                                  [])]).

ctr_key_words(bin_packing,['resource constraint'                 ,
                           'assignment'                          ,
                           'assignment dimension'                ,
                           'assignment to the same set of values',
                           'automaton'                           ,
                           'automaton with array of counters'    ,
                           'acyclic'                             ,
                           'bipartite'                           ,
                           'no loop'                             ]).

ctr_persons(bin_packing,['Martello S.'           ,
                         'Toth P.'               ,
                         'Aggoun A.'             ,
                         'Beldiceanu N.'         ,
                         'M\\"uller-Hannemann M.',
                         'Stille W.'             ,
                         'Weihe K.'              ,
                         'Shaw P.'               ,
                         'Coffman E. G.'         ,
                         'Garey M. R.'           ,
                         'Johnson D. S.'         ,
                         'Cambazard H.'          ,
                         'O\'Sullivan B.'        ]).

ctr_application(bin_packing, [2]).

ctr_eval(bin_packing, [reformulation(bin_packing_r)]).

bin_packing_r(CAPACITY, ITEMS) :-
    integer(CAPACITY),
    CAPACITY >= 0,
    collection(ITEMS, [dvar,int(0,CAPACITY)]),
    bin_packing1(ITEMS, 1, TASKS),
    cumulative(TASKS, [limit(CAPACITY)]).

