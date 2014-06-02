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

ctr_date(cumulative,['20000128','20030820','20040530','20060806','20090923']).

ctr_origin(cumulative, '\\cite{AggounBeldiceanu93}', []).

ctr_arguments(cumulative,
              ['TASKS'-collection(origin-dvar, duration-dvar, end-dvar,
                                  height-dvar                         ),
               'LIMIT'-int                                             ]).

ctr_exchangeable(cumulative,
                 [items('TASKS',all),
                  vals(['TASKS'^duration],int(>=(0)),>,dontcare,dontcare),
                  vals(['TASKS'^height],int(>=(0)),>,dontcare,dontcare),
                  translate(['TASKS'^origin,'TASKS'^end]),
                  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_synonyms(cumulative,[cumulative_max]).

ctr_restrictions(cumulative,
                 [require_at_least(2,'TASKS',[origin,duration,end]),
                  required('TASKS',height)                         ,
                  'TASKS'^duration >= 0                            ,
                  'TASKS'^origin   =< 'TASKS'^end                  ,
                  'TASKS'^height   >= 0                            ,
                  'LIMIT'          >= 0                            ]).

ctr_typical(cumulative,
            [size('TASKS')           > 1                  ,
             range('TASKS'^origin)   > 1                  ,
             range('TASKS'^duration) > 1                  ,
             range('TASKS'^end)      > 1                  ,
             range('TASKS'^height)   > 1                  ,
             'TASKS'^duration        > 0                  ,
             'TASKS'^height          > 0                  ,
             'LIMIT'                 < sum('TASKS'^height)]).

ctr_contractible(cumulative, [], 'TASKS', any).

ctr_graph(cumulative,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [tasks^origin+tasks^duration = tasks^end],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(cumulative,
          ['TASKS','TASKS'],
          2,
          ['PRODUCT'>>collection(tasks1,tasks2)],
          [tasks1^duration > 0             ,
           tasks2^origin   =< tasks1^origin,
           tasks1^origin   <  tasks2^end   ],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^height)])]],
          [sum_ctr(variables,=<,'LIMIT')]).

ctr_example(cumulative,
            cumulative([[origin-1, duration-3 , end-4 , height-1],
                        [origin-2, duration-9 , end-11, height-2],
                        [origin-3, duration-10, end-13, height-1],
                        [origin-6, duration-6 , end-12, height-1],
                        [origin-7, duration-2 , end-9 , height-3]],8)).

ctr_draw_example(cumulative,
                 ['TASKS','TASKS'],
                 [[[origin-1, duration-3 , end-4 , height-1],
                   [origin-2, duration-9 , end-11, height-2],
                   [origin-3, duration-10, end-13, height-1],
                   [origin-6, duration-6 , end-12, height-1],
                   [origin-7, duration-2 , end-9 , height-3]]],
                 ['PRODUCT'],
                 [1-1,
                  2-[1,2],
                  3-[1,2,3],
                  4-[2,3,4],
                  5-[2,3,4,5]],
                 ['COLLECTIONS'(['TASKS'-[1,2,3,4,5],'TASKS'-[6,7,8,9,10]])],
                 '','',
                 [2.3,3,4.2,3]).

ctr_cond_imply(cumulative, coloured_cumulative, ['TASKS'^height > 0], [], [same('TASKS'),same('LIMIT')]).

ctr_see_also(cumulative,
 [link('assignment dimension added', cumulatives,                       'negative %e allowed and %k added',                                                                 [heights,'assignment dimension']),
  link('assignment dimension added', coloured_cumulatives,              'sum of %e %e replaced by number of distinct %e, %k added',                                         [task,heights,colours,'assignment dimension']),
  link(generalisation,               cumulative_two_d,                  '%e replaced by %e with a %e',                                                                      [task,rectangle,height]),
  link(specialisation,               atmost,                            '%e replaced by %e',                                                                                [task,variable]),
  link(specialisation,               disjunctive,                       'all %e have a %e of %e',                                                                           [tasks,height,1]),
  link(specialisation,               bin_packing,                       'all %e have a %e of %e and a fixed %e',                                                            [tasks,duration,1,height]),
  link(specialisation,               multi_inter_distance,              'all %e have the same %e equal to %e and the same %e equal to %e',              [tasks,duration,'DIST',height,1]),
  link('soft variant',               soft_cumulative,                   '',                                                                                                 []),
  link('implied by',                 diffn,                             '%c is a neccessary condition for each dimension of the %c constraint',                             [cumulative,diffn]),
  link('related',                    ordered_global_cardinality,        'controlling the shape of the cumulative profile for breaking symmetry',                            []),
  link('related',                    lex_chain_less,                    'lexicographic ordering on the origins of %e, %e, $\\ldots$',                                       [tasks, rectangles]),
  link('related',                    lex_chain_lesseq,                  'lexicographic ordering on the origins of %e, %e, $\\ldots$',                                       [tasks, rectangles]),
  link('common keyword',             calendar,                          '%k',                                                                                               ['scheduling constraint']),
  link('common keyword',             coloured_cumulative,               '%k, sum of %e %e replaced by number of distinct values',                                           ['resource constraint',task,heights]),
  link('common keyword',             coloured_cumulatives,              '%k',                                                                                               ['resource constraint']),
  link('common keyword',             cumulative_convex,                 '%k, %e defined by a set of %e',                                                                    ['resource constraint',task,points]),
  link('common keyword',             cumulative_product,                '%k, sum of %e %e replaced by product of %e %e',                                                    ['resource constraint',task,heights,task,heights]),
  link('common keyword',             cumulative_with_level_of_priority, '%k, a %c constraint for each set of %e having a priority less than or equal to a given threshold', ['resource constraint',cumulative,tasks]),
  link('used in graph description',  sum_ctr,                           '',                                                                                                 [])]).

ctr_key_words(cumulative,['core'                                       ,
                          'scheduling constraint'                      ,
                          'resource constraint'                        ,
                          'temporal constraint'                        ,
                          'linear programming'                         ,
                          'dynamic programming'                        ,
                          'compulsory part'                            ,
                          'producer-consumer'                          ,
                          'squared squares'                            ,
                          'automaton'                                  ,
                          'automaton with array of counters'           ,
                          'sequencing with release times and deadlines',
                          'cumulative longest hole problems'           ,
                          'Phi-tree'                                   ,
                          'zero-duration task'                         ]).

ctr_persons(cumulative,['Aggoun A.'        ,
                        'Baptiste P.'      ,
                        'Beldiceanu N.'    ,
                        'Caseau Y.'        ,
                        'Demassey S.'      ,
                        'Erschler J.'      ,
                        'Hooker J. N.'     ,
                        'Laburthe F.'      ,
                        'Lahrichi A.'      ,
                        'Le Pape C.'       ,
                        'Lock H. C. R.'    ,
                        'Lopez P.'         ,
                        'Yan H.'           ,
                        'Carlsson M.'      ,
                        'Poder E.'         ,
                        'Mercier L.'       ,
                        'Van Hentenryck P.',
                        'Vil\\\'im P.'     ,
                        'Schutt A.'        ,
                        'Wolf A.'          ,
                        'Kameugne R.'      ,
                        'Fotso L. P.'      ,
                        'Scott J.'         ,
                        'Ngo-Kateu Y.'     ]).

ctr_application(cumulative, [1]).

ctr_eval(cumulative, [builtin(cumulative_b)]).

cumulative_b(TASKS, LIMIT) :-
    collection(TASKS, [dvar,dvar_gteq(0),dvar,dvar_gteq(0)]),
    integer(LIMIT),
    LIMIT >= 0,
    get_attr1(TASKS, ORIGINS  ),
    get_attr2(TASKS, DURATIONS),
    get_attr3(TASKS, ENDS     ),
    get_attr4(TASKS, HEIGHTS  ),
    gen_cum_tasks(ORIGINS, DURATIONS, ENDS, HEIGHTS, 1, Tasks),
    cumulative(Tasks, [limit(LIMIT)]).
