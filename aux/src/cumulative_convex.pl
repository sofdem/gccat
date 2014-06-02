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

ctr_date(cumulative_convex,['20050817','20060807']).

ctr_origin(cumulative_convex, 'Derived from %c', [cumulative]).

ctr_types(cumulative_convex, ['POINTS'-collection(var-dvar)]).

ctr_arguments(cumulative_convex,
              ['TASKS'-collection(points-'POINTS',height-dvar),
               'LIMIT'-int                                    ]).

ctr_exchangeable(cumulative_convex,
                 [items('TASKS',all),
                  items('TASKS'^points,all),
                  vals(['TASKS'^height],int(>=(0)),>,dontcare,dontcare),
                  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_restrictions(cumulative_convex,
                 [required('POINTS',var)           ,
                  size('POINTS') > 0               ,
                  required('TASKS',[points,height]),
                  'TASKS'^height >= 0              ,
                  'LIMIT'        >= 0              ]).

ctr_typical(cumulative_convex,
            [size('TASKS')  > 1                  ,
             'TASKS'^height > 0                  ,
             'LIMIT'        < sum('TASKS'^height)]).

ctr_contractible(cumulative_convex, [], 'TASKS', any).

ctr_derived_collections(cumulative_convex,
    [col('INSTANTS'-collection(instant-dvar),
         [item(instant-'TASKS'^points^var)])]).

ctr_graph(cumulative_convex,
          ['TASKS'],
          1,
          ['SELF'>>collection(tasks)],
          [alldifferent(tasks^points)],
          ['NARC' = size('TASKS')],
          []).

ctr_graph(cumulative_convex,
          ['INSTANTS','TASKS'],
          2,
          ['PRODUCT'>>collection(instants,tasks)],
          [between_min_max(instants^instant,tasks^points)],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TASKS'^height)])]],
          [sum_ctr(variables,=<,'LIMIT')]).

ctr_example(cumulative_convex,
            cumulative_convex([[points-[[var-2 ],[var-1 ],[var-5]]                  , height-1],
                               [points-[[var-4 ],[var-5 ],[var-7]]                  , height-2],
                               [points-[[var-14],[var-13],[var-9],[var-11],[var-10]], height-2]],3)).

ctr_draw_example(cumulative_convex,
                 ['INSTANTS','TASKS'],
                 [[[instant-2],[instant-1],[instant-5],
                   [instant-4],[instant-5],[instant-7],
	           [instant-14],[instant-13],[instant-9],[instant-11],[instant-10]],
		  [[points-[[var-2 ],[var-1 ],[var-5]]                  ],
                   [points-[[var-4 ],[var-5 ],[var-7]]                  ],
                   [points-[[var-14],[var-13],[var-9],[var-11],[var-10]]]]],
                 ['PRODUCT'],
                 [1-1,
                  2-1,
                  3-[1,2],
                  4-[1,2],
                  5-[1,2],
                  6-2,
                  7-3,
                  8-3,
                  9-3,
                  10-3,
                  11-3],
                 ['COLLECTIONS'(['INSTANTS'-[1,2,3,4,5,6,7,8,9,10,11],'TASKS'-[12,13,14]])],
                 '','',
                 [4.5,2.2,4.5,2.2]).

ctr_see_also(cumulative_convex,
 [link('common keyword',            cumulative,      '%k', ['resource constraint']),
  link('used in graph description', alldifferent,    '',   []),
  link('used in graph description', between_min_max, '',   []),
  link('used in graph description', sum_ctr,         '',   [])]).

ctr_key_words(cumulative_convex,['scheduling constraint',
                                 'resource constraint'  ,
                                 'temporal constraint'  ,
                                 'convex'               ,
                                 'compulsory part'      ,
                                 'pattern sequencing'   ]).

ctr_persons(cumulative_convex,['Lahrichi A.'  ,
                               'Fink A.'      ,
                               'Voss S.'      ,
                               'Linhares A.'  ,
                               'Yanasse H. H.']).

ctr_application(cumulative_convex, [1]).
