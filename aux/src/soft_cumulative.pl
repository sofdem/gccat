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

ctr_date(soft_cumulative,['20091121']).

ctr_origin(soft_cumulative, 'Derived from %c', [cumulative]).

ctr_arguments(soft_cumulative,
              ['TASKS'-collection(origin-dvar, duration-dvar, end-dvar,
                                  height-dvar                         ),
               'LIMIT'-int                                             ,
               'INTERMEDIATE_LEVEL'-int                                ,
               'SURFACE_ON_TOP'-dvar                                   ]).

ctr_exchangeable(soft_cumulative,
                 [items('TASKS',all),
                  translate(['TASKS'^origin,'TASKS'^end]),
                  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_restrictions(soft_cumulative,
                 [require_at_least(2,'TASKS',[origin,duration,end]),
                  required('TASKS',height)                         ,
                  'TASKS'^duration     >= 0                        ,
                  'TASKS'^origin       =< 'TASKS'^end              ,
                  'TASKS'^height       >= 0                        ,
                  'LIMIT'              >= 0                        ,
                  'INTERMEDIATE_LEVEL' >= 0                        ,
                  'INTERMEDIATE_LEVEL' =< 'LIMIT'                  ,
                  'SURFACE_ON_TOP'     >= 0                        ]).

ctr_typical(soft_cumulative,
            [size('TASKS')           > 1                  ,
             range('TASKS'^origin)   > 1                  ,
             range('TASKS'^duration) > 1                  ,
             range('TASKS'^end)      > 1                  ,
             range('TASKS'^height)   > 1                  ,
             'TASKS'^duration        > 0                  ,
             'TASKS'^height          > 0                  ,
             'LIMIT'                 < sum('TASKS'^height),
             'INTERMEDIATE_LEVEL'    > 0                  ,
             'INTERMEDIATE_LEVEL'    < 'LIMIT'            ,
             'SURFACE_ON_TOP'        > 0                  ]).

ctr_predefined(soft_cumulative).

ctr_example(soft_cumulative,
            soft_cumulative([[origin-1, duration-4, end-5, height-1],
                             [origin-1, duration-1, end-2, height-2],
                             [origin-3, duration-3, end-6, height-2]], 3, 2, 3)).

ctr_see_also(soft_cumulative,
 [link('hard version', cumulative, '', [])]).

ctr_key_words(soft_cumulative,['predefined constraint',
			       'soft constraint'      ,
                               'scheduling constraint',
                               'resource constraint'  ,
                               'temporal constraint'  ,
                               'relaxation'           ]).

ctr_persons(soft_cumulative,['Beldiceanu N.',
                             'Petit T.'     ,
                             'Poder E.'     ,
			     'De Clercq A.' ]).

ctr_application(soft_cumulative, [1]).
