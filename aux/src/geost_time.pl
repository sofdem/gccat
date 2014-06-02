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

ctr_date(geost_time,['20060919']).

ctr_origin(geost_time, 'Generalisation of %c.', [diffn]).

ctr_types(geost_time,
          ['VARIABLES'-collection(v-dvar),
           'INTEGERS'-collection(v-int)  ,
           'POSITIVES'-collection(v-int) ]).

ctr_arguments(geost_time,
              ['K'-int                             ,        % number of dimensions of the placement space
               'DIMS'-sint                         ,        % dimensions of interest
               'OBJECTS'-collection(oid-int        ,        % identifier of the object
                                    sid-dvar       ,        % potential shapes that can take the object
                                    x-'VARIABLES'  ,        % coordinates of the origin of the object
                                    start-dvar     ,        % temporal start of the object
                                    duration-dvar  ,        % duration of time interval of the object
                                    end-dvar       ),       % temporal end of the object
               'SBOXES'-collection(sid-int         ,        % identifier of the shape corresponding to the sbox
                                   t-'INTEGERS'    ,        % shift offsets from the origin of the object
                                   l-'POSITIVES'   )]).     % sizes of the sbox

ctr_exchangeable(geost_time,
                 [items('OBJECTS',all),
                  items('SBOXES',all),
                  items_sync('OBJECTS'^x,'SBOXES'^t,'SBOXES'^l,all),
                  vals(['SBOXES'^l^v],int(>=(1)),>,dontcare,dontcare),
                  translate(['OBJECTS'^start,'OBJECTS'^end])]).

ctr_restrictions(geost_time,
                 [size('VARIABLES') >= 1                            ,
                  size('INTEGERS')  >= 1                            ,
                  size('POSITIVES') >= 1                            ,
                  required('VARIABLES',v)                           ,
                  size('VARIABLES')  = 'K'                          ,
                  required('INTEGERS',v)                            ,
                  size('INTEGERS')   = 'K'                          ,
                  required('POSITIVES',v)                           ,
                  size('POSITIVES')   = 'K'                         ,
                  'POSITIVES'^v       >  0                          ,
                  'K'                >= 0                           ,
                  'DIMS'             >= 0                           ,
                  'DIMS'             < 'K'                          ,
                  distinct('OBJECTS',oid)                           ,
                  required('OBJECTS',[oid,sid,x])                   ,
                  require_at_least(2,'OBJECTS',[start,duration,end]),
                  'OBJECTS'^oid      >= 1                           ,
                  'OBJECTS'^oid      =< size('OBJECTS')             ,
                  'OBJECTS'^sid      >= 1                           ,
                  'OBJECTS'^sid      =< size('SBOXES')              ,
                  'OBJECTS'^duration >= 0                           ,
                  size('SBOXES')     >= 1                           ,
                  required('SBOXES',[sid,t,l])                      ,
                  'SBOXES'^sid       >= 1                           ,
                  'SBOXES'^sid       =< size('SBOXES')              ,
                  do_not_overlap('SBOXES')                          ]).

ctr_typical(geost_time,
            [size('OBJECTS') > 1]).

ctr_predefined(geost_time).

ctr_example(geost_time,
            geost_time(2,
                       {0,1},
                       [[oid-1,sid-1,x-[[v-1],[v-2]],start-0,duration-1,end-1],
                        [oid-2,sid-5,x-[[v-2],[v-1]],start-0,duration-1,end-1],
                        [oid-3,sid-8,x-[[v-4],[v-1]],start-0,duration-1,end-1]],
                       [[sid-1,t-[[v-0   ],[v-0]],l-[[v-2],[v-1]]],    % the 3 sboxes of shape s1
                        [sid-1,t-[[v-0   ],[v-1]],l-[[v-1],[v-2]]],
                        [sid-1,t-[[v-1   ],[v-2]],l-[[v-3],[v-1]]],                    
                        [sid-2,t-[[v-0   ],[v-0]],l-[[v-3],[v-1]]],    % the 3 sboxes of shape s2
                        [sid-2,t-[[v-0   ],[v-1]],l-[[v-1],[v-3]]],
                        [sid-2,t-[[v-2   ],[v-1]],l-[[v-1],[v-1]]],
                        [sid-3,t-[[v-0   ],[v-0]],l-[[v-2],[v-1]]],    % the 3 sboxes of shape s3
                        [sid-3,t-[[v-1   ],[v-1]],l-[[v-1],[v-2]]],
                        [sid-3,t-[[v-(-2)],[v-2]],l-[[v-3],[v-1]]],
                        [sid-4,t-[[v-0   ],[v-0]],l-[[v-3],[v-1]]],    % the 3 sboxes of shape s4
                        [sid-4,t-[[v-0   ],[v-1]],l-[[v-1],[v-1]]],
                        [sid-4,t-[[v-2   ],[v-1]],l-[[v-1],[v-3]]],
                        [sid-5,t-[[v-0   ],[v-0]],l-[[v-2],[v-1]]],    % the 3 sboxes of shape s5
                        [sid-5,t-[[v-1   ],[v-1]],l-[[v-1],[v-1]]],
                        [sid-5,t-[[v-0   ],[v-2]],l-[[v-2],[v-1]]],
                        [sid-6,t-[[v-0   ],[v-0]],l-[[v-3],[v-1]]],    % the 3 sboxes of shape s6
                        [sid-6,t-[[v-0   ],[v-1]],l-[[v-1],[v-1]]],
                        [sid-6,t-[[v-2   ],[v-1]],l-[[v-1],[v-1]]],
                        [sid-7,t-[[v-0   ],[v-0]],l-[[v-3],[v-2]]],    % the sbox of shape s7
                        [sid-8,t-[[v-0   ],[v-0]],l-[[v-2],[v-3]]]])). % the sbox of shape s8

ctr_see_also(geost_time,
 [link('specialisation', geost,              '%e %e removed', [temporal, dimension]),
  link('common keyword', diffn,              '%k,%k',         ['geometrical constraint', 'non-overlapping'],'\\\\ '),
  link('common keyword', non_overlap_sboxes, '%k,%k',         ['geometrical constraint', 'non-overlapping']),
  link('common keyword', visible,            '%k,%k',         ['geometrical constraint', 'sweep'])]).

ctr_key_words(geost_time,['decomposition'                                      ,
                          'geometrical constraint'                             ,
                          'timetabling constraint'                             ,
                          'predefined constraint'                              ,
                          'non-overlapping'                                    ,
                          'assignment dimension'                               ,
                          'assignment to the same set of values'               ,
                          'assigning and scheduling tasks that run in parallel',
                          'disjunction'                                        ,
                          'sweep'                                              ]).

ctr_persons(geost_time,['Beldiceanu N.',
                        'Carlsson M.'  ,
                        'Poder E.'     ,
                        'Sadek R.'     ,
                        'Truchet C.'   ]).

ctr_application(geost_time, [3]).
