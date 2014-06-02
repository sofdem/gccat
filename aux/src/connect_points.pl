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

ctr_date(connect_points,['20000128','20030820','20040530','20060806']).

ctr_origin(connect_points, 'N.~Beldiceanu', []).

ctr_arguments(connect_points,
              ['SIZE1'-int                ,
               'SIZE2'-int                ,
               'SIZE3'-int                ,
               'NGROUP'-dvar              ,
               'POINTS'-collection(p-dvar)]).

ctr_exchangeable(connect_points,
                 [vals(['POINTS'^p],int(=\=(0)),=\=,all,dontcare)]).

ctr_restrictions(connect_points,
                 ['SIZE1'  >  0                           ,
                  'SIZE2'  >  0                           ,
                  'SIZE3'  >  0                           ,
                  'NGROUP' >= 0                           ,
                  'NGROUP' =< size('POINTS')              ,
                  'SIZE1'*'SIZE2'*'SIZE3' = size('POINTS'),
                  required('POINTS',p)                    ]).

ctr_typical(connect_points,
            ['SIZE1'  > 1             ,
             'SIZE2'  > 1             ,
             'NGROUP' > 0             ,
             'NGROUP' < size('POINTS'),
             size('POINTS') > 3       ]).

ctr_functional_dependency(connect_points, 4, [1,2,3,5]).

ctr_graph(connect_points,
          ['POINTS'],
          2,
          ['GRID'(['SIZE1','SIZE2','SIZE3'])>>collection(points1,points2)],
          [points1^p =\= 0        ,
           points1^p  =  points2^p],
          ['NSCC' = 'NGROUP'],
          ['SYMMETRIC']).

ctr_example(connect_points,
            connect_points(8,4,2,2,
                           [[p-0],[p-0],[p-1],[p-1],[p-0],[p-2],[p-0],[p-0],
                            [p-0],[p-0],[p-0],[p-1],[p-0],[p-2],[p-0],[p-0],
                            [p-0],[p-0],[p-0],[p-1],[p-1],[p-1],[p-1],[p-1],
                            [p-0],[p-2],[p-0],[p-1],[p-0],[p-2],[p-0],[p-0],
                            [p-0],[p-0],[p-0],[p-0],[p-0],[p-0],[p-0],[p-0],
                            [p-0],[p-0],[p-0],[p-0],[p-0],[p-2],[p-0],[p-0],
                            [p-0],[p-2],[p-2],[p-2],[p-2],[p-2],[p-0],[p-0],
                            [p-0],[p-2],[p-0],[p-0],[p-0],[p-2],[p-0],[p-0]])).

ctr_key_words(connect_points,['geometrical constraint'      ,
                              'channel routing'             ,
                              'strongly connected component',
                              'joker value'                 ,
                              'symmetric'                   ,
                              'functional dependency'       ]).

ctr_persons(connect_points,['Beldiceanu N.',
                            'Simonis H.'   ,
                            'Zhou N.-F.'   ]).
