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

ctr_date(cumulative_two_d,['20000128','20030820','20060807']).

ctr_origin(cumulative_two_d, 'Inspired by %c and %c.', [cumulative,diffn]).

ctr_arguments(cumulative_two_d,
              ['RECTANGLES'-collection(start1-dvar, size1-dvar, last1-dvar,
                                       start2-dvar, size2-dvar, last2-dvar,
                                       height-dvar                        ),
               'LIMIT'-int                                                 ]).

ctr_exchangeable(cumulative_two_d,
                 [items('RECTANGLES',all),
                  attrs_sync('RECTANGLES',[[start1,start2],[size1,size2],[last1,last2],[height]]),
                  vals(['RECTANGLES'^height],int(>=(0)),>,dontcare,dontcare),
                  translate(['RECTANGLES'^start1,'RECTANGLES'^last1]),
                  translate(['RECTANGLES'^start2,'RECTANGLES'^last2]),
                  vals(['LIMIT'],int,<,dontcare,dontcare)]).

ctr_restrictions(cumulative_two_d,
                 [require_at_least(2,'RECTANGLES',[start1,size1,last1]),
                  require_at_least(2,'RECTANGLES',[start2,size2,last2]),
                  required('RECTANGLES',height)                        ,
                  'RECTANGLES'^size1  >= 0                             ,
                  'RECTANGLES'^size2  >= 0                             ,
                  'RECTANGLES'^height >= 0                             ,
                  'LIMIT'             >= 0                             ]).

ctr_typical(cumulative_two_d,
            [size('RECTANGLES')  > 1                       ,
             'RECTANGLES'^size1  > 0                       ,
             'RECTANGLES'^size2  > 0                       ,
             'RECTANGLES'^height > 0                       ,
             'LIMIT'             < sum('RECTANGLES'^height)]).

ctr_contractible(cumulative_two_d, [], 'RECTANGLES', any).

ctr_predefined(cumulative_two_d).

% ctr_derived_collections(cumulative_two_d,
%     [col('CORNERS'-collection(size1-dvar, size2-dvar, x-dvar, y-dvar),
%          [item(size1-'RECTANGLES'^size1, size2-'RECTANGLES'^size2, x-'RECTANGLES'^start1, y-'RECTANGLES'^start2),
%           item(size1-'RECTANGLES'^size1, size2-'RECTANGLES'^size2, x-'RECTANGLES'^start1, y-'RECTANGLES'^last2 ),
%           item(size1-'RECTANGLES'^size1, size2-'RECTANGLES'^size2, x-'RECTANGLES'^last1 , y-'RECTANGLES'^start2),
%           item(size1-'RECTANGLES'^size1, size2-'RECTANGLES'^size2, x-'RECTANGLES'^last1 , y-'RECTANGLES'^last2 )])]).

% ctr_graph(cumulative_two_d,
%           ['RECTANGLES'],
%           1,
%           ['SELF'>>collection(rectangles)],
%           [rectangles^start1+rectangles^size1-1 = rectangles^last1,
%            rectangles^start2+rectangles^size2-1 = rectangles^last2],
%           ['NARC' = size('RECTANGLES')],
%           []).

% ctr_graph(cumulative_two_d,
%           ['CORNERS','RECTANGLES'],
%           2,
%           ['PRODUCT'>>collection(corners,rectangles)],
%           [   corners^size1   >          0      ,
%               corners^size2   >          0      ,
%            rectangles^start1 =<    corners^x    ,
%               corners^x      =< rectangles^last1,
%            rectangles^start2 =<    corners^y    ,
%               corners^y      =< rectangles^last2],
%           [],
%           ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
%           ['SUCC'>>[source,
%                     variables-col('VARIABLES'-collection(var-dvar),
%                                   [item(var-'RECTANGLES'^height)])]],
%           [sum_ctr(variables,=<,'LIMIT')]).

ctr_example(cumulative_two_d,
            cumulative_two_d([[start1-1, size1-4, last1-4,
                               start2-3, size2-3, last2-5, height-4],
                              [start1-3, size1-2, last1-4,
                               start2-1, size2-2, last2-2, height-2],
                              [start1-1, size1-2, last1-2,
                               start2-1, size2-2, last2-2, height-3],
                              [start1-4, size1-1, last1-4,
                               start2-1, size2-1, last2-1, height-1]],
                             4)).

% ctr_draw_example(cumulative_two_d,
%                  ['CORNERS','RECTANGLES'],
%                  [[[size1-4, size2-3, x-1, y-3],
%                    [size1-4, size2-3, x-1, y-5],
%                    [size1-4, size2-3, x-4, y-3],
%                    [size1-4, size2-3, x-4, y-5],
%                    [size1-2, size2-2, x-3, y-1],
%                    [size1-2, size2-2, x-3, y-2],
%                    [size1-2, size2-2, x-4, y-1],
%                    [size1-2, size2-2, x-4, y-2],
%                    [size1-2, size2-2, x-1, y-1],
%                    [size1-2, size2-2, x-1, y-2],
%                    [size1-2, size2-2, x-2, y-1],
%                    [size1-2, size2-2, x-2, y-2],
%                    [size1-1, size2-1, x-4, y-1],
%                    [size1-1, size2-1, x-4, y-1],
%                    [size1-1, size2-1, x-4, y-1],
%                    [size1-1, size2-1, x-4, y-1]],
%                   [[start1-1, size1-4, last1-4, start2-3, size2-3, last2-5, height-4],
%                    [start1-3, size1-2, last1-4, start2-1, size2-2, last2-2, height-2],
%                    [start1-1, size1-2, last1-2, start2-1, size2-2, last2-2, height-3],
%                    [start1-4, size1-1, last1-4, start2-1, size2-1, last2-1, height-1]]],
%                  ['PRODUCT'],
%                  [ 1-1    , 2-1    , 3-1    , 4-1,
%                    5-2    , 6-2    , 7-[2,4], 8-2,
%                    9-3    ,10-3    ,11-3    ,12-3,
%                   13-[2,4],14-[2,4],15-[2,4],16-[2,4]],
%                  ['COLLECTIONS'(['CORNERS'-[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],
%                                  'RECTANGLES'-[17,18,19,20]])],
%                  '','',
%                  [4.5,2.2,5,2.2]).

ctr_see_also(cumulative_two_d,
 [link(specialisation, cumulative,  '%e with a %e replaced by %e with same %e',                                                              [rectangle,height,task,height]),
  link(specialisation, bin_packing, '%e of size %e with a %e replaced by %e of %e %e',                                                       [square,1,height,task,duration,1]),
  link(related,        diffn,       '%c is a necessary condition for %c: forget one dimension when the number of dimensions is equal to %e', [cumulative_two_d,diffn,3])]).

ctr_key_words(cumulative_two_d,['predefined constraint' ,
                                'geometrical constraint',
                                'derived collection'    ,
                                'quadtree'              ,
                                'compulsory part'       ]).

ctr_persons(cumulative_two_d,['Lahrichi A.',
                              'Samet H.'   ]).

ctr_application(cumulative_two_d, [1]).
