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

ctr_date(polyomino,['20000128','20030820','20060813']).

ctr_origin(polyomino, 'Inspired by \\cite{Golomb65}.', []).

ctr_arguments(polyomino,
              ['CELLS'-collection(index-int ,
                                  right-dvar, left-dvar,
                                  up-dvar   , down-dvar)]).

ctr_exchangeable(polyomino,
                 [items('CELLS',all),
                  attrs_sync('CELLS',[[index],[right,left],[up],[down]]),
                  attrs_sync('CELLS',[[index],[right],[left],[up,down]]),
                  attrs_sync('CELLS',[[index],[up,left,down,right]])]).

ctr_restrictions(polyomino,
                 ['CELLS'^index >= 1                          ,
                  'CELLS'^index =< size('CELLS')              ,
                  size('CELLS') >= 1                          ,
                  required('CELLS',[index,right,left,up,down]),
                  distinct('CELLS',index)                     ,
                  'CELLS'^right >= 0                          ,
                  'CELLS'^right =< size('CELLS')              ,
                  'CELLS'^left  >= 0                          ,
                  'CELLS'^left  =< size('CELLS')              ,
                  'CELLS'^up    >= 0                          ,
                  'CELLS'^up    =< size('CELLS')              ,
                  'CELLS'^down  >= 0                          ,
                  'CELLS'^down  =< size('CELLS')              ]).

ctr_graph(polyomino,
          ['CELLS'],
          2,
          ['CLIQUE'(=\=)>>collection(cells1,cells2)],
          [(cells1^right = cells2^index  #/\  cells2^left  = cells1^index)  #\/
           (cells1^left  = cells2^index  #/\  cells2^right = cells1^index)  #\/
           (cells1^up    = cells2^index  #/\  cells2^down  = cells1^index)  #\/
           (cells1^down  = cells2^index  #/\  cells2^up    = cells1^index)],
          ['NVERTEX' = size('CELLS'),
           'NCC'     = 1            ],
          []).

ctr_example(polyomino,
            polyomino([[index-1, right-0, left-0, up-2, down-0],
                       [index-2, right-3, left-0, up-0, down-1],
                       [index-3, right-0, left-2, up-4, down-0],
                       [index-4, right-5, left-0, up-0, down-3],
                       [index-5, right-0, left-4, up-0, down-0]])).

ctr_draw_example(polyomino,
                 ['CELLS'],
                 [[[index-1, right-0, left-0, up-2, down-0],
                   [index-2, right-3, left-0, up-0, down-1],
                   [index-3, right-0, left-2, up-4, down-0],
                   [index-4, right-5, left-0, up-0, down-3],
                   [index-5, right-0, left-4, up-0, down-0]]],
                 ['CLIQUE'],
                 [1-2,2-[1,3],3-[2,4],4-[3,5],5-4],
                 ['NCC'([[1,2,3,4,5]])],
                 '','NVERTEX=5\\nNCC=1',
                 [2.4,2.145,2.145,3]).

ctr_key_words(polyomino,['geometrical constraint'      ,
                         'strongly connected component',
                         'pentomino'                   ]).

ctr_persons(polyomino,['Golomb S. W.']).

ctr_application(polyomino, [1]).
