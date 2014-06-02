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

ctr_date(inverse_offset,['20091404']).

ctr_origin(inverse_offset, '\\index{Gecode|indexuse}Gecode', []).

ctr_arguments(inverse_offset,
              ['SOFFSET'-int                                      ,
               'POFFSET'-int                                      ,
               'NODES'-collection(index-int, succ-dvar, pred-dvar)]).

ctr_exchangeable(inverse_offset,
                 [items('NODES',all)]).

ctr_synonyms(inverse_offset,[channel]).

ctr_restrictions(inverse_offset,
                 [required('NODES',[index,succ,pred])     ,
                  'NODES'^index >= 1                      ,
                  'NODES'^index =< size('NODES')          ,
                  distinct('NODES',index)                 ,
                  'NODES'^succ  >= 1+'SOFFSET'            ,
                  'NODES'^succ  =< size('NODES')+'SOFFSET',
                  'NODES'^pred  >= 1+'POFFSET'            ,
                  'NODES'^pred  =< size('NODES')+'POFFSET']).

ctr_typical(inverse_offset,
            ['SOFFSET'     >= -1,
             'SOFFSET'     =<  1,
             'POFFSET'     >= -1,
             'POFFSET'     =<  1,
             size('NODES')  >  1]).

ctr_pure_functional_dependency(inverse_offset, []).
ctr_functional_dependency(inverse_offset, 3-2, [1,2,3-1,3-3]).
ctr_functional_dependency(inverse_offset, 3-3, [1,2,3-1,3-2]).

ctr_graph(inverse_offset,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^succ-'SOFFSET' = nodes2^index,
           nodes2^pred-'POFFSET' = nodes1^index],
          ['NARC' = size('NODES')],
          []).

ctr_example(inverse_offset,
            inverse_offset(-1,
                           0,
                           [[index-1, succ-4, pred-3],
                            [index-2, succ-2, pred-5],
                            [index-3, succ-0, pred-2],
                            [index-4, succ-6, pred-8],
                            [index-5, succ-1, pred-1],
                            [index-6, succ-7, pred-7],
                            [index-7, succ-5, pred-4],
                            [index-8, succ-3, pred-6]])).

ctr_draw_example(inverse_offset,
                 ['NODES'],
                 [[[index-1, succ-4, pred-3],
                   [index-2, succ-2, pred-5],
                   [index-3, succ-0, pred-2],
                   [index-4, succ-6, pred-8],
                   [index-5, succ-1, pred-1],
                   [index-6, succ-7, pred-7],
                   [index-7, succ-5, pred-4],
                   [index-8, succ-3, pred-6]]],
                 ['CLIQUE'],
                 [1-5,2-3,3-1,4-7,5-2,6-8,7-6,8-4],
                 ['NARC'],
                 '','NARC=8',
                 [2.145,2.145,1.7,1.7]).

ctr_see_also(inverse_offset,
 [link(specialisation, inverse, 'assume that %e and %e are both equal to %e', ['SOFFSET', 'POFFSET', 0])]).

ctr_key_words(inverse_offset,['graph constraint'          ,
                              'channelling constraint'    ,
                              'dual model'                ,
			      'functional dependency'     ,
			      'pure functional dependency',
                              'heuristics'                ,
                              'arc-consistency'           ]).
