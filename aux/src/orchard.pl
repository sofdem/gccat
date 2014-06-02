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

ctr_date(orchard,['20000128','20030820']).

ctr_origin(orchard, '\\cite{Jackson1821}', []).

ctr_arguments(orchard,
              ['NROW'-dvar,
               'TREES'-collection(index-int, x-dvar, y-dvar)]).

ctr_exchangeable(orchard,
                 [items('TREES',all),
                  attrs_sync('TREES',[[index],[x,y]]),
                  translate(['TREES'^x]),
                  translate(['TREES'^y])]).

ctr_restrictions(orchard,
                 ['NROW'        >= 0            ,
                  'TREES'^index >= 1            ,
                  'TREES'^index =< size('TREES'),
                  required('TREES',[index,x,y]) ,
                  distinct('TREES',index)       ,
                  'TREES'^x     >= 0            ,
                  'TREES'^y     >= 0            ]).

ctr_typical(orchard,
            ['NROW'        > 0,
             size('TREES') > 3]).

ctr_pure_functional_dependency(orchard, []).
ctr_functional_dependency(orchard, 1, [2]).

ctr_graph(orchard,
          ['TREES'],
          3,
          ['CLIQUE'(<)>>collection(trees1,trees2,trees3)],
          [(trees1^x*trees2^y - trees1^x*trees3^y) +
           (trees1^y*trees3^x - trees1^y*trees2^x) +
           (trees2^x*trees3^y - trees2^y*trees3^x) = 0],
          ['NARC' = 'NROW'],
          []).

ctr_example(orchard,
            orchard(10,
                    [[index-1, x-0, y-0],
                     [index-2, x-4, y-0],
                     [index-3, x-8, y-0],
                     [index-4, x-2, y-4],
                     [index-5, x-4, y-4],
                     [index-6, x-6, y-4],
                     [index-7, x-0, y-8],
                     [index-8, x-4, y-8],
                     [index-9, x-8, y-8]])).

ctr_key_words(orchard,['geometrical constraint'    ,
                       'alignment'                 ,
                       'hypergraph'                ,
                       'functional dependency'     ,
		       'pure functional dependency']).

ctr_persons(orchard,['Jackson J.']).

ctr_application(orchard, [2]).
