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

ctr_date(orth_on_the_ground,['20030820','20040726','20060812']).

ctr_origin(orth_on_the_ground, 'Used for defining %c.', [place_in_pyramid]).

ctr_arguments(orth_on_the_ground,
              ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar),
               'VERTICAL_DIM'-int                                  ]).

ctr_restrictions(orth_on_the_ground,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >= 0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  'VERTICAL_DIM'  >= 1                         ,
                  'VERTICAL_DIM'  =< size('ORTHOTOPE')         ,
                  orth_link_ori_siz_end('ORTHOTOPE')           ]).

ctr_typical(orth_on_the_ground,
            [size('ORTHOTOPE') > 1,
             'ORTHOTOPE'^siz   > 0]).

ctr_graph(orth_on_the_ground,
          ['ORTHOTOPE'],
          1,
          ['SELF'>>collection(orthotope)],
          [orthotope^key = 'VERTICAL_DIM',
           orthotope^ori = 1             ],
          ['NARC' = 1],
          []).

ctr_example(orth_on_the_ground,
            orth_on_the_ground([[ori-1,siz-2,end-3],[ori-2,siz-3,end-5]], 1)).

ctr_draw_example(orth_on_the_ground,
                 ['ORTHOTOPE'],
                 [[[ori-1,siz-2,end-3],[ori-2,siz-3,end-5]]],
                 ['SELF'],
                 [1-1],
                 ['NARC'],
                 '','NARC=1',
                 [1.3,1.3,0.7,0.7]).

ctr_key_words(orth_on_the_ground,['geometrical constraint',
                                  'orthotope'             ]).