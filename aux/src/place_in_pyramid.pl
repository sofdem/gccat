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

ctr_date(place_in_pyramid,['20000128','20030820','20041230','20060813']).

ctr_origin(place_in_pyramid, 'N.~Beldiceanu', []).

ctr_types(place_in_pyramid,
          ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_arguments(place_in_pyramid,
              ['ORTHOTOPES'-collection(orth-'ORTHOTOPE'),
               'VERTICAL_DIM'-int                       ]).

ctr_exchangeable(place_in_pyramid,
                 [items('ORTHOTOPES',all)]).

ctr_restrictions(place_in_pyramid,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >= 0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  required('ORTHOTOPES',orth)                  ,
                  same_size('ORTHOTOPES',orth)                 ,
                  'VERTICAL_DIM' >= 1                          ,
                  diffn('ORTHOTOPES')                          ]).

ctr_typical(place_in_pyramid,
            [size('ORTHOTOPE')  > 1,
             'ORTHOTOPE'^siz    > 0,
             size('ORTHOTOPES') > 1]).

ctr_graph(place_in_pyramid,
          ['ORTHOTOPES'],
          2,
          ['CLIQUE'>>collection(orthotopes1,orthotopes2)],
          [(orthotopes1^key = orthotopes2^key #/\
            orth_on_the_ground(orthotopes1^orth,'VERTICAL_DIM')) #\/
           (orthotopes1^key =\= orthotopes2^key #/\
            orth_on_top_of_orth(orthotopes1^orth,orthotopes2^orth,'VERTICAL_DIM'))],
          ['NARC' = size('ORTHOTOPES')],
          []).

ctr_example(place_in_pyramid,
            place_in_pyramid([[orth-[[ori-1, siz-3, end-4 ], [ori-1, siz-2, end-3]] ],
                              [orth-[[ori-1, siz-2, end-3 ], [ori-3, siz-3, end-6]] ],
                              [orth-[[ori-5, siz-6, end-11], [ori-1, siz-2, end-3]] ],
                              [orth-[[ori-5, siz-2, end-7 ], [ori-3, siz-2, end-5]] ],
                              [orth-[[ori-8, siz-3, end-11], [ori-3, siz-2, end-5]] ],
                              [orth-[[ori-8, siz-2, end-10], [ori-5, siz-2, end-7]] ]],2)).

ctr_draw_example(place_in_pyramid,
                 ['ORTHOTOPES'],
                 [[[orth-[[ori-1, siz-3, end-4 ], [ori-1, siz-2, end-3]] ],
                   [orth-[[ori-1, siz-2, end-3 ], [ori-3, siz-3, end-6]] ],
                   [orth-[[ori-5, siz-6, end-11], [ori-1, siz-2, end-3]] ],
                   [orth-[[ori-5, siz-2, end-7 ], [ori-3, siz-2, end-5]] ],
                   [orth-[[ori-8, siz-3, end-11], [ori-3, siz-2, end-5]] ],
                   [orth-[[ori-8, siz-2, end-10], [ori-5, siz-2, end-7]] ]]],
                 ['CLIQUE'],
                 [1-1,2-1,3-3,4-3,5-3,6-5],
                 ['NARC'],
                 '','NARC=6',
                 [2.4,2.145,2.145,2.145]).

ctr_see_also(place_in_pyramid,
 [link('used in graph description', orth_on_top_of_orth, '', []),
  link('used in graph description', orth_on_the_ground,  '', [])]).

ctr_key_words(place_in_pyramid,['logic'                 ,
				'geometrical constraint',
                                'non-overlapping'       ,
                                'orthotope'             ]).

ctr_persons(place_in_pyramid,['Beldiceanu N.']).

ctr_application(place_in_pyramid, [1]).
