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

ctr_date(orths_are_connected,['20000128','20030820','20060812']).

ctr_origin(orths_are_connected, 'N.~Beldiceanu', []).

ctr_types(orths_are_connected,
          ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_arguments(orths_are_connected,
              ['ORTHOTOPES'-collection(orth-'ORTHOTOPE')]).

ctr_exchangeable(orths_are_connected,
                 [items('ORTHOTOPES',all),
                  items_sync('ORTHOTOPES'^orth,all),
                  translate(['ORTHOTOPES'^orth^ori,'ORTHOTOPES'^orth^end])]).

ctr_restrictions(orths_are_connected,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >  0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  required('ORTHOTOPES',orth)                  ,
                  same_size('ORTHOTOPES',orth)                 ]).

ctr_typical(orths_are_connected,
            [size('ORTHOTOPE')  > 1,
             size('ORTHOTOPES') > 1]).

ctr_graph(orths_are_connected,
          ['ORTHOTOPES'],
          1,
          ['SELF'>>collection(orthotopes)],
          [orth_link_ori_siz_end(orthotopes^orth)],
          ['NARC' = size('ORTHOTOPES')],
          []).

ctr_graph(orths_are_connected,
          ['ORTHOTOPES'],
          2,
          ['CLIQUE'(=\=)>>collection(orthotopes1,orthotopes2)],
          [two_orth_are_in_contact(orthotopes1^orth,orthotopes2^orth)],
          ['NVERTEX' = size('ORTHOTOPES'),
           'NCC' = 1                     ],
          []).

ctr_example(orths_are_connected,
            orths_are_connected([[orth-[[ori-2, siz-4, end-6], [ori-2, siz-2, end-4]]],
                                 [orth-[[ori-1, siz-2, end-3], [ori-4, siz-3, end-7]]],
                                 [orth-[[ori-6, siz-3, end-9], [ori-1, siz-2, end-3]]],
                                 [orth-[[ori-6, siz-2, end-8], [ori-3, siz-2, end-5]]]])).

ctr_draw_example(orths_are_connected,
                 ['ORTHOTOPES'],
                 [[[orth-[[ori-2, siz-4, end-6 ], [ori-2, siz-2, end-4]]],
                   [orth-[[ori-1, siz-2, end-3 ], [ori-4, siz-3, end-7]]],
                   [orth-[[ori-6, siz-3, end-9], [ori-1, siz-2, end-3]]],
                   [orth-[[ori-6, siz-2, end-8 ], [ori-3, siz-2, end-5]]]]],
                 ['CLIQUE'],
                 [1-[1,2,3,4],
                  2-[1,2],
                  3-[1,3,4],
                  4-[1,3,4]],
                 ['NSCC'([[1,2,3,4]])],
                 '','NVERTEX=4\\nNCC=1',
                 []).

ctr_see_also(orths_are_connected,
 [link('implies',                   diffn,                   '', []),
  link('used in graph description', orth_link_ori_siz_end,   '', []),
  link('used in graph description', two_orth_are_in_contact, '', [])]).

ctr_key_words(orths_are_connected,['geometrical constraint',
                                   'touch'                 ,
                                   'contact'               ,
                                   'non-overlapping'       ,
                                   'orthotope'             ]).

ctr_persons(orths_are_connected,['Beldiceanu N.']).

ctr_application(orths_are_connected, [1]).
