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

ctr_date(orth_link_ori_siz_end,['20030820','20060812']).

ctr_origin(orth_link_ori_siz_end, 'Used by several constraints between orthotopes', []).

ctr_arguments(orth_link_ori_siz_end,
              ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_exchangeable(orth_link_ori_siz_end,
                 [items('ORTHOTOPE',all),
                  translate(['ORTHOTOPE'^ori,'ORTHOTOPE'^end]),
                  translate(['ORTHOTOPE'^siz,'ORTHOTOPE'^end])]).

ctr_restrictions(orth_link_ori_siz_end,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >= 0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ]).

ctr_typical(orth_link_ori_siz_end,
            [size('ORTHOTOPE') > 1,
             'ORTHOTOPE'^siz   > 0]).

ctr_pure_functional_dependency(orth_link_ori_siz_end, []).
ctr_functional_dependency(orth_link_ori_siz_end, 1-1, [1-2,1-3]).
ctr_functional_dependency(orth_link_ori_siz_end, 1-2, [1-1,1-3]).
ctr_functional_dependency(orth_link_ori_siz_end, 1-3, [1-1,1-2]).

ctr_contractible(orth_link_ori_siz_end, [], 'ORTHOTOPE', any).

ctr_graph(orth_link_ori_siz_end,
          ['ORTHOTOPE'],
          1,
          ['SELF'>>collection(orthotope)],
          [orthotope^ori+orthotope^siz = orthotope^end],
          ['NARC' = size('ORTHOTOPE')],
          []).

ctr_example(orth_link_ori_siz_end,
            orth_link_ori_siz_end([[ori-2,siz-2,end-4],
                                   [ori-1,siz-3,end-4]])).

ctr_draw_example(orth_link_ori_siz_end,
                 ['ORTHOTOPE'],
                 [[[ori-2,siz-2,end-4],[ori-1,siz-3,end-4]]],
                 ['SELF'],
                 [1-1,2-2],
                 ['NARC'],
                 '','NARC=2',
                 [1.3,1.3,1.3,1.3]).

ctr_key_words(orth_link_ori_siz_end,['decomposition'             ,
                                     'orthotope'                 ,
				     'functional dependency'     ,
				     'pure functional dependency']).

ctr_eval(orth_link_ori_siz_end, [reformulation(orth_link_ori_siz_end_r)]).

orth_link_ori_siz_end_r(ORTHOTOPE) :-
    collection(ORTHOTOPE, [dvar,dvar_gteq(0),dvar]),
    length(ORTHOTOPE, N),
    N > 0,
    get_attr1(ORTHOTOPE, ORIGINS),
    get_attr2(ORTHOTOPE, SIZES),
    get_attr3(ORTHOTOPE, ENDS),
    gen_varcst(ORIGINS, SIZES, ENDS).
