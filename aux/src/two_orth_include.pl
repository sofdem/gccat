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

ctr_date(two_orth_include,['20030820', '20090524']).

ctr_origin(two_orth_include, 'Used for defining %c.', [diffn_include]).

ctr_types(two_orth_include,
          ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_arguments(two_orth_include,
              ['ORTHOTOPE1'-'ORTHOTOPE',
               'ORTHOTOPE2'-'ORTHOTOPE',
               'DIM'-int               ]).

ctr_exchangeable(two_orth_include,
                 [args([['ORTHOTOPE1','ORTHOTOPE2'],['DIM']])]).

ctr_restrictions(two_orth_include,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >= 0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  size('ORTHOTOPE1') = size('ORTHOTOPE2')      ,
                  orth_link_ori_siz_end('ORTHOTOPE1')          ,
                  orth_link_ori_siz_end('ORTHOTOPE2')          ,
                  'DIM' > 0                                    ,
                  'DIM' =< size('ORTHOTOPE1')                  ]).

ctr_typical(two_orth_include,
            [size('ORTHOTOPE') > 1]).

ctr_graph(two_orth_include,
          ['ORTHOTOPE1','ORTHOTOPE2'],
          2,
          ['PRODUCT'(=)>>collection(orthotope1,orthotope2)],
          [(orthotope1^key = 'DIM'          #/\
            orthotope1^ori < orthotope2^end #/\
            orthotope2^ori < orthotope1^end #/\
            orthotope1^siz > 0              #/\
            orthotope2^siz > 0
           )
            #=>
            min(orthotope1^end,orthotope2^end)-max(orthotope1^ori,orthotope2^ori) = min(orthotope1^siz,orthotope2^siz)
           ],
          ['NARC' = 1],
          []).

ctr_example(two_orth_include,
            two_orth_include([[ori-1,siz-3,end-4], [ori-1,siz-1,end-2]],
                             [[ori-1,siz-2,end-3], [ori-2,siz-3,end-5]],
                             1)).

ctr_draw_example(two_orth_include,
                 ['ORTHOTOPE1','ORTHOTOPE2'],
                 [[[ori-1,siz-3,end-4], [ori-1,siz-1,end-2]],
                  [[ori-1,siz-2,end-3], [ori-2,siz-3,end-5]]],
                 ['PRODUCT'(=)],
                 [1-2],
                 ['NARC'],
                 '','NARC=1',
                 [1.4,1.4,1.1,1.1]).

ctr_see_also(two_orth_include,
 [link('implied by', two_orth_column, '',                                  []),
  link('related',    diffn,           'an extension of the %c constraint', [diffn])]).

ctr_key_words(two_orth_include,['logic'                 ,
				'geometrical constraint',
                                'positioning constraint',
                                'orthotope'             ]).

ctr_eval(two_orth_include, [reformulation(two_orth_include_r)]).

two_orth_include_r(ORTHOTOPE1, ORTHOTOPE2, DIM) :-
	collection(ORTHOTOPE1, [dvar,dvar_gteq(0),dvar]),
	collection(ORTHOTOPE2, [dvar,dvar_gteq(0),dvar]),
	length(ORTHOTOPE1, DIM1),
	length(ORTHOTOPE2, DIM2),
	DIM1 = DIM2,
    check_type(int(1,DIM1), DIM),
    get_attr1(ORTHOTOPE1, ORIS1), nth1(DIM, ORIS1, O1), 
    get_attr2(ORTHOTOPE1, SIZS1), nth1(DIM, SIZS1, S1),
    get_attr3(ORTHOTOPE1, ENDS1), nth1(DIM, ENDS1, E1),
    get_attr1(ORTHOTOPE2, ORIS2), nth1(DIM, ORIS2, O2),
    get_attr2(ORTHOTOPE2, SIZS2), nth1(DIM, SIZS2, S2),
    get_attr3(ORTHOTOPE2, ENDS2), nth1(DIM, ENDS2, E2),
    O1 #< E2 #/\ O2 #< E1 #/\ S1 #> 0 #/\ S2 #> 0 #=> min(E1,E2)-max(O1,O2) #= min(S1,S2).
