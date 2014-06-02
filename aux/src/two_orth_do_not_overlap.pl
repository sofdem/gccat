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

ctr_date(two_orth_do_not_overlap,['20030820','20040530','20060819']).

ctr_origin(two_orth_do_not_overlap, 'Used for defining %c.', [diffn]).

ctr_types(two_orth_do_not_overlap,
          ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_arguments(two_orth_do_not_overlap,
              ['ORTHOTOPE1'-'ORTHOTOPE',
               'ORTHOTOPE2'-'ORTHOTOPE']).

ctr_exchangeable(two_orth_do_not_overlap,
                 [args([['ORTHOTOPE1','ORTHOTOPE2']]),
                  items_sync('ORTHOTOPE1','ORTHOTOPE2',all),
                  vals(['ORTHOTOPE1'^siz],int(>=(0)),>,dontcare,dontcare),
                  vals(['ORTHOTOPE2'^siz],int(>=(0)),>,dontcare,dontcare)]).

ctr_restrictions(two_orth_do_not_overlap,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >= 0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  size('ORTHOTOPE1') = size('ORTHOTOPE2')      ,
                  orth_link_ori_siz_end('ORTHOTOPE1')          ,
                  orth_link_ori_siz_end('ORTHOTOPE2')          ]).

ctr_typical(two_orth_do_not_overlap,
            [size('ORTHOTOPE') > 1]).

ctr_graph(two_orth_do_not_overlap,
          ['ORTHOTOPE1','ORTHOTOPE2'],
          2,
          ['SYMMETRIC_PRODUCT'(=)>>collection(orthotope1,orthotope2)],
          [orthotope1^end =< orthotope2^ori #\/ orthotope1^siz = 0],
          ['NARC' >= 1],
          ['BIPARTITE', 'NO_LOOP']).

ctr_example(two_orth_do_not_overlap,
            two_orth_do_not_overlap([[ori-2,siz-2,end-4], [ori-1,siz-3,end-4]],
                                    [[ori-4,siz-4,end-8], [ori-3,siz-3,end-6]])).

ctr_draw_example(two_orth_do_not_overlap,
                 ['ORTHOTOPE1','ORTHOTOPE2'],
                 [[[ori-2,siz-2,end-4], [ori-1,siz-3,end-4]],
                  [[ori-4,siz-4,end-8], [ori-3,siz-3,end-6]]],
                 ['SYMMETRIC_PRODUCT'(=)],
                 [ 1-1],
                 ['NARC'],
                 '','NARC=1',
                 [1.4,1.4,1.1,1.1]).

ctr_see_also(two_orth_do_not_overlap,
 [link('implied by', two_orth_are_in_contact, '', [])]).

ctr_key_words(two_orth_do_not_overlap,['logic'                           ,
				       'geometrical constraint'          ,
                                       'non-overlapping'                 ,
                                       'orthotope'                       ,
                                       'Berge-acyclic constraint network',
                                       'automaton'                       ,
                                       'automaton without counters'      ,
                                       'reified automaton constraint'    ,
                                       'arc-consistency'                 ,
                                       'constructive disjunction'        ,
                                       'bipartite'                       ,
                                       'no loop'                         ]).

ctr_eval(two_orth_do_not_overlap, [automaton(two_orth_do_not_overlap_a)]).

% 0: SIZ1=0 or  SIZ2=0 or  END1=<ORI2 or  END2=<ORI1
% 1: SIZ1>0 and SIZ2>0 and END1 >ORI2 and END2 >ORI1
two_orth_do_not_overlap_a(FLAG, ORTHOTOPE1, ORTHOTOPE2) :-
    length(ORTHOTOPE1, D1),
    length(ORTHOTOPE2, D2),
    D1 > 0,
    D2 > 0,
    D1 = D2,
    collection(ORTHOTOPE1, [dvar,dvar_gteq(0),dvar]),
    collection(ORTHOTOPE2, [dvar,dvar_gteq(0),dvar]),
    get_attr1(ORTHOTOPE1, ORIS1),
    get_attr3(ORTHOTOPE1, ENDS1),
    check_lesseq(ORIS1, ENDS1),
    get_attr1(ORTHOTOPE2, ORIS2),
    get_attr3(ORTHOTOPE2, ENDS2),
    check_lesseq(ORIS2, ENDS2),
    eval(orth_link_ori_siz_end(ORTHOTOPE1)),
    eval(orth_link_ori_siz_end(ORTHOTOPE2)),
    two_orth_do_not_overlap_signature(ORTHOTOPE1, ORTHOTOPE2, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,1,s),
                           arc(s,0,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

two_orth_do_not_overlap_signature([], [], []).
two_orth_do_not_overlap_signature([[ori-ORI1,siz-SIZ1,end-END1]|Q1],
                                  [[ori-ORI2,siz-SIZ2,end-END2]|Q2], [S|Ss]) :-
    ((SIZ1#>0) #/\ (SIZ2#>0) #/\ (END1#>ORI2) #/\ (END2#>ORI1)) #<=> S,
    two_orth_do_not_overlap_signature(Q1, Q2, Ss).
