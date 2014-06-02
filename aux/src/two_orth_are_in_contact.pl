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

ctr_date(two_orth_are_in_contact,['20030820','20040530','20060819']).

ctr_origin(two_orth_are_in_contact, '\\cite{Roach84}, used for defining %c.', [orths_are_connected]).

ctr_types(two_orth_are_in_contact,
          ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_arguments(two_orth_are_in_contact,
              ['ORTHOTOPE1'-'ORTHOTOPE',
               'ORTHOTOPE2'-'ORTHOTOPE']).

ctr_exchangeable(two_orth_are_in_contact,
                 [args([['ORTHOTOPE1','ORTHOTOPE2']]),
                  items_sync('ORTHOTOPE1','ORTHOTOPE2',all)]).

ctr_restrictions(two_orth_are_in_contact,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >  0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  size('ORTHOTOPE1') = size('ORTHOTOPE2')      ,
                  orth_link_ori_siz_end('ORTHOTOPE1')          ,
                  orth_link_ori_siz_end('ORTHOTOPE2')          ]).

ctr_typical(two_orth_are_in_contact,
            [size('ORTHOTOPE') > 1]).

ctr_graph(two_orth_are_in_contact,
          ['ORTHOTOPE1','ORTHOTOPE2'],
          2,
          ['PRODUCT'(=)>>collection(orthotope1,orthotope2)],
          [orthotope1^end > orthotope2^ori,
           orthotope2^end > orthotope1^ori],
          ['NARC' = size('ORTHOTOPE1')-1],
          []).

ctr_graph(two_orth_are_in_contact,
          ['ORTHOTOPE1','ORTHOTOPE2'],
          2,
          ['PRODUCT'(=)>>collection(orthotope1,orthotope2)],
          [max(0,max(orthotope1^ori,orthotope2^ori)-min(orthotope1^end,orthotope2^end)) = 0],
          ['NARC' = size('ORTHOTOPE1')],
          []).

ctr_example(two_orth_are_in_contact,
            two_orth_are_in_contact([[ori-1,siz-3,end-4], [ori-5,siz-2,end-7]],
                                    [[ori-3,siz-2,end-5], [ori-2,siz-3,end-5]])).

ctr_draw_example(two_orth_are_in_contact,
                 ['ORTHOTOPE1','ORTHOTOPE2'],
                 [[[ori-1,siz-3,end-4], [ori-5,siz-2,end-7]],
                  [[ori-3,siz-2,end-5], [ori-2,siz-3,end-5]]],
                 ['PRODUCT'(=)],
                 [ 1-1],
                 ['NARC'],
                 '','NARC=1',
                 [1.4,1.4,1.1,1.1]).

ctr_see_also(two_orth_are_in_contact,
 [link('implies', two_orth_do_not_overlap, '', [])]).

ctr_key_words(two_orth_are_in_contact,['logic'                           ,
				       'geometrical constraint'          ,
                                       'touch'                           ,
                                       'contact'                         ,
                                       'non-overlapping'                 ,
                                       'orthotope'                       ,
                                       'Berge-acyclic constraint network',
                                       'automaton'                       ,
                                       'automaton without counters'      ,
                                       'reified automaton constraint'    ,
                                       'arc-consistency'                 ]).

ctr_persons(two_orth_are_in_contact,['Roach J. A.']).

ctr_eval(two_orth_are_in_contact, [automaton(two_orth_are_in_contact_a)]).

% 0: SIZ1>0 and SIZ2>0 and  END1>ORI2 and END2>ORI1
% 1: SIZ1>0 and SIZ2>0 and (END1=ORI2 or  END2=ORI1)
% 2: otherwise
two_orth_are_in_contact_a(FLAG, ORTHOTOPE1, ORTHOTOPE2) :-
    length(ORTHOTOPE1, D1),
    length(ORTHOTOPE2, D2),
    D1 > 0,
    D2 > 0,
    D1 = D2,
    collection(ORTHOTOPE1, [dvar,dvar_gteq(1),dvar]),
    collection(ORTHOTOPE2, [dvar,dvar_gteq(1),dvar]),
    get_attr1(ORTHOTOPE1, ORIS1),
    get_attr3(ORTHOTOPE1, ENDS1),
    check_lesseq(ORIS1, ENDS1),
    get_attr1(ORTHOTOPE2, ORIS2),
    get_attr3(ORTHOTOPE2, ENDS2),
    check_lesseq(ORIS2, ENDS2),
    eval(orth_link_ori_siz_end(ORTHOTOPE1)),
    eval(orth_link_ori_siz_end(ORTHOTOPE2)),
    two_orth_are_in_contact_signature(ORTHOTOPE1, ORTHOTOPE2, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,0,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

two_orth_are_in_contact_signature([], [], []).
two_orth_are_in_contact_signature([[ori-ORI1,siz-SIZ1,end-END1]|Q1], [[ori-ORI2,siz-SIZ2,end-END2]|Q2], [S|Ss]) :-
    S in 0..2,
    (SIZ1#>0 #/\ SIZ2#>0 #/\  END1#>ORI2 #/\ END2#>ORI1 ) #<=> S#=0,
    (SIZ1#>0 #/\ SIZ2#>0 #/\ (END1#=ORI2 #\/ END2#=ORI1)) #<=> S#=1,
    two_orth_are_in_contact_signature(Q1, Q2, Ss).
