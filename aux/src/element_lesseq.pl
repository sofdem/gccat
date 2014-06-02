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

ctr_date(element_lesseq,['20030820','20040530','20060808']).

ctr_origin(element_lesseq,'\\cite{OttossonThorsteinssonHooker99}', []).

ctr_arguments(element_lesseq,
              ['ITEM'-collection(index-dvar, value-dvar),
               'TABLE'-collection(index-int, value-int )]).

ctr_exchangeable(element_lesseq,
                 [items('TABLE',all),
                  vals(['ITEM'^value,'TABLE'^value],int,=\=,all,dontcare)]).

ctr_restrictions(element_lesseq,
                 [required('ITEM',[index,value]) ,
                  'ITEM'^index  >= 1             ,
                  'ITEM'^index  =< size('TABLE') ,
                  size('ITEM')  = 1              ,
                  size('TABLE') > 0              ,
                  required('TABLE',[index,value]),
                  'TABLE'^index >= 1             ,
                  'TABLE'^index =< size('TABLE') ,
                  distinct('TABLE',index)        ]).

ctr_typical(element_lesseq,
            [size('TABLE')        > 1,
             range('TABLE'^value) > 1]).

ctr_graph(element_lesseq,
          ['ITEM','TABLE'],
          2,
          ['PRODUCT'>>collection(item,table)],
          [item^index = table^index,
           item^value =< table^value],
          ['NARC' = 1],
          []).

ctr_example(element_lesseq,
            element_lesseq([[index-3, value-1]],
                           [[index-1, value-6],
                            [index-2, value-9],
                            [index-3, value-2],
                            [index-4, value-9]])).

ctr_draw_example(element_lesseq,
                 ['ITEM','TABLE'],
                 [[[index-3, value-1]],
                  [[index-1, value-6],
                   [index-2, value-9],
                   [index-3, value-2],
                   [index-4, value-9]]],
                 ['PRODUCT'],
                 [1-3],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,2.145,1.15,1.15]).

ctr_cond_imply(element_lesseq, bin_packing_capa, [minval('ITEM'^value) > 0, 'TABLE'^value > 0], [], [same('TABLE'), same('ITEM')]).

ctr_see_also(element_lesseq,
 [link('implied by',     elem,              '',   []),
  link('common keyword', element,           '%k', ['array constraint']),
  link('common keyword', element_greatereq, '%k', ['array constraint']),
  link('common keyword', element_product,   '%k', ['array constraint'])]).

ctr_key_words(element_lesseq,['array constraint'                        ,
                              'data constraint'                         ,
                              'binary constraint'                       ,
                              'table'                                   ,
                              'linear programming'                      ,
                              'variable subscript'                      ,
                              'variable indexing'                       ,
                              'automaton'                               ,
                              'automaton without counters'              ,
                              'reified automaton constraint'            ,
                              'centered cyclic(2) constraint network(1)',
                              'arc-consistency'                         ]).

ctr_persons(element_lesseq,['Ottosson G.'        ,
                            'Thorsteinsson E. S.',
                            'Hooker J. N.'       ]).

ctr_eval(element_lesseq, [reformulation(element_lesseq_r),
                          automaton(element_lesseq_a)]).

element_lesseq_r(ITEM, TABLE) :-
    length(ITEM , 1),
    length(TABLE, N),
    N > 0,
    collection(ITEM , [dvar(1,N),dvar]),
    collection(TABLE, [ int(1,N),dvar]),
    sort_collection(TABLE, index, SORTED_TABLE),
    get_attr1(SORTED_TABLE, INDEXES),
    increasing_values(INDEXES),
    get_attr2(SORTED_TABLE, VALUES),
    get_attr1(ITEM, [INDEX]),
    get_attr2(ITEM, [VALUE]),
    element(INDEX, VALUES, VAL),
    VALUE #=< VAL.

% 0: ITEM_INDEX=\=TABLE_INDEX or  ITEM_VALUE> TABLE_VALUE
% 1: ITEM_INDEX = TABLE_INDEX and ITEM_VALUE=<TABLE_VALUE
element_lesseq_a(FLAG, ITEM, TABLE) :-
    length(ITEM , 1),
    length(TABLE, N),
    N > 0,
    collection(ITEM , [dvar(1,N),dvar]),
    collection(TABLE, [ int(1,N),dvar]),
    get_attr1(TABLE, INDEXES),
    all_different(INDEXES),
    ITEM = [[index-ITEM_INDEX, value-ITEM_VALUE]],
    element_lesseq_signature(TABLE, SIGNATURE, ITEM_INDEX, ITEM_VALUE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

element_lesseq_signature([], [], _, _).
element_lesseq_signature([[index-TABLE_INDEX,value-TABLE_VALUE]|TABLEs],
                         [S|Ss], ITEM_INDEX, ITEM_VALUE) :-
    ((ITEM_INDEX #= TABLE_INDEX) #/\ (ITEM_VALUE #=< TABLE_VALUE)) #<=> S,
    element_lesseq_signature(TABLEs, Ss, ITEM_INDEX, ITEM_VALUE).
