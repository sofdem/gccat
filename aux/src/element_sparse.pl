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

ctr_date(element_sparse,['20030820','20040530','20060808']).

ctr_origin(element_sparse, '\\index{CHIP|indexuse}CHIP', []).

ctr_usual_name(element_sparse, element).

ctr_arguments(element_sparse,
              ['ITEM'-collection(index-dvar, value-dvar),
               'TABLE'-collection(index-int, value-int ),
               'DEFAULT'-int]).

ctr_exchangeable(element_sparse,
                 [items('TABLE',all),
                  vals(['ITEM'^value,'TABLE'^value,'DEFAULT'],int,=\=,all,dontcare)]).

ctr_restrictions(element_sparse                  ,
                 [required('ITEM',[index,value]) ,
                  'ITEM'^index  >= 1             ,
                  size('ITEM')  =  1             ,
                  size('TABLE') >  0             ,
                  required('TABLE',[index,value]),
                  'TABLE'^index >= 1             ,
                  distinct('TABLE',index)        ]).

ctr_typical(element_sparse,
            [size('TABLE')        > 1,
             range('TABLE'^value) > 1]).

ctr_derived_collections(element_sparse,
                        [col('DEF'-collection(index-int, value-int),
                             [item(index-0, value-'DEFAULT')]),
                         col('TABLE_DEF'-collection(index-dvar, value-dvar),
                             [item(index-'TABLE'^index, value-'TABLE'^value),
                              item(index-'DEF'^index  , value-'DEF'^value  )])
                        ]).

ctr_graph(element_sparse,
          ['ITEM','TABLE_DEF'],
          2,
          ['PRODUCT'>>collection(item,table_def)],
          [item^value = table_def^value                        ,
           item^index = table_def^index #\/ table_def^index = 0],
          ['NARC' >= 1],
          []).

ctr_example(element_sparse,
            element_sparse([[index-2, value-5]],
                           [[index-1, value-6],
                            [index-2, value-5],
                            [index-4, value-2],
                            [index-8, value-9]],
                           5)).

ctr_draw_example(element_sparse,
                 ['ITEM','TABLE_DEF'],
                 [[[index-2, value-5]],
                  [[index-0, value-5],
                   [index-1, value-6],
                   [index-2, value-5],
                   [index-4, value-2],
                   [index-8, value-9]]],
                 ['PRODUCT'],
                 [1-1,1-3],
                 ['NARC'],
                 '','NARC=2',
                 [2.3,2.145,1.15,1.15]).

ctr_see_also(element_sparse,
 [link('implies',               elements_sparse, '',   []),
  link('system of constraints', elements_sparse, '',   []),
  link('common keyword',        elements_sparse, '%k', ['sparse table']),
  link('common keyword',        elem,            '%k', ['array constraint']),
  link('common keyword',        element,         '%k', ['array constraint'])]).

ctr_key_words(element_sparse,['array constraint'                        ,
                              'data constraint'                         ,
                              'binary constraint'                       ,
                              'table'                                   ,
                              'sparse table'                            ,
                              'sparse functional dependency'            ,
                              'variable indexing'                       ,
                              'automaton'                               ,
                              'automaton without counters'              ,
                              'reified automaton constraint'            ,
                              'centered cyclic(2) constraint network(1)',
                              'derived collection'                      ,
                              'arc-consistency'                         ]).

ctr_eval(element_sparse, [reformulation(element_sparse_r),
                          automaton(element_sparse_a)]).

element_sparse_r(ITEM, TABLE, DEFAULT) :-
    length(ITEM , 1),
    length(TABLE, N),
    N > 0,
    collection(ITEM , [dvar_gteq(1),dvar]),
    collection(TABLE, [ int_gteq(1),dvar]),
    check_type(int, DEFAULT),
    get_attr1(ITEM , [I]),
    get_attr2(ITEM , [V]),
    get_attr1(TABLE, INDEXES),
    get_attr2(TABLE, VALUES ),
    all_different(INDEXES),
    element_sparse1(INDEXES, VALUES, I, V, DEFAULT, Term1, Term2),
    call(Term1 #\/ Term2).

element_sparse1([], [], _, V, DEFAULT, 0, V#=DEFAULT).
element_sparse1([IND|R], [VAL|S], I, V, DEFAULT, (I#=IND #/\ V#=VAL) #\/ T, I#\=IND #/\ U) :-
    element_sparse1(R, S, I, V, DEFAULT, T, U).

% 0: ITEM_INDEX=\=TABLE_INDEX and ITEM_VALUE=\=DEFAULT
% 1: ITEM_INDEX = TABLE_INDEX and ITEM_VALUE = TABLE_VALUE
% 2: ITEM_INDEX=\=TABLE_INDEX and ITEM_VALUE = DEFAULT
element_sparse_a(FLAG, ITEM, TABLE, DEFAULT) :-
    length(ITEM , 1),
    length(TABLE, N),
    N > 0,
    collection(ITEM , [dvar_gteq(1),dvar]),
    collection(TABLE, [ int_gteq(1),dvar]),
    check_type(int, DEFAULT),
    get_attr1(TABLE, INDEXES),
    all_different(INDEXES),
    ITEM = [[index-ITEM_INDEX, value-ITEM_VALUE]],
    element_sparse_signature(TABLE, SIGNATURE, ITEM_INDEX, ITEM_VALUE, DEFAULT),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(d),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(s,2,d),
                           arc(d,1,t),
                           arc(d,2,d),
                           arc(t,0,t),
                           arc(t,1,t),
                           arc(t,2,t)],
                           [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

element_sparse_signature([], [], _, _, _).
element_sparse_signature([[index-TABLE_INDEX,value-TABLE_VALUE]|TABLEs],
                         [S|Ss], ITEM_INDEX, ITEM_VALUE, DEFAULT) :-
    S in 0..2,
    (ITEM_INDEX #\=TABLE_INDEX #/\ ITEM_VALUE #\= DEFAULT    ) #<=> S #= 0,
    (ITEM_INDEX #= TABLE_INDEX #/\ ITEM_VALUE #=  TABLE_VALUE) #<=> S #= 1,
    (ITEM_INDEX #\=TABLE_INDEX #/\ ITEM_VALUE #=  DEFAULT    ) #<=> S #= 2,
    element_sparse_signature(TABLEs, Ss, ITEM_INDEX, ITEM_VALUE, DEFAULT).
