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

ctr_date(stage_element,['20040828','20060816']).

ctr_origin(stage_element, '\\index{Choco|indexuse}Choco, derived from %c.', [element]).

ctr_usual_name(stage_element, stage_elt).

ctr_arguments(stage_element,
              ['ITEM'-collection(index-dvar,value-dvar)    ,
               'TABLE'-collection(low-int,up-int,value-int)]).

ctr_exchangeable(stage_element,
                 [vals(['ITEM'^value,'TABLE'^value],int,=\=,all,dontcare)]).

ctr_synonyms(stage_element,[stage_elem]).

ctr_restrictions(stage_element,
                 [required('ITEM',[index,value])  ,
                  size('ITEM')  = 1               ,
                  size('TABLE') > 0               ,
                  required('TABLE',[low,up,value]),
                  'TABLE'^low =< 'TABLE'^up       ,
                  increasing_seq('TABLE',[low])   ]).

ctr_typical(stage_element,
            [size('TABLE')        > 1         ,
             range('TABLE'^value) > 1         ,
             'TABLE'^low          < 'TABLE'^up]).

ctr_pure_functional_dependency(stage_element, []).
ctr_functional_dependency(stage_element,1-2, [1-1,2]).

ctr_extensible(stage_element, [], 'TABLE', suffix).

ctr_graph(stage_element,
          ['TABLE'],
          2,
          ['PATH'>>collection(table1,table2)],
          [table1^low    =< table1^up ,
           table1^up + 1 =  table2^low,
           table2^low    =< table2^up ],
          ['NARC' = size('TABLE')-1],
          []).

ctr_graph(stage_element,
          ['ITEM','TABLE'],
          2,
          ['PRODUCT'>>collection(item,table)],
          [item^index >= table^low  ,
           item^index =< table^up   ,
           item^value =  table^value],
          ['NARC' = 1],
          []).

ctr_example(stage_element,
            stage_element([[index-5, value-6]],
                          [[low-3 , up-7 , value-6],
                           [low-8 , up-8 , value-8],
                           [low-9 , up-14, value-2],
                           [low-15, up-19, value-9]])).

ctr_draw_example(stage_element,
                 ['ITEM','TABLE'],
                 [[[index-5, value-6]],
                  [[low-3 , up-7 , value-6],
                   [low-8 , up-8 , value-8],
                   [low-9 , up-14, value-2],
                   [low-15, up-19, value-9]]],
                 ['PRODUCT'],
                 [1-1],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,1.8,1,1]).

ctr_see_also(stage_element,
 [link('common keyword', elem,    '%k', ['data constraint']),
  link('common keyword', element, '%k', ['data constraint'])]).

ctr_key_words(stage_element,['data constraint'                         ,
                             'binary constraint'                       ,
                             'table'                                   ,
                             'functional dependency'                   ,
		             'pure functional dependency'              ,
                             'automaton'                               ,
                             'automaton without counters'              ,
                             'reified automaton constraint'            ,
                             'centered cyclic(2) constraint network(1)',
                             'arc-consistency'                         ]).

ctr_eval(stage_element, [automaton(stage_element_a)]).

% 0: TABLE_LOW  > ITEM_INDEX or  ITEM_INDEX  > TABLE_UP or  ITEM_VALUE =\= TABLE_VALUE
% 1: TABLE_LOW =< ITEM_INDEX and ITEM_INDEX =< TABLE_UP and ITEM_VALUE  =  TABLE_VALUE
stage_element_a(FLAG, ITEM, TABLE) :-
    collection(ITEM, [dvar,dvar]),
    collection(TABLE, [int,int,int]),
    length(TABLE, N),
    N > 0,
    get_attr1(TABLE, LOWS),
    get_attr2(TABLE, UPS),
    check_lesseq(LOWS, UPS),
    collection_increasing_seq(TABLE, [1]),
    ITEM = [[index-ITEM_INDEX, value-ITEM_VALUE]],
    stage_element_signature(TABLE, SIGNATURE, ITEM_INDEX, ITEM_VALUE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

stage_element_signature([], [], _, _).
stage_element_signature([[low-TABLE_LOW,up-TABLE_UP,value-TABLE_VALUE]|TABLEs], [S|Ss], ITEM_INDEX, ITEM_VALUE) :-
    ((TABLE_LOW #=< ITEM_INDEX) #/\ (ITEM_INDEX #=< TABLE_UP) #/\ (ITEM_VALUE #= TABLE_VALUE)) #<=> S,
    stage_element_signature(TABLEs, Ss, ITEM_INDEX, ITEM_VALUE).
