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

ctr_date(next_element,['20030820','20040530','20060812']).

ctr_origin(next_element, 'N.~Beldiceanu', []).

ctr_arguments(next_element,
              ['THRESHOLD'-dvar                         ,
               'INDEX'-dvar                             ,
               'TABLE'-collection(index-int, value-dvar),
               'VAL'-dvar                               ]).

ctr_restrictions(next_element,
                 ['INDEX'       >= 1             ,
                  'INDEX'       =< size('TABLE') ,
                  'THRESHOLD'   < 'INDEX'        ,
                  required('TABLE',[index,value]),
                  size('TABLE') >  0             ,
                  'TABLE'^index >= 1             ,
                  'TABLE'^index =< size('TABLE') ,
                  distinct('TABLE',index)        ]).

ctr_typical(next_element,
            [size('TABLE')         > 1,
             range('TABLE'^ value) > 1]).

ctr_derived_collections(next_element,
                        [col('ITEM'-collection(index-dvar, value-dvar),
                             [item(index-'THRESHOLD', value-'VAL')])]).

ctr_graph(next_element,
          ['ITEM','TABLE'],
          2,
          ['PRODUCT'>>collection(item,table)],
          [item^index < table^index,
           item^value = table^value],
          ['NARC' > 0],
          [],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'TABLE'^index)])]],
          [minimum('INDEX',variables)]).

ctr_example(next_element,
            next_element(2,3,
                         [[index-1, value-1],
                          [index-2, value-8],
                          [index-3, value-9],
                          [index-4, value-5],
                          [index-5, value-9]],9)).

ctr_draw_example(next_element,
                 ['ITEM','TABLE'],
                 [[[index-2, value-9]],
                  [[index-1, value-1],
                   [index-2, value-8],
                   [index-3, value-9],
                   [index-4, value-5],
                   [index-5, value-9]]],
                 ['PRODUCT'],
                 [1-[3,5]],
                 ['NARC'],
                 '','NARC=2',
                 [2.5,2.145,1.2,1.2]).

ctr_see_also(next_element,
 [link('related', minimum_greater_than, 'identify an element in a table',              []),
  link('related', next_greater_element, 'allow to iterate over the values of a table', [])]).

ctr_key_words(next_element,['data constraint'                         ,
                            'minimum'                                 ,
                            'table'                                   ,
                            'automaton'                               ,
                            'automaton without counters'              ,
                            'reified automaton constraint'            ,
                            'centered cyclic(3) constraint network(1)',
                            'derived collection'                      ]).

ctr_persons(next_element,['Beldiceanu N.']).

ctr_eval(next_element, [automaton(next_element_a)]).

%  0: I<=THRESHOLD and I<INDEX and V = VAL
%  1: I<=THRESHOLD and I<INDEX and V=\=VAL
%  2: I<=THRESHOLD and I=INDEX and V = VAL
%  3: I<=THRESHOLD and I=INDEX and V=\=VAL
%  4: I<=THRESHOLD and I>INDEX and V = VAL
%  5: I<=THRESHOLD and I>INDEX and V=\=VAL
%  6: I> THRESHOLD and I<INDEX and V = VAL
%  7: I> THRESHOLD and I<INDEX and V=\=VAL
%  8: I> THRESHOLD and I=INDEX and V = VAL
%  9: I> THRESHOLD and I=INDEX and V=\=VAL
% 10: I> THRESHOLD and I>INDEX and V = VAL
% 11: I> THRESHOLD and I>INDEX and V=\=VAL
next_element_a(FLAG, THRESHOLD, INDEX, TABLE, VAL) :-
    length(TABLE, N),
    N > 0,
    check_type(dvar, THRESHOLD),
    check_type(dvar(1,N), INDEX),
    collection(TABLE, [int(1,N), dvar]),
    check_type(dvar, VAL),
    THRESHOLD #< INDEX,
    get_attr1(TABLE, INDEXES),
    all_different(INDEXES),
    next_element_signature(TABLE, SIGNATURE, THRESHOLD, INDEX, VAL),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s, 0,s),
                           arc(s, 1,s),
                           arc(s, 2,s),
                           arc(s, 3,s),
                           arc(s, 4,s),
                           arc(s, 5,s),
                           arc(s, 7,s),
                           arc(s, 9,s),
                           arc(s,10,s),
                           arc(s,11,s),
                           arc(s, 8,t),
                           arc(t, 0,t),
                           arc(t, 1,t),
                           arc(t, 2,t),
                           arc(t, 3,t),
                           arc(t, 4,t),
                           arc(t, 5,t),
                           arc(t, 7,t),
                           arc(t, 8,t),
                           arc(t, 9,t),
                           arc(t,10,t),
                           arc(t,11,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1,2,3,4,5,6,7,8,9,10,11], AUTOMATON).

next_element_signature([], [], _, _, _).
next_element_signature([[index-I, value-V]|Ts], [S|Ss], THRESHOLD, INDEX, VAL) :-
    S in 0..11,
    I #=< THRESHOLD  #/\  I #< INDEX  #/\  V #=  VAL  #<=>  S #=  0,
    I #=< THRESHOLD  #/\  I #< INDEX  #/\  V #\= VAL  #<=>  S #=  1,
    I #=< THRESHOLD  #/\  I #= INDEX  #/\  V #=  VAL  #<=>  S #=  2,
    I #=< THRESHOLD  #/\  I #= INDEX  #/\  V #\= VAL  #<=>  S #=  3,
    I #=< THRESHOLD  #/\  I #> INDEX  #/\  V #=  VAL  #<=>  S #=  4,
    I #=< THRESHOLD  #/\  I #> INDEX  #/\  V #\= VAL  #<=>  S #=  5,
    I #>  THRESHOLD  #/\  I #< INDEX  #/\  V #=  VAL  #<=>  S #=  6,
    I #>  THRESHOLD  #/\  I #< INDEX  #/\  V #\= VAL  #<=>  S #=  7,
    I #>  THRESHOLD  #/\  I #= INDEX  #/\  V #=  VAL  #<=>  S #=  8,
    I #>  THRESHOLD  #/\  I #= INDEX  #/\  V #\= VAL  #<=>  S #=  9,
    I #>  THRESHOLD  #/\  I #> INDEX  #/\  V #=  VAL  #<=>  S #= 10,
    I #>  THRESHOLD  #/\  I #> INDEX  #/\  V #\= VAL  #<=>  S #= 11,
    next_element_signature(Ts, Ss, THRESHOLD, INDEX, VAL).
