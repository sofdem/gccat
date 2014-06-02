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

ctr_date(in_same_partition,['20030820','20040530','20060810']).

ctr_origin(in_same_partition, 'Used for defining several entries of this catalog.', []).

ctr_types(in_same_partition,
          ['VALUES'-collection(val-int)]).

ctr_arguments(in_same_partition,
              ['VAR1'-dvar                       ,
               'VAR2'-dvar                       ,
               'PARTITIONS'-collection(p-'VALUES')]).

ctr_exchangeable(in_same_partition,
                 [args([['VAR1','VAR2'],['PARTITIONS']]),
                  items('PARTITIONS',all),
                  items('PARTITIONS'^p,all)]).

ctr_restrictions(in_same_partition,
                 [size('VALUES') >= 1     ,
                  required('VALUES',val)  ,
                  distinct('VALUES',val)  ,
                  required('PARTITIONS',p),
                  size('PARTITIONS') >= 2 ]).

ctr_typical(in_same_partition,
            ['VAR1' =\= 'VAR2']).

ctr_extensible(in_same_partition, [], 'PARTITIONS', any).

ctr_derived_collections(in_same_partition,
                        [col('VARIABLES'-collection(var-dvar),
                             [item(var-'VAR1'),
                              item(var-'VAR2')])]).

ctr_graph(in_same_partition,
          ['VARIABLES','PARTITIONS'],
          2,
          ['PRODUCT'>>collection(variables,partitions)],
          [in(variables^var,partitions^p)],
          ['NSOURCE' = 2,
           'NSINK'   = 1],
          []).

ctr_example(in_same_partition,
            in_same_partition(6,2,
                              [[p-[[val-1], [val-3]]],
                               [p-[[val-4]         ]],
                               [p-[[val-2], [val-6]]]])).

ctr_draw_example(in_same_partition,
                 ['VARIABLES','PARTITIONS'],
                 [[[var-6],[var-2]],
                  [[p-[[val-1], [val-3]]],
                   [p-[[val-4]         ]],
                   [p-[[val-2], [val-6]]]]],
                 ['PRODUCT'],
                 [1-3,2-3],
                 ['NSOURCE'([1,2]),'NSINK'([5])],
                 '','NSOURCE=2,NSINK=1',
                 [1.6,1.6,1.4,1.4]).

ctr_see_also(in_same_partition,
 [link('common keyword',            alldifferent_partition, '%k', [partition]),
  link('common keyword',            in,                     '%k', ['value constraint']),
  link('used in graph description', in,                     '',   [])]).

ctr_key_words(in_same_partition,['value constraint'                        ,
                                 'binary constraint'                       ,
                                 'partition'                               ,
                                 'automaton'                               ,
                                 'automaton without counters'              ,
                                 'reified automaton constraint'            ,
                                 'centered cyclic(2) constraint network(1)',
                                 'derived collection'                      ,
                                 'arc-consistency'                         ]).

ctr_eval(in_same_partition, [reformulation(in_same_partition_r),
                             automaton(in_same_partition_a)]).

in_same_partition_r(VAR1, VAR2, PARTITIONS) :-
    check_type(dvar, VAR1),
    check_type(dvar, VAR2),
    collection(PARTITIONS, [col_len_gteq(1, [int])]),
    length(PARTITIONS, P),
    P > 1,
    collection_distinct(PARTITIONS, 1),
    get_col_attr1(PARTITIONS, 1, PVALS),
    in_same_partition1(PVALS, VAR1, VAR2, TERM),
    call(TERM).

in_same_partition1([], _, _, 0).
in_same_partition1([VALS|R], VAR1, VAR2, (TERM1 #/\ TERM2) #\/ TERM) :-
    build_or_var_in_values(VALS, VAR1, TERM1),
    build_or_var_in_values(VALS, VAR2, TERM2),
    in_same_partition1(R, VAR1, VAR2, TERM).

% 0: not_in(VAR1,VALUES) or not_in(VAR2,VALUES)
% 1:     in(VAR1,VALUES) and    in(VAR2,VALUES)
in_same_partition_a(FLAG, VAR1, VAR2, PARTITIONS) :-
    check_type(dvar, VAR1),
    check_type(dvar, VAR2),
    collection(PARTITIONS, [col_len_gteq(1, [int])]),
    length(PARTITIONS, P),
    P > 1,
    collection_distinct(PARTITIONS, 1),
    in_same_partition_signature(PARTITIONS, SIGNATURE, VAR1, VAR2),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

in_same_partition_signature([], [], _, _).
in_same_partition_signature([[p-VALUES]|PARTITIONs], [S|Ss], VAR1, VAR2) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    ((VAR1 in_set SET_OF_VALUES) #/\ (VAR2 in_set SET_OF_VALUES)) #<=> S,
    in_same_partition_signature(PARTITIONs, Ss, VAR1, VAR2).
