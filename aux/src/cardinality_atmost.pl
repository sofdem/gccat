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

ctr_date(cardinality_atmost,['20030820','20040530','20060805']).

ctr_origin(cardinality_atmost, 'Derived from %c.', [global_cardinality]).

ctr_arguments(cardinality_atmost,
              ['ATMOST'-dvar                   ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(cardinality_atmost,
                 [items('VARIABLES',all),
                  items('VALUES',all),
                  vals(['VARIABLES'^var],all(notin('VALUES'^val)),=,dontcare,dontcare),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(cardinality_atmost,
                 ['ATMOST' >= 0                ,
                  'ATMOST' =< size('VARIABLES'),
                  required('VARIABLES',var)    ,
                  required('VALUES',val)       ,
                  distinct('VALUES',val)       ]).

ctr_typical(cardinality_atmost,
            ['ATMOST' > 0                      ,
             'ATMOST' < size('VARIABLES')      ,
             size('VARIABLES') > 1             ,
             size('VALUES')    > 0             ,
             size('VARIABLES') > size('VALUES')]).

ctr_pure_functional_dependency(cardinality_atmost, []).
ctr_functional_dependency(cardinality_atmost, 1, [2,3]).

ctr_graph(cardinality_atmost,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var = values^val],
          ['MAX_ID' = 'ATMOST'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(cardinality_atmost,
            cardinality_atmost(2,
                               [[var-2],[var-1],[var-7],[var-1],[var-2]],
                               [[val-5],[val-7],[val-2],[val-9]])).

ctr_draw_example(cardinality_atmost,
                 ['VARIABLES','VALUES'],
                 [[[var-2],[var-1],[var-7],[var-1],[var-2]],
                  [[val-5],[val-7],[val-2],[val-9]]],
                 ['PRODUCT'],
                 [1-3,3-2,5-3],
                 ['MAX_ID'([8])],
                 '','MAX_ID=2',
                 [2.145,2.145,2.145,1]).

ctr_see_also(cardinality_atmost,
 [link('implied by',   among,                '',                                                              []),
  link(generalisation, multi_inter_distance, 'window of size %e replaced by window of %e consecutive values', [1,'DIST']),
  link(generalisation, global_cardinality,   'single %e replaced by an individual %e for each value',         ['count~variable','count~variable'])]).

ctr_key_words(cardinality_atmost,['value constraint'                ,
                                  'assignment'                      ,
                                  'at most'                         ,
                                  'automaton'                       ,
                                  'automaton with array of counters',
                                  'acyclic'                         ,
                                  'bipartite'                       ,
                                  'no loop'                         ,
                                  'arc-consistency'                 ,
                                  'functional dependency'           ,
		                  'pure functional dependency'      ]).

ctr_persons(cardinality_atmost,['R\\\'egin J.-C.']).

ctr_eval(cardinality_atmost, [reformulation(cardinality_atmost_r)]).

cardinality_atmost_r(ATMOST, VARIABLES, VALUES) :-
	check_type(dvar, ATMOST),
	ATMOST #>= 0,
	collection(VARIABLES, [dvar]),
	length(VARIABLES, N),
	ATMOST #=< N,
	(VALUES = [] ->
        true
        ;
        collection(VALUES, [int]),
        length(VALUES, M),
        get_attr1(VARIABLES, VARS),
        get_attr1(VALUES, VALS),
        all_different(VALS),
        length(NOCCS, M),
        fd_max(ATMOST, MAX_ATMOST),
        domain(NOCCS, 0, MAX_ATMOST),
        get_minimum(VARS, MINVARS),
        get_maximum(VARS, MAXVARS),
        get_minimum(VALS, MINVALS),
        get_maximum(VALS, MAXVALS),
        MIN is min(MINVARS,MINVALS),
        MAX is max(MAXVARS,MAXVALS),
        complete_card(MIN, MAX, N, VALS, NOCCS, VN),
        global_cardinality(VARS, VN)
    ).
