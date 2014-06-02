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

ctr_date(cardinality_atleast,['20030820','20040530','20060805']).

ctr_origin(cardinality_atleast, 'Derived from %c.', [global_cardinality]).

ctr_arguments(cardinality_atleast,
              ['ATLEAST'-dvar                  ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(cardinality_atleast,
                 [items('VARIABLES',all),
                  items('VALUES',all),
                  vals(['VARIABLES'^var],all(notin('VALUES'^val)),=,dontcare,dontcare),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(cardinality_atleast,
                 ['ATLEAST' >= 0                ,
                  'ATLEAST' =< size('VARIABLES'),
                  required('VARIABLES',var)     ,
                  required('VALUES',val)        ,
                  distinct('VALUES',val)        ]).

ctr_typical(cardinality_atleast,
            ['ATLEAST' > 0                     ,
             'ATLEAST' < size('VARIABLES')     ,
             size('VARIABLES') > 1             ,
             size('VALUES')    > 0             ,
             size('VARIABLES') > size('VALUES')]).

ctr_pure_functional_dependency(cardinality_atleast, []).
ctr_functional_dependency(cardinality_atleast, 1, [2,3]).

ctr_graph(cardinality_atleast,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var =\= values^val],
          ['MAX_ID' = size('VARIABLES')-'ATLEAST'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(cardinality_atleast,
            cardinality_atleast(1,
                                [[var-3], [var-3], [var-8]],
                                [[val-3], [val-8]])).

ctr_draw_example(cardinality_atleast,
                 ['VARIABLES','VALUES'],
                 [[[var-3], [var-3], [var-8]],
                  [[val-3], [val-8]]],
                 ['PRODUCT'],
                 [1-2,2-2,3-1],
                 ['MAX_ID'([5])],
                 '','MAX_ID=2',
                 [1.4,1.4,1.4,1.1]).

ctr_see_also(cardinality_atleast,
 [link(generalisation, global_cardinality, 'single %e replaced by an individual %e for each value', ['count~variable','count~variable'])]).

ctr_key_words(cardinality_atleast,['value constraint'                ,
                                   'assignment'                      ,
                                   'functional dependency'           ,
		                   'pure functional dependency'      ,
                                   'at least'                        ,
                                   'automaton'                       ,
                                   'automaton with array of counters',
                                   'acyclic'                         ,
                                   'bipartite'                       ,
                                   'no loop'                         ,
                                   'arc-consistency'                 ]).

ctr_persons(cardinality_atleast,['R\\\'egin J.-C.']).

ctr_eval(cardinality_atleast, [reformulation(cardinality_atleast_r)]).

cardinality_atleast_r(ATLEAST, VARIABLES, VALUES) :-
      check_type(dvar, ATLEAST),
      ATLEAST #>= 0,
      collection(VARIABLES, [dvar]),
      length(VARIABLES, N),
      ATLEAST #=< N,
      (VALUES = [] ->
        true
        ;
        collection(VALUES, [int]),
        length(VALUES, M),
        get_attr1(VARIABLES, VARS),
        get_attr1(VALUES, VALS),
        all_different(VALS),
        length(NOCCS, M),
        fd_min(ATLEAST, MIN_ATLEAST),
        domain(NOCCS, MIN_ATLEAST, N),
        get_minimum(VARS, MINVARS),
        get_maximum(VARS, MAXVARS),
        get_minimum(VALS, MINVALS),
        get_maximum(VALS, MAXVALS),
        MIN is min(MINVARS,MINVALS),
        MAX is max(MAXVARS,MAXVALS),
        complete_card(MIN, MAX, N, VALS, NOCCS, VN),
        global_cardinality(VARS, VN)
    ).
