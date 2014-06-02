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

ctr_date(ordered_global_cardinality,['20090911']).

ctr_origin(ordered_global_cardinality, '\\cite{PetitRegin09}', []).

ctr_usual_name(ordered_global_cardinality, ordgcc).

ctr_synonyms(ordered_global_cardinality, [ordered_gcc]).

ctr_arguments(ordered_global_cardinality,
              ['VARIABLES'-collection(var-dvar)      ,
               'VALUES'-collection(val-int, omax-int)]).

ctr_exchangeable(ordered_global_cardinality,
                 [items('VARIABLES',all)]).

ctr_restrictions(ordered_global_cardinality,
                 [required('VARIABLES',var)         ,
                  size('VALUES') > 0                ,
                  required('VALUES',[val,omax])     ,
                  increasing_seq('VALUES',[val])    ,
                  'VALUES'^omax >= 0                ,
                  'VALUES'^omax =< size('VARIABLES')]).

ctr_contractible(ordered_global_cardinality, [], 'VALUES', any).

ctr_graph(ordered_global_cardinality,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var >= 'VALUES'^val],
          ['NVERTEX' =< 'VALUES'^omax],
          []).

ctr_example(ordered_global_cardinality,
            ordered_global_cardinality([[var-2],[var-0],[var-1],[var-0],[var-0]],
                                       [[val-0, omax-5],
                                        [val-1, omax-3],
                                        [val-2, omax-1]])).

ctr_draw_example(ordered_global_cardinality,
                 ['VARIABLES'],
                 [[[var-2],[var-0],[var-1],[var-0],[var-0]]],
                 ['SELF'],
                 [1-1,2-2,3-3,4-4,5-5],
                 ['NVERTEX',
                  'FOREACH'('VALUES',[0-[1,2,3,4,5],1-[1,3],2-[1]])],
                 '','0:NVERTEX=5, 1:NVERTEX=2, 2:NVERTEX=1',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(ordered_global_cardinality,
 [link('root concept', global_cardinality,            '',                                                                           []),
  link('related',      cumulative,                    'controlling the shape of the cumulative profile for breaking symmetry',      []),
  link('related',      global_cardinality_low_up,     '',                                                                           []),
  link('related',      increasing_global_cardinality, 'the order is imposed on the main variables, and not on the count variables', [])]).

ctr_key_words(ordered_global_cardinality,['value constraint',
                                          'assignment'      ,
                                          'arc-consistency' ,
                                          'order constraint']).

ctr_persons(ordered_global_cardinality,['R\\\'egin J.-C.',
                                        'Petit T.'       ]).

ctr_eval(ordered_global_cardinality, [reformulation(ordered_global_cardinality_r)]).

ordered_global_cardinality_r(VARIABLES, VALUES) :-
    length(VARIABLES, N),
	collection(VARIABLES, [dvar]),
	collection(VALUES, [int,int(0,N)]),
	length(VALUES, M),
    M > 0,
	collection_increasing_seq(VALUES,[1]),
    (N = 0 ->
        true
        ;
        get_attr1(VALUES, VALS ),
        get_attr2(VALUES, OMAXS),
        length(OCCS, M),
        domain(OCCS, 0, N),
        create_collection(VALS, OCCS, val, noccurrence, VALUES_GC),
        eval(global_cardinality(VARIABLES, VALUES_GC)),
        reverse(OCCS, ROCCS),
        build_sliding_sums(ROCCS, 0, SUMS),
        reverse(OMAXS, ROMAXS),
        ordered_global_cardinality1(SUMS, ROMAXS)
    ).

ordered_global_cardinality1([], []).
ordered_global_cardinality1([V|R], [L|S]) :-
    V #=< L,
    ordered_global_cardinality1(R, S).
