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

ctr_date(correspondence,['20030820','20060806']).

ctr_origin(correspondence, 'Derived from %c by removing the sorting condition.', [sort_permutation]).

ctr_arguments(correspondence,
              ['FROM'-collection(from-dvar)      ,
               'PERMUTATION'-collection(var-dvar),
               'TO'-collection(tvar-dvar)        ]).

ctr_exchangeable(correspondence,
                 [vals(['FROM'^from,'TO'^tvar],int,=\=,all,dontcare)]).

ctr_restrictions(correspondence,
                 [size('PERMUTATION') = size('FROM')      ,
                  size('PERMUTATION') = size('TO')        ,
                  'PERMUTATION'^var >= 1                  ,
                  'PERMUTATION'^var =< size('PERMUTATION'),
                  alldifferent('PERMUTATION')             ,
                  required('FROM',from)                   ,
                  required('PERMUTATION',var)             ,
                  required('TO',tvar)                     ]).

ctr_typical(correspondence,
            [size('FROM')       > 1,
             range('FROM'^from) > 1]).

ctr_derived_collections(correspondence,
                        [col('FROM_PERMUTATION'-collection(from-dvar,var-dvar),
                             [item(from-'FROM'^from,var-'PERMUTATION'^var)])]).

ctr_graph(correspondence,
          ['FROM_PERMUTATION','TO'],
          2,
          ['PRODUCT'>>collection(from_permutation,to)],
          [from_permutation^from = to^tvar,
           from_permutation^var  = to^key ],
          ['NARC' = size('PERMUTATION')],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(correspondence,
            correspondence([[from-1],[from-9],[from-1],[from-5],[from-2],[from-1]],
                           [[ var-6],[ var-1],[ var-3],[ var-5],[ var-4],[ var-2]],
                           [[tvar-9],[tvar-1],[tvar-1],[tvar-2],[tvar-5],[tvar-1]])).

ctr_draw_example(correspondence,
                 ['FROM_PERMUTATION','TO'],
                 [[[from-1,var-6],[from-9,var-1],[from-1,var-3],
                   [from-5,var-5],[from-2,var-4],[from-1,var-2]],
                  [[tvar-9],[tvar-1],[tvar-1],[tvar-2],[tvar-5],[tvar-1]]],
                 ['PRODUCT'],
                 [1-6,2-1,3-3,4-5,5-4,6-2],
                 ['NARC'],
                 '','NARC=6',
                 [2.6,2.145,2.8,2.0]).

ctr_see_also(correspondence,
 [link(specialisation, same,             '%e parameter removed', ['PERMUTATION']),
  link('implied by',   sort_permutation, '',                     [])]).

ctr_persons(correspondence,['Cymer R.']).

ctr_key_words(correspondence,['constraint between three collections of variables',
                              'permutation'                                      ,
			      'bipartite matching'                               ,
                              'derived collection'                               ,
                              'acyclic'                                          ,
                              'bipartite'                                        ,
                              'no loop'                                          ]).

ctr_eval(correspondence, [reformulation(correspondence_r)]).

correspondence_r(FROM, PERMUTATION, TO) :-
    collection(FROM, [dvar]),
    length(FROM, NFROM),
    collection(PERMUTATION, [dvar(1,NFROM)]),
    length(PERMUTATION, NPERMUTATION),
    collection(TO, [dvar]),
    length(TO, NTO),
    NPERMUTATION = NFROM,
    NPERMUTATION = NTO,
    get_attr1(FROM, FROMS),
    get_attr1(PERMUTATION, PERMS),
    get_attr1(TO, TOS),
    all_different(PERMS),
    correspondence1(PERMS, FROMS, TOS).

correspondence1([], [], _).
correspondence1([Pi|R], [Fi|S], TOS) :-
    element(Pi, TOS, Fi),
    correspondence1(R, S, TOS).
