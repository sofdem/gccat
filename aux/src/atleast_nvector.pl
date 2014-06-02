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

ctr_date(atleast_nvector,['20081226']).

ctr_origin(atleast_nvector, 'Derived from %c', [nvector]).

ctr_types(atleast_nvector, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(atleast_nvector,
              ['NVEC'-dvar                       ,
               'VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(atleast_nvector,
                 [vals(['NVEC'],int(>=(0)),>,dontcare,dontcare),
                  items('VECTORS',all),
                  items_sync('VECTORS'^vec,all),
                  vals(['VECTORS'^vec],int,=\=,all,dontcare)]).

ctr_restrictions(atleast_nvector,
                 [size('VECTOR') >= 1              ,
                  'NVEC'         >= 0              ,
                  'NVEC'         =< size('VECTORS'),
                  required('VECTORS',vec)          ,
                  same_size('VECTORS',vec)         ]).

ctr_typical(atleast_nvector,
            [size('VECTOR')  > 1              ,
             'NVEC'          > 1              ,
             'NVEC'          < size('VECTORS'),
             size('VECTORS') > 1              ]).

ctr_extensible(atleast_nvector, [], 'VECTORS', any).

ctr_graph(atleast_nvector,
          ['VECTORS'],
          2,
          ['CLIQUE'>>collection(vectors1,vectors2)],
          [lex_equal(vectors1^vec,vectors2^vec)],
          ['NSCC' >= 'NVEC'],
          ['EQUIVALENCE']).

ctr_example(atleast_nvector,
            atleast_nvector(2, [[vec-[[var-5], [var-6]]],
                                [vec-[[var-5], [var-6]]],
                                [vec-[[var-9], [var-3]]],
                                [vec-[[var-5], [var-6]]],
                                [vec-[[var-9], [var-4]]]])).

ctr_draw_example(atleast_nvector,
                 ['VECTORS'],
                 [[[vec-[[var-5], [var-6]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-9], [var-3]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-9], [var-4]]]]],
                 ['CLIQUE'],
                 [1-[1,2,4],
                  2-[1,2,4],
                  3-[3],
                  4-[1,2,4],
                  5-[5]],
                 ['NSCC'([[1,2,4],[3],[5]])],
                 '','NSCC=3',
                 [2.145,2.3,2.8,2.33]).

ctr_see_also(atleast_nvector,
 [link('comparison swapped',        atmost_nvector,          '',                              []),
  link('implied by',                nvector,                 '$\\geq$ %e replaced by $=$ %e', ['NVEC','NVEC']),
  link('implied by',                ordered_atleast_nvector, '',                              []),
  link('used in graph description', lex_equal,               '',                              [])]).

ctr_key_words(atleast_nvector,['counting constraint'                   ,
                               'value partitioning constraint'         ,
                               'number of distinct equivalence classes',
                               'strongly connected component'          ,
                               'domination'                            ,
                               'equivalence'                           ,
                               'vector'                                ]).

ctr_eval(atleast_nvector, [reformulation(atleast_nvector_r)]).

atleast_nvector_r(NVEC, []) :-
    !,
    check_type(dvar, NVEC),
    NVEC #= 0.
atleast_nvector_r(NVEC, VECTORS) :-
    check_type(dvar, NVEC),
    length(VECTORS, N),
    NVEC #>= 0,
    NVEC #=< N,
    NV in 0..N,
    nvector_common(NV, VECTORS),    
    NV #>= NVEC.
