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

ctr_date(nvector,['20081220']).

ctr_origin(nvector, 'Introduced by G.~Chabert as a generalisation of %c', [nvalue]).

ctr_types(nvector, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(nvector,
              ['NVEC'-dvar                       ,
               'VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(nvector,
                 [items('VECTORS',all),
                  items_sync('VECTORS'^vec,all),
                  vals(['VECTORS'^vec],int,=\=,all,dontcare)]).

ctr_synonyms(nvector,[nvectors,
                      npoint  ,
                      npoints ]).

ctr_restrictions(nvector,
                 [size('VECTOR') >= 1                     ,
                  'NVEC'         >= min(1,size('VECTORS')),
                  'NVEC'         =< size('VECTORS')       ,
                  required('VECTORS',vec)                 ,
                  same_size('VECTORS',vec)                ]).

ctr_typical(nvector,
            [size('VECTOR')  > 1              ,
             'NVEC'          > 1              ,
             'NVEC'          < size('VECTORS'),
             size('VECTORS') > 1              ]).

ctr_pure_functional_dependency(nvector, []).
ctr_functional_dependency(nvector, 1, [2]).

ctr_contractible(nvector, ['NVEC'=1,size('VECTORS')>0], 'VECTORS', any).
ctr_contractible(nvector, ['NVEC'=size('VECTORS')], 'VECTORS', any).

ctr_graph(nvector,
          ['VECTORS'],
          2,
          ['CLIQUE'>>collection(vectors1,vectors2)],
          [lex_equal(vectors1^vec,vectors2^vec)],
          ['NSCC' = 'NVEC'],
          ['EQUIVALENCE']).

ctr_example(nvector,
            nvector(2, [[vec-[[var-5], [var-6]]],
                        [vec-[[var-5], [var-6]]],
                        [vec-[[var-9], [var-3]]],
                        [vec-[[var-5], [var-6]]],
                        [vec-[[var-9], [var-3]]]])).

ctr_draw_example(nvector,
                 ['VECTORS'],
                 [[[vec-[[var-5], [var-6]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-9], [var-3]]],
                   [vec-[[var-5], [var-6]]],
                   [vec-[[var-9], [var-3]]]]],
                 ['CLIQUE'],
                 [1-[1,2,4],
                  2-[1,2,4],
                  3-[3,5],
                  4-[1,2,4],
                  5-[3,5]],
                 ['NSCC'([[1,2,4],[3,5]])],
                 '','NSCC=2',
                 [2.145,2.3,2.8,2.33]).

ctr_see_also(nvector,
 [link('specialisation', nvalue,                  '%k replaced by %e',                                                                                            [vector, variable]),
  link('generalisation', nvectors,                'replace an equality with the number of distinct vectors by a comparison with the number of distinct nvectors', []),
  link('implies',        atleast_nvector,         '$=\\argument{NVEC}$ replaced by $\\geq\\argument{NVEC}$',                                                      []),
  link('implies',        atmost_nvector,          '$=\\argument{NVEC}$ replaced by $\\leq\\argument{NVEC}$',                                                      []),
  link('implied by',     ordered_nvector,         '',                                                                                                             []),
  link('common keyword', ordered_atleast_nvector, '%k',                                                                                                           [vector]),
  link('common keyword', ordered_atmost_nvector,  '%k',                                                                                                           [vector]),
  link('common keyword', lex_equal,               '%k',                                                                                                           [vector])]).

ctr_key_words(nvector,['counting constraint'                   ,
                       'value partitioning constraint'         ,
                       'number of distinct equivalence classes',
                       'strongly connected component'          ,
                       'domination'                            ,
                       'equivalence'                           ,
                       'vector'                                ,
                       'rectangle clique partition'            ,
                       'SLAM problem'                          ,
                       'functional dependency'                 ,
		       'pure functional dependency'            ]).

ctr_persons(nvector,['Chabert G.',
                     'Lorca X.'  ,
                     'Jaulin L.' ]).

ctr_eval(nvector, [reformulation(nvector_r)]).

nvector_r(0, []) :-
    !.
nvector_r(NVEC, VECTORS) :-
    check_type(dvar, NVEC),
    collection(VECTORS, [col([dvar])]),
    same_size(VECTORS),
    length(VECTORS, N),
    NVEC #>= min(1,N),
    NVEC #=< N,
    nvector_common(NVEC, VECTORS).
