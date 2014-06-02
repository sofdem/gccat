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
    ctr_extensible/2,
    ctr_extensible/3,
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

ctr_date(max_occ_of_sorted_tuples_of_values,['20120327']).

ctr_origin(max_occ_of_sorted_tuples_of_values, 'Design.', []).

ctr_types(max_occ_of_sorted_tuples_of_values, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(max_occ_of_sorted_tuples_of_values,
              ['MAX'-int                          ,
	       'K'-int                            ,
               'VECTORS'-collection(vec-'VECTOR')]).

ctr_restrictions(max_occ_of_sorted_tuples_of_values,
                 [required('VECTOR',var)           ,
                  size('VECTOR')  >= 2             ,
		  alldifferent('VECTOR')           ,
                  'MAX'           >= 1             ,	  
		  'K'             >= 2             ,
		  'K'             <  size('VECTOR'),
                  required('VECTORS',vec)          ,
                  size('VECTORS') >= 1             ,
                  same_size('VECTORS',vec)         ]).

ctr_typical(max_occ_of_sorted_tuples_of_values,
            ['MAX' = 1             ,
	     'K'+1 = size('VECTOR'),
	     size('VECTORS') > 2   ]).

ctr_predefined(max_occ_of_sorted_tuples_of_values).

ctr_functional_dependency(max_occ_of_sorted_tuples_of_values, 1, [2,3]).

ctr_contractible(max_occ_of_sorted_tuples_of_values, ['MAX'=1], 'VECTORS', any).

ctr_example(max_occ_of_sorted_tuples_of_values,
            max_occ_of_sorted_tuples_of_values(1, 2, [[vec-[[var-4], [var-2], [var-1]]],
                                                      [vec-[[var-2], [var-3], [var-5]]],
                                                      [vec-[[var-3], [var-6], [var-4]]],
					              [vec-[[var-5], [var-4], [var-7]]],
					              [vec-[[var-6], [var-5], [var-1]]],
					              [vec-[[var-7], [var-6], [var-2]]],
					              [vec-[[var-3], [var-1], [var-7]]]])).

ctr_see_also(max_occ_of_sorted_tuples_of_values,
 [ %link('common keyword', min_occ_of_sorted_tuples_of_values,      '%k,%k', ['vector','functional dependency']),
  link('implied by',     max_occ_of_tuples_of_values,             '',      []      ),
  link('common keyword', max_occ_of_tuples_of_values,             '%k',    [vector]),
  link('common keyword', max_occ_of_consecutive_tuples_of_values, '%k',    [vector])]).

ctr_key_words(max_occ_of_sorted_tuples_of_values,['vector'               ,
					          'functional dependency']).

ctr_eval(max_occ_of_sorted_tuples_of_values, [checker(max_occ_of_sorted_tuples_of_values_c)]).

max_occ_of_sorted_tuples_of_values_c(MAX, K, VECTORS) :-
    (integer(MAX) -> MAX >= 1 ; true),
    integer(K),
    K >= 2,
    collection(VECTORS, [col([int])]),
    same_size(VECTORS),
    VECTORS = [[vec-VECTOR]|_],
    length(VECTOR, N),
    N >= 2,
    K < N,
    generate_subtuples(VECTORS, K, 1, SUBTUPLES), % check that components are distinct when sort
    create_pairs(SUBTUPLES, PSUBTUPLES),
    keysort(PSUBTUPLES, SORTED),
    (integer(MAX) -> Limit is MAX ; length(SORTED, Limit)),
    get_max_occ_tuples_of_values(SORTED, Limit, 0, M), 
    MAX = M,
    MAX >= 1.
