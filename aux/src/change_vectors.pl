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

ctr_date(change_vectors,['20110616']).

ctr_origin(change_vectors, 'Derived from %c', [change]).

ctr_types(change_vectors, ['VECTOR'-collection(var-dvar),
                           'CTR'-atom                   ]).

ctr_arguments(change_vectors,
              ['NCHANGE'-dvar                    ,
               'VECTORS'-collection(vec-'VECTOR'),
               'CTRS'-collection(ctr-'CTR')      ]).

ctr_restrictions(change_vectors,
                 [size('VECTOR') >= 1             ,
                  required('VECTOR',var)          ,
                  in_list('CTR',[=,=\=,<,>=,>,=<]),
                  'NCHANGE' >=  0                 ,
                  'NCHANGE' <  size('VECTORS')    ,
                  required('VECTORS',vec)         ,
                  same_size('VECTORS',vec)        ,
                  required('CTRS',ctr)            ,
                  size('CTRS') = size('VECTOR')   ]).

ctr_typical(change_vectors,
            [in_list('CTR',[=\=]),
             size('VECTOR')  > 1 ,
             'NCHANGE'       > 0 ,
             size('VECTORS') > 1 ]).

ctr_pure_functional_dependency(change_vectors, []).
ctr_functional_dependency(change_vectors, 1, [2,3]).

ctr_example(change_vectors,
            change_vectors(3,
                           [[vec-[[var-4], [var-0]]],
                            [vec-[[var-4], [var-0]]],
                            [vec-[[var-4], [var-5]]],
                            [vec-[[var-3], [var-4]]],
                            [vec-[[var-3], [var-4]]],
                            [vec-[[var-3], [var-4]]],
                            [vec-[[var-4], [var-0]]]],
                           [[ctr-(=\=)],[ctr-(=\=)]])).

ctr_see_also(change_vectors,
 [link('specialisation', change,      '%k replaced by %e',       [vector, variable]),
  link('specialisation', change_pair, '%k replaced by %e of %e', [vector, pair, variables])]).

ctr_key_words(change_vectors,['Berge-acyclic constraint network',
                              'automaton'                       ,
                              'automaton with counters'         ,
                              'number of changes'               ,
                              'vector'                          ,
                              'functional dependency'           ,
			      'pure functional dependency'      ]).

ctr_eval(change_vectors, [automaton(change_vectors_a)]).

change_vectors_a(FLAG, NCHANGE, VECTORS, CTRS) :-
    collection(VECTORS, [col([dvar])]),
    length(VECTORS, N),
    N_1 is N-1,
    check_type(dvar(0, N_1), NCHANGE),
    collection(CTRS, [atom([=,=\=,<,>=,>,=<])]),
    same_size(VECTORS),
    length(CTRS, M),
    VECTORS = [[_-VECTOR1]|_],
    length(VECTOR1, M),
    M >= 1,
    get_attr11(VECTORS, VECTS),
    get_attr1(CTRS, LCTRS),
    change_vectors_signature(VECTS, SIGNATURE, LCTRS),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,0,s      ),
                           arc(s,1,s,[C+1])],
                          [C],[0],[NCHANGE]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

change_vectors_signature([], [], _) :- !.
change_vectors_signature([_], [], _) :- !.
change_vectors_signature([VEC1,VEC2|VECs], [S|Ss], CTRS) :- !,
    build_vectors_compare_change(VEC1, VEC2, CTRS, Term),
    call(Term #<=> S),
    change_vectors_signature([VEC2|VECs], Ss, CTRS).
