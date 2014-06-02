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

ctr_date(element_matrix,['20031101','20060808']).

ctr_origin(element_matrix, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(element_matrix,
              ['MAX_I'-int                             ,
               'MAX_J'-int                             ,
               'INDEX_I'-dvar                          ,
               'INDEX_J'-dvar                          ,
               'MATRIX'-collection(i-int, j-int, v-int),
               'VALUE'-dvar                            ]).

ctr_exchangeable(element_matrix,
                 [vals(['MATRIX'^v,'VALUE'],int,=\=,all,dontcare)]).

ctr_synonyms(element_matrix,[elem_matrix, matrix]).

ctr_restrictions(element_matrix,
                 ['MAX_I'   >=  1                   ,
                  'MAX_J'   >=  1                   ,
                  'INDEX_I' >=  1                   ,
                  'INDEX_I' =< 'MAX_I'              ,
                  'INDEX_J' >=  1                   ,
                  'INDEX_J' =< 'MAX_J'              ,
                  required('MATRIX',[i,j,v])        ,
                  increasing_seq('MATRIX',[i,j])    ,
                  'MATRIX'^i >= 1                   ,
                  'MATRIX'^i =< 'MAX_I'             ,
                  'MATRIX'^j >= 1                   ,
                  'MATRIX'^j =< 'MAX_J'             ,
                  size('MATRIX') = 'MAX_I' * 'MAX_J']).

ctr_typical(element_matrix,
            ['MAX_I'            > 1,
             'MAX_J'            > 1,
             size('MATRIX')     > 3,
             maxval('MATRIX'^i) > 1,
             maxval('MATRIX'^j) > 1,
             range('MATRIX'^v)  > 1]).

ctr_derived_collections(element_matrix,
                        [col('ITEM'-collection(index_i-dvar,index_j-dvar,value-dvar),
                        [item(index_i-'INDEX_I',index_j-'INDEX_J',value-'VALUE')])]).

ctr_graph(element_matrix,
          ['ITEM','MATRIX'],
          2,
          ['PRODUCT'>>collection(item,matrix)],
          [item^index_i = matrix^i,
           item^index_j = matrix^j,
           item^value   = matrix^v],
          ['NARC' = 1],
          []).

ctr_example(element_matrix,
            element_matrix(4,3,1,3,[[i-1 ,j-1, v-4],
                                    [i-1 ,j-2, v-1],
                                    [i-1 ,j-3, v-7],
                                    [i-2 ,j-1, v-1],
                                    [i-2 ,j-2, v-0],
                                    [i-2 ,j-3, v-8],
                                    [i-3 ,j-1, v-3],
                                    [i-3 ,j-2, v-2],
                                    [i-3 ,j-3, v-1],
                                    [i-4 ,j-1, v-0],
                                    [i-4 ,j-2, v-0],
                                    [i-4 ,j-3, v-6]],7)).

ctr_draw_example(element_matrix,
                 ['ITEM','MATRIX'],
                 [[[index_i-1, index_j-3, value-7]],
                  [[i-1 ,j-1, v-4],
                   [i-1 ,j-2, v-1],
                   [i-1 ,j-3, v-7],
                   [i-2 ,j-1, v-1],
                   [i-2 ,j-2, v-0],
                   [i-2 ,j-3, v-8],
                   [i-3 ,j-1, v-3],
                   [i-3 ,j-2, v-2],
                   [i-3 ,j-3, v-1],
                   [i-4 ,j-1, v-0],
                   [i-4 ,j-2, v-0],
                   [i-4 ,j-3, v-6]]],
                 ['PRODUCT'],
                 [1-3],
                 ['NARC'],
                 '','NARC=1',
                 [4.5,2.3,1,1]).

ctr_see_also(element_matrix,
 [link('common keyword', element, '%k', ['array constraint']),
  link('common keyword', elem,    '%k', ['array constraint'])]).

ctr_key_words(element_matrix,['array constraint'                        ,
                              'data constraint'                         ,
                              'ternary constraint'                      ,
                              'matrix'                                  ,
                              'automaton'                               ,
                              'automaton without counters'              ,
                              'reified automaton constraint'            ,
                              'centered cyclic(3) constraint network(1)',
                              'derived collection'                      ,
                              'arc-consistency'                         ]).

ctr_eval(element_matrix, [reformulation(element_matrix_r),
                          automaton(element_matrix_a)]).

element_matrix_r(MAX_I, MAX_J, INDEX_I, INDEX_J, MATRIX, VALUE) :-
    check_type(int, MAX_I),
    MAX_I >= 1,
    check_type(int, MAX_J),
    MAX_J >= 1,
    check_type(dvar, INDEX_I),
    INDEX_I #>= 1,
    INDEX_I #=< MAX_I,
    check_type(dvar, INDEX_J),
    INDEX_J #>= 1,
    INDEX_J #=< MAX_J,
    collection(MATRIX, [int(1,MAX_I),int(1,MAX_J),int]),
    length(MATRIX, N),
    N is MAX_I*MAX_J,
    collection_increasing_seq(MATRIX,[1,2]),
    check_type(dvar, VALUE),
    get_attr3(MATRIX , VALUES),
    element_matrix1(MAX_I, MAX_J, INDEX_J, VALUES, TABLE_VARS),
    element(INDEX_I, TABLE_VARS, VALUE).

element_matrix1(0, _, _, _, []) :- !.
element_matrix1(I, MAX_J, INDEX_J, VALUES, [V_J|R]) :-
    I > 0,
    element_matrix2(MAX_J, VALUES, TABLE_VALS, REST_VALUES),
    element(INDEX_J, TABLE_VALS, V_J),
    I1 is I-1,
    element_matrix1(I1, MAX_J, INDEX_J, REST_VALUES, R).

element_matrix2(0, VALUES, [], VALUES) :- !.
element_matrix2(J, [V|R], [V|S], REST_VALUES) :-
    J > 0,
    J1 is J-1,
    element_matrix2(J1, R, S, REST_VALUES).

% 0: INDEX_I=\=MATRIX_I or  INDEX_J=\=MATRIX_J or  VALUE=\=MATRIX_V
% 1: INDEX_I = MATRIX_I and INDEX_J = MATRIX_J and VALUE = MATRIX_V
element_matrix_a(FLAG, MAX_I, MAX_J, INDEX_I, INDEX_J, MATRIX, VALUE) :-
    check_type(int, MAX_I),
    MAX_I >= 1,
    check_type(int, MAX_J),
    MAX_J >= 1,
    check_type(dvar, INDEX_I),
    INDEX_I #>= 1,
    INDEX_I #=< MAX_I,
    check_type(dvar, INDEX_J),
    INDEX_J #>= 1,
    INDEX_J #=< MAX_J,
    collection(MATRIX, [int(1,MAX_I),int(1,MAX_J),int]),
    length(MATRIX, N),
    N is MAX_I*MAX_J,
    collection_increasing_seq(MATRIX,[1,2]),
    check_type(dvar, VALUE),
    element_matrix_signature(MATRIX, INDEX_I, INDEX_J, VALUE, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

element_matrix_signature([], _, _, _, []).
element_matrix_signature([[i-I, j-J, v-V]|Ms], INDEX_I, INDEX_J, VALUE, [S|Ss]) :-
    INDEX_I#=I #/\ INDEX_J#=J #/\ VALUE#=V #<=> S,
    element_matrix_signature(Ms, INDEX_I, INDEX_J, VALUE, Ss).
