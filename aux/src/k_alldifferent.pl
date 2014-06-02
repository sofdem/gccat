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

ctr_date(k_alldifferent,['20050618','20060811']).

ctr_origin(k_alldifferent, '\\cite{ElbassioniKatrielKutzMahajan05}', []).

ctr_types(k_alldifferent, ['X'-collection(x-dvar)]).

ctr_arguments(k_alldifferent,
              ['VARS'-collection(vars-'X')]).

ctr_exchangeable(k_alldifferent,
                 [items('VARS',all),
                  items('VARS'^vars,all),
                  vals(['VARS'^vars^x],int,=\=,all,dontcare)]).

ctr_synonyms(k_alldifferent,[k_alldiff     ,
                             k_alldistinct ,
                             some_different]).

ctr_restrictions(k_alldifferent,
                 [size('X')    >= 1    ,
                  required('X',x)      ,
                  required('VARS',vars),
                  size('VARS') >= 1    ]).

ctr_typical(k_alldifferent,
            [size('X')    > 1,
             size('VARS') > 1]).

ctr_contractible(k_alldifferent, [], 'VARS', any).

ctr_graph(k_alldifferent,
          ['VARS'^vars],
          2,
          foreach('VARS',['CLIQUE'>>collection(x1,x2)]),
          [x1^x = x2^x],
          ['MAX_NSCC' =< 1],
          []).

ctr_example(k_alldifferent,
            k_alldifferent([[vars-[[x-5], [x-6], [x-0], [x-9], [x-3]]],
                            [vars-[[x-5], [x-6], [x-1], [x-2]]]       ])).

ctr_see_also(k_alldifferent,
 [link('part of system of constraints', alldifferent,                '',                                                        []),
  link('generalisation',                diffn,                       'tasks for which the start attribute is not fixed',        []),
  link('generalisation',                geost,                       'tasks for which the start attribute is not fixed',        []),
  link('common keyword',                colored_matrix,              '%k',                                                      ['system of constraints']),
  link('related',                       nvalue,                      'implied by two overlapping %c',                           [alldifferent]),
  link('related',                       same_and_global_cardinality, 'implied by two overlapping %c and restriction on values', [alldifferent])]).

ctr_key_words(k_alldifferent,['system of constraints'   ,
                              'overlapping alldifferent',
                              'value constraint'        ,
                              'decomposition'           ,
                              'bound-consistency'       ,
                              'set packing'             ,
                              'permutation'             ,
                              'all different'           ,
                              'disequality'             ,
                              'duplicated variables'    ,
                              'graph colouring'         ,
                              'Latin square'            ,
                              'Sudoku'                  ,
                              'air traffic management'  ,
                              'assignment'              ]).

ctr_persons(k_alldifferent,['Elbassioni K. M.',
                            'Katriel I.'      ,
                            'Kutz M.'         ,
                            'Mahajan M.'      ,
                            'Simonis H.'      ,
                            'Dincbas M.'      ,
                            'Barnier N.'      ,
                            'Brisset P.'      ,
                            'Martin P.'       ,
                            'Shmoys D. B.'    ,
                            'Richter Y.'      ,
                            'Freund A.'       ,
                            'Naveh Y.'        ,
                            'Lardeux F.'      ,
                            'Monfroy \\\'E.'  ,
                            'Saubion F.'      ,
                            'Bessi\\`ere C.'  ,
                            'Katsirelos G.'   ,
                            'Narodytska N.'   ,
                            'Quimper C.-G.'   ,
                            'Walsh T.'        ,
                            'Asaf S.'         ,
                            'Eran H.'         ,
                            'Richter Y.'      ,
                            'Connors D. P.'   ,
                            'Gresh D. L.'     ,
                            'Ortega J.'       ,
                            'Mcinnis M. J.'   ]).

ctr_eval(k_alldifferent, [checker(k_alldifferent_c)      ,
			  reformulation(k_alldifferent_r)]).

k_alldifferent_c(VARS) :-
    length(VARS, N),
    N > 0,
    collection(VARS, [non_empty_col([int])]),
    get_col_attr1(VARS, 1, VS),
    k_alldifferent0(VS).

k_alldifferent0([]).
k_alldifferent0([V|R]) :-
    sort(V, S),
    length(V, N),
    length(S, N),
    k_alldifferent0(R).

k_alldifferent_r(VARS) :-
    length(VARS, N),
    N > 0,
    collection(VARS, [non_empty_col([dvar])]),
    get_col_attr1(VARS, 1, VS),
    k_alldifferent1(VS).

k_alldifferent1([]).
k_alldifferent1([V|R]) :-
    all_different(V),
    k_alldifferent1(R).
