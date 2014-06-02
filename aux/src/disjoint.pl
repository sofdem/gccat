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

ctr_date(disjoint,['20000315','20031017','20040530','20060808']).

ctr_origin(disjoint, 'Derived from %c.', [alldifferent]).

ctr_arguments(disjoint,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(disjoint,
                 [args([['VARIABLES1','VARIABLES2']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var],int,=\=,dontcare,in),
                  vals(['VARIABLES2'^var],int,=\=,dontcare,in),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_restrictions(disjoint,
                 [required('VARIABLES1',var),
                  required('VARIABLES2',var)]).

ctr_typical(disjoint,
            [size('VARIABLES1') > 1,
             size('VARIABLES2') > 1]).

ctr_contractible(disjoint, [], 'VARIABLES1', any).
ctr_contractible(disjoint, [], 'VARIABLES2', any).

ctr_graph(disjoint,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NARC' = 0],
          []).

ctr_example(disjoint,
            disjoint([[var-1],[var-9],[var-1],[var-5]],
                     [[var-2],[var-7],[var-7],[var-0],[var-6],[var-8]])).

ctr_draw_example(disjoint,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5]],
                  [[var-2],[var-7],[var-7],[var-0],[var-6],[var-8]]],
                 ['PRODUCT'],
                 [],
                 ['NARC'],
                 '','NARC=0',
                 [2.6,2.145,1,1]).

ctr_see_also(disjoint,
 [link(implies,                 alldifferent_on_intersection, '',                  []),
  link(implies,                 lex_different,                '',                  []),
  link(generalisation,          disjoint_tasks,               '%e replaced by %e', [variable,task]),
  link('system of constraints', k_disjoint,                   '',                  [])]).

ctr_key_words(disjoint,['value constraint'                ,
                        'empty intersection'              ,
                        'disequality'                     ,
                        'bipartite matching'              ,
                        'automaton'                       ,
                        'automaton with array of counters']).

ctr_persons(disjoint,['Beldiceanu N.',
                      'Carlsson M.'  ,
                      'Thiel S.'     ]).

ctr_eval(disjoint, [reformulation(disjoint_r)]).

disjoint_r(VARIABLES1, VARIABLES2) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    disjoint1_(VARS1, VARS2).

disjoint1_([], _).
disjoint1_([V|R], VARS2) :-
    disjoint2_(VARS2, V),
    disjoint1_(R, VARS2).

disjoint2_([], _).
disjoint2_([U|R], V) :-
    U #\= V,
    disjoint2_(R, V).
