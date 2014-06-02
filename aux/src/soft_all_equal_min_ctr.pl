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
    ctr_total_relation/1,
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

ctr_date(soft_all_equal_min_ctr,['20081004']).

ctr_origin(soft_all_equal_min_ctr, '\\cite{HebrardSullivanRazgon08}', []).

ctr_arguments(soft_all_equal_min_ctr,
              ['N'-int                         ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(soft_all_equal_min_ctr,
                 [vals(['N'],int(>=(0)),>,dontcare,dontcare),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(soft_all_equal_min_ctr,[soft_alldiff_max_ctr     ,
                                     soft_alldifferent_max_ctr,
                                     soft_alldistinct_max_ctr ]).

ctr_restrictions(soft_all_equal_min_ctr,
                 ['N' >= 0                                                    ,
                  'N' =< size('VARIABLES')*size('VARIABLES')-size('VARIABLES'),
                  required('VARIABLES',var)                                   ]).

ctr_typical(soft_all_equal_min_ctr,
            ['N'               > 0                                                    ,
             'N'               < size('VARIABLES')*size('VARIABLES')-size('VARIABLES'),
             size('VARIABLES') > 1                                                    ]).

ctr_total_relation(soft_all_equal_min_ctr).

ctr_graph(soft_all_equal_min_ctr,
          ['VARIABLES'],
          2,
          ['CLIQUE'(=\=)>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['NARC' >= 'N'],
          []).

ctr_example(soft_all_equal_min_ctr,
            soft_all_equal_min_ctr(6, [[var-5],[var-1],[var-5],[var-5]])).

ctr_draw_example(soft_all_equal_min_ctr,
                 ['VARIABLES'],
                 [[[var-5],[var-1],[var-5],[var-5]]],
                 ['CLIQUE'(=\=)],
                 [1-[3,4],
                  3-[1,4],
                  4-[1,3]],
                 ['NARC'],
                 '','NARC=6',
                 []).

ctr_see_also(soft_all_equal_min_ctr,
 [link('implied by',     and,                    '',   []),
  link('implied by',     balance,                '',   []),
  link('implied by',     equivalent,             '',   []),
  link('implied by',     nor,                    '',   []),
  link('hard version',   all_equal,              '',   []),
  link('common keyword', soft_all_equal_max_var, '%k', ['soft constraint']),
  link('common keyword', soft_all_equal_min_var, '%k', ['soft constraint']),
  link('common keyword', soft_alldifferent_ctr,  '%k', ['soft constraint']),
  link('common keyword', soft_alldifferent_var,  '%k', ['soft constraint']),
  link('related',        atmost_nvalue,          '',   [])]).

ctr_key_words(soft_all_equal_min_ctr,['soft constraint'                      ,
                                      'value constraint'                     ,
                                      'relaxation'                           ,
                                      'decomposition-based violation measure',
                                      '3-dimensional-matching'               ,
                                      'bound-consistency'                    ]).

ctr_persons(soft_all_equal_min_ctr,['Hebrard E.'    ,
                                    'O\'Sullivan B.',
                                    'Razgon I.'     ,
                                    'Marx D.'       ]).

ctr_eval(soft_all_equal_min_ctr, [      checker(soft_all_equal_min_ctr_c),
				  reformulation(soft_all_equal_min_ctr_r)]).

soft_all_equal_min_ctr_c(N, VARIABLES) :-
    collection(VARIABLES,[int]),
    length(VARIABLES, L),
    L2 is L*L-L,
    check_type(dvar(0,L2), N),
    (L = 0 ->
	N = 0
    ;
	get_attr1(VARIABLES, VARS),
        samsort(VARS, SVARS),
        SVARS = [V|R],
        soft_all_equal_min_ctr_c(R, 1, V, 0, NB_EQ_CTR),
        N #=< NB_EQ_CTR
    ).

soft_all_equal_min_ctr_c([], C, _, Sum, Res) :-
    !,
    Res is C*C-C+Sum.
soft_all_equal_min_ctr_c([V|R], C, V, Sum, Res) :-
    !,
    C1 is C+1,
    soft_all_equal_min_ctr_c(R, C1, V, Sum, Res).
soft_all_equal_min_ctr_c([V|R], C, Prev, Sum, Res) :-
    C > 0,
    V =\= Prev,
    NewSum is C*C-C+Sum,
    soft_all_equal_min_ctr_c(R, 1, V, NewSum, Res).

soft_all_equal_min_ctr_r(N, VARIABLES) :-
    collection(VARIABLES,[dvar]),
    length(VARIABLES, L),
    L2 is L*L-L,
    check_type(dvar(0,L2), N),
    get_attr1(VARIABLES, VARS),
    soft_all_equal_min_ctr1(VARS, TERM),
    call(N #=< TERM).

soft_all_equal_min_ctr1([], 0).
soft_all_equal_min_ctr1([V|R], S+T) :-
    soft_all_equal_min_ctr2(R, V, S),
    soft_all_equal_min_ctr1(R, T).

soft_all_equal_min_ctr2([], _, 0).
soft_all_equal_min_ctr2([U|R], V, 2*B+T) :-
    B #<=> U #= V,
    soft_all_equal_min_ctr2(R, V, T).
