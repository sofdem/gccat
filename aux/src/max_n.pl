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

ctr_date(max_n,['20000128','20030820','20041230','20060811']).

ctr_origin(max_n, '\\cite{Beldiceanu01}', []).

ctr_arguments(max_n,
              ['MAX'-dvar,
               'RANK'-int,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(max_n,
                 [items('VARIABLES',all),
                  translate(['MAX','VARIABLES'^var])]).

ctr_restrictions(max_n,
                 ['RANK' >= 0               ,
                  'RANK' < size('VARIABLES'),
                  size('VARIABLES') > 0     ,
                  required('VARIABLES',var) ]).

ctr_typical(max_n,
            ['RANK'                 > 0,
             'RANK'                 < 3,
             size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_pure_functional_dependency(max_n, []).
ctr_functional_dependency(max_n, 1, [2,3]).

ctr_graph(max_n,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [(variables1^key = variables2^key #\/ variables1^var > variables2^var)],
          ['ORDER'('RANK','MININT',var) = 'MAX'],
          []).

ctr_example(max_n,
            max_n(6, 1, [[var-3],[var-1],[var-7],[var-1],[var-6]])).

ctr_draw_example(max_n,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-7],[var-1],[var-6]]],
                 ['CLIQUE'],
                 [1-[1,2,4],
                  2-2,
                  3-[1,2,3,4,5],
                  4-4,
                  5-[1,2,4,5]],
                 ['ORDER'([5])],
                 '','ORDER(1,MININT,var)=6',
                 []).

ctr_see_also(max_n,
 [link('generalisation',     maximum, 'absolute maximum replaced by maximum or order %e', [n]),
  link('comparison swapped', min_n,   '',                                                 [])]).

ctr_key_words(max_n,['order constraint'          ,
                     'rank'                      ,
                     'maximum'                   ,
                     'functional dependency'     ,
		     'pure functional dependency']).

ctr_persons(max_n,['Beldiceanu N.']).

ctr_eval(max_n, [      checker(max_n_c),
		 reformulation(max_n_r)]).

max_n_c(MAX, RANK, VARIABLES) :-
    length(VARIABLES, N),
    N > 0,
    N1 is N-1,
    check_type(dvar, MAX),
    check_type(int(0,N1), RANK),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    sort(VARS, SVARS),
    length(SVARS, NN),
    Pos is NN-RANK,
    nth1(Pos, SVARS, MAX).

max_n_r(MAX, RANK, VARIABLES) :-
    length(VARIABLES, N),
    N > 0,
    N1 is N-1,
    check_type(dvar, MAX),
    check_type(int(0,N1), RANK),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    create_collection([MAX], var, VMAX),
    create_collection(VARS, val, VALUES),
    eval(among_var(1, VMAX, VALUES)),
    NVAL in 0..N,
    eval(nvalue(NVAL, VARIABLES)),
    length(RANKS, N),
    domain(RANKS, 0, N1),
    max_n1(VARS, RANKS, MAX, RANK, NVAL).

max_n1([], [], _, _, _).
max_n1([V|RV], [R|RR], MAX, RANK, NVAL) :-
    R #< NVAL,
    R #= RANK #<=> V #= MAX,
    max_n2(RV, RR, V, R),
    max_n1(RV, RR, MAX, RANK, NVAL).

max_n2([], [], _, _).
max_n2([Vj|RV], [Rj|RR], Vi, Ri) :-
    Vi #> Vj #<=> Ri #< Rj,
    Vi #= Vj #<=> Ri #= Rj,
    Vi #< Vj #<=> Ri #> Rj,
    max_n2(RV, RR, Vi, Ri).
