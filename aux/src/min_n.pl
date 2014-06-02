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

ctr_date(min_n,['20000128','20030820','20040530','20041230','20060811']).

ctr_origin(min_n, '\\cite{Beldiceanu01}', []).

ctr_arguments(min_n,
              ['MIN'-dvar                      ,
               'RANK'-int                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(min_n,
                 [items('VARIABLES',all),
                  translate(['MIN','VARIABLES'^var])]).

ctr_restrictions(min_n,
                 [size('VARIABLES') > 0     ,
                  'RANK' >= 0               ,
                  'RANK' < size('VARIABLES'),
                  required('VARIABLES',var) ]).

ctr_typical(min_n,
            ['RANK'                 > 0,
             'RANK'                 < 3,
             size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_pure_functional_dependency(min_n, []).
ctr_functional_dependency(min_n, 1, [2,3]).

ctr_graph(min_n,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^key = variables2^key #\/ variables1^var < variables2^var],
          ['ORDER'('RANK','MAXINT',var) = 'MIN'],
          []).

ctr_example(min_n,
            min_n(3, 1, [[var-3],[var-1],[var-7],[var-1],[var-6]])).

ctr_draw_example(min_n,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-7],[var-1],[var-6]]],
                 ['CLIQUE'],
                 [1-[1,3,5],
                  2-[1,2,3,5],
                  3-[3],
                  4-[1,3,4,5],
                  5-[3,5]],
                 ['ORDER'([1])],
                 '','ORDER(1,MAXINT,var)=3',
                 []).

ctr_cond_imply(min_n, atleast,              [],                                        ['N' = 1], [none, 'VARIABLES', 'MIN']).
ctr_cond_imply(min_n, minimum_greater_than, ['RANK' = 1, minval('VARIABLES'^var) = 1], [],        id                        ).

ctr_see_also(min_n,
 [link('generalisation',        minimum,   'absolute minimum replaced by minimum or order %e', [n]),
  link('comparison swapped',    max_n,     '',                                                 []),
  link('used in reformulation', among_var, '',                                                 []),
  link('used in reformulation', nvalue,    '',                                                 [])]).

ctr_key_words(min_n,['order constraint'                ,
                     'rank'                            ,
                     'minimum'                         ,
                     'maxint'                          ,
                     'automaton'                       ,
                     'automaton with array of counters',
                     'functional dependency'           ,
		     'pure functional dependency'      ]).

ctr_persons(min_n,['Beldiceanu N.']).

ctr_eval(min_n, [      checker(min_n_c),
		 reformulation(min_n_r)]).

min_n_c(MIN, RANK, VARIABLES) :-
    length(VARIABLES, N),
    N > 0,
    N1 is N-1,
    check_type(dvar, MIN),
    check_type(int(0,N1), RANK),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    sort(VARS, SVARS),
    nth0(RANK, SVARS, MIN).

min_n_r(MIN, RANK, VARIABLES) :-
    length(VARIABLES, N),
    N > 0,
    N1 is N-1,
    check_type(dvar, MIN),
    check_type(int(0,N1), RANK),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    create_collection([MIN], var, VMIN),
    create_collection(VARS, val, VALUES),
    eval(among_var(1, VMIN, VALUES)),
    NVAL in 0..N,
    eval(nvalue(NVAL, VARIABLES)),
    length(RANKS, N),
    domain(RANKS, 0, N1),
    min_n1(VARS, RANKS, MIN, RANK, NVAL).

min_n1([], [], _, _, _).
min_n1([V|RV], [R|RR], MIN, RANK, NVAL) :-
    R #< NVAL,
    R #= RANK #<=> V #= MIN,
    min_n2(RV, RR, V, R),
    min_n1(RV, RR, MIN, RANK, NVAL).

min_n2([], [], _, _).
min_n2([Vj|RV], [Rj|RR], Vi, Ri) :-
    Vi #< Vj #<=> Ri #< Rj,
    Vi #= Vj #<=> Ri #= Rj,
    Vi #> Vj #<=> Ri #> Rj,
    min_n2(RV, RR, Vi, Ri).
