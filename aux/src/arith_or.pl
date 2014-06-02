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
    ctr_predefined/2,
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
    ctr_pure_functional_dependency/1,
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

ctr_date(arith_or,['20040814','20060804']).

ctr_origin(arith_or, 'Used in the definition of several automata', []).

ctr_arguments(arith_or,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'RELOP'-atom                     ,
               'VALUE'-int                      ]).

ctr_exchangeable(arith_or,
                 [args([['VARIABLES1','VARIABLES2'],['RELOP'],['VALUE']]),
		  items_sync('VARIABLES1','VARIABLES2',all)]).

ctr_restrictions(arith_or,
                 [required('VARIABLES1',var)           ,
                  required('VARIABLES2',var)           ,
                  size('VARIABLES1')=size('VARIABLES2'),
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]) .

ctr_typical(arith_or,
            [size('VARIABLES1') > 0,
             in_list('RELOP',[=])  ]).

ctr_contractible(arith_or, [], ['VARIABLES1','VARIABLES2'], any).

ctr_graph(arith_or,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'(=)>>collection(variables1,variables2)],
          ['RELOP'(variables1^var,'VALUE') #\/ 'RELOP'(variables2^var,'VALUE')],
          ['NARC' = size('VARIABLES1')],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(arith_or,
            arith_or([[var-0],[var-1],[var-0],[var-0],[var-1]],
                     [[var-0],[var-0],[var-0],[var-1],[var-0]],=,0)).

ctr_draw_example(arith_or,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-0],[var-1],[var-0],[var-0],[var-1]],
                  [[var-0],[var-0],[var-0],[var-1],[var-0]]],
                 ['PRODUCT'(=)],
                 [1-1, 2-2, 3-3, 4-4, 5-5],
                 ['NARC'],
                 '','NARC=5',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(arith_or,
 [link(specialisation, arith, '%e %e %e $\\vee$ %e %e %e replaced by %e %e %e', [variable,'RELOP','VALUE',variable,'RELOP','VALUE',variable,'RELOP','VALUE'])]).

ctr_key_words(arith_or,['decomposition'                   ,
                        'value constraint'                ,
                        'disjunction'                     ,
                        'Berge-acyclic constraint network',
                        'automaton'                       ,
                        'automaton without counters'      ,
                        'reified automaton constraint'    ,
                        'acyclic'                         ,
                        'bipartite'                       ,
                        'no loop'                         ,
                        'arc-consistency'                 ]).

ctr_eval(arith_or, [reformulation(arith_or_r), automaton(arith_or_a)]).

arith_or_r(VARIABLES1, VARIABLES2, RELOP, VALUE) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    integer(VALUE),
    length(VARIABLES1, N1),
    length(VARIABLES2, N2),
    N1 = N2,
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    arith_or1(VARS1, VARS2, RELOP, VALUE).

arith_or1([], [], _, _).
arith_or1([VAR1|RVAR1], [VAR2|RVAR2], =, VALUE) :- !,
    VAR1 #= VALUE #\/ VAR2 #= VALUE,
    arith_or1(RVAR1, RVAR2, =, VALUE).
arith_or1([VAR1|RVAR1], [VAR2|RVAR2], =\=, VALUE) :- !,
    VAR1 #\= VALUE #\/ VAR2 #\= VALUE,
    arith_or1(RVAR1, RVAR2, =\=, VALUE).
arith_or1([VAR1|RVAR1], [VAR2|RVAR2], <, VALUE) :- !,
    VAR1 #< VALUE #\/ VAR2 #< VALUE,
    arith_or1(RVAR1, RVAR2, [VAR2|RVAR2], <, VALUE).
arith_or1([VAR1|RVAR1], [VAR2|RVAR2], >=, VALUE) :- !,
    VAR1 #>= VALUE #\/ VAR2 #>= VALUE,
    arith_or1(RVAR1, RVAR2, [VAR2|RVAR2], >=, VALUE).
arith_or1([VAR1|RVAR1], [VAR2|RVAR2], >, VALUE) :- !,
    VAR1 #> VALUE #\/ VAR2 #> VALUE,
    arith_or1(RVAR1, RVAR2, [VAR2|RVAR2], >, VALUE).
arith_or1([VAR1|RVAR1], [VAR2|RVAR2], =<, VALUE) :-
    VAR1 #=< VALUE #\/ VAR2 #=< VALUE,
    arith_or1(RVAR1, RVAR2, =<, VALUE).

% 0: VAR1 not RELOP VALUE and VAR2 not RELOP VALUE
% 1: VAR1     RELOP VALUE or  VAR2     RELOP VALUE
arith_or_a(FLAG, VARIABLES1, VARIABLES2, RELOP, VALUE) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    integer(VALUE),
    length(VARIABLES1, N1),
    length(VARIABLES2, N2),
    N1 = N2,
    arith_or_signature(VARIABLES1, VARIABLES2, SIGNATURE, RELOP, VALUE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,1,s)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

arith_or_signature([], [], [], _, _).
arith_or_signature([[var-VAR1]|VAR1s], [[var-VAR2]|VAR2s], [S|Ss], =  , VALUE) :- !,
        VAR1 #=  VALUE #\/ VAR2 #=  VALUE #<=> S,
        arith_or_signature(VAR1s, VAR2s, Ss, =, VALUE).
arith_or_signature([[var-VAR1]|VAR1s], [[var-VAR2]|VAR2s], [S|Ss], =\=, VALUE) :- !,
        VAR1 #\= VALUE #\/ VAR2 #\= VALUE #<=> S,
        arith_or_signature(VAR1s, VAR2s, Ss, =\=, VALUE).
arith_or_signature([[var-VAR1]|VAR1s], [[var-VAR2]|VAR2s], [S|Ss], <  , VALUE) :- !,
        VAR1 #<  VALUE #\/ VAR2 #<  VALUE #<=> S,
        arith_or_signature(VAR1s, VAR2s, Ss, <, VALUE).
arith_or_signature([[var-VAR1]|VAR1s], [[var-VAR2]|VAR2s], [S|Ss], >= , VALUE) :- !,
        VAR1 #>= VALUE #\/ VAR2 #>= VALUE #<=> S,
        arith_or_signature(VAR1s, VAR2s, Ss, >=, VALUE).
arith_or_signature([[var-VAR1]|VAR1s], [[var-VAR2]|VAR2s], [S|Ss], >  , VALUE) :- !,
        VAR1 #>  VALUE #\/ VAR2 #>  VALUE #<=> S,
        arith_or_signature(VAR1s, VAR2s, Ss, >, VALUE).
arith_or_signature([[var-VAR1]|VAR1s], [[var-VAR2]|VAR2s], [S|Ss], =< , VALUE) :-
        VAR1 #=< VALUE #\/ VAR2 #=< VALUE #<=> S,
        arith_or_signature(VAR1s, VAR2s, Ss, =<, VALUE).
