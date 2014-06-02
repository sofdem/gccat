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

:-dynamic arith_sliding_a/3.

ctr_date(arith_sliding,['20040814']).

ctr_origin(arith_sliding, 'Used in the definition of some automaton', []).

ctr_arguments(arith_sliding,
              ['VARIABLES'-collection(var-dvar),
               'RELOP'-atom                    ,
               'VALUE'-int                     ]).

ctr_restrictions(arith_sliding,
                 [required('VARIABLES',var)         ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(arith_sliding,
            [size('VARIABLES') > 1       ,
             in_list('RELOP',[<,>=,>,=<])]).

ctr_contractible(arith_sliding, [in_list('RELOP',[<,=<]),minval('VARIABLES'^var)>=0], 'VARIABLES', any).
ctr_contractible(arith_sliding, [], 'VARIABLES', suffix).

ctr_graph(arith_sliding,
          ['VARIABLES'],
          *,
          ['PATH_1'>>collection],
          [arith(collection,'RELOP','VALUE')],
          ['NARC' = size('VARIABLES')],
	  []).

ctr_example(arith_sliding,
            arith_sliding([[var-0],[var-0],[var-1],[var-2],[var-0],[var-0],[var-(-3)]],<,4)).

ctr_see_also(arith_sliding,
 [link('implies',                       sum_ctr, '',   []),
  link('part of system of constraints', arith,   '',   []),
  link('used in graph description',     arith,   '',   []),
  link('common keyword',                sum_ctr, '%k', ['arithmetic constraint'])]).

ctr_key_words(arith_sliding,['arithmetic constraint'      ,
                             'decomposition'              ,
                             'sliding sequence constraint',
                             'sequence'                   ,
                             'hypergraph'                 ,
                             'automaton'                  ,
                             'automaton with counters'    ]).

ctr_eval(arith_sliding, [reformulation(arith_sliding_r), automaton(arith_sliding_a)]).

arith_sliding_r(VARIABLES, RELOP, VALUE) :-
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    integer(VALUE),
    get_attr1(VARIABLES, VARS),
    reverse(VARS, RVARS),
    arith_sliding1(RVARS, RELOP, VALUE).

arith_sliding1([], _, _).
arith_sliding1([VAR|RVARS], RELOP, VALUE) :-
    arith_sliding2([VAR|RVARS], SUM),
    call_term_relop_value(SUM, RELOP, VALUE),
    arith_sliding1(RVARS, RELOP, VALUE).    

arith_sliding2([], 0).
arith_sliding2([VAR|RVARS], VAR+R) :-
    arith_sliding2(RVARS, R).

% 0: VAR
arith_sliding_a(FLAG, VARIABLES, =, VALUE) :- !,
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, N),
    length(SIGNATURE, N),
    domain(SIGNATURE, 0, 0),
    arith_sliding_signature(VARIABLES, VARS, SIGNATURE),
    automaton(VARS, VAR, SIGNATURE,
              [source(s),sink(s),sink(t)],
              [arc(s,0,t,                [T,C+VAR]),
               arc(t,0,t,(C #= VALUE ->  [T,C+VAR])),
               arc(t,0,t,(C #\= VALUE -> [0,C+VAR]))],
              [T,C],[1,0],[T1,C1]),
    T1 #= 1 #/\ C1 #= VALUE #<=> FLAG.

arith_sliding_a(FLAG, VARIABLES, =\=, VALUE) :- !,
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, N),
    length(SIGNATURE, N),
    domain(SIGNATURE, 0, 0),
    arith_sliding_signature(VARIABLES, VARS, SIGNATURE),
    automaton(VARS, VAR, SIGNATURE,
              [source(s),sink(s),sink(t)],
              [arc(s,0,t,                [T,C+VAR]),
               arc(t,0,t,(C #\= VALUE -> [T,C+VAR])),
               arc(t,0,t,(C #= VALUE ->  [0,C+VAR]))],
              [T,C],[1,0],[T1,C1]),
    T1 #= 1 #/\ C1 #\= VALUE #<=> FLAG.

arith_sliding_a(FLAG, VARIABLES, <, VALUE) :- !,
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, N),
    length(SIGNATURE, N),
    domain(SIGNATURE, 0, 0),
    arith_sliding_signature(VARIABLES, VARS, SIGNATURE),
    automaton(VARS, VAR, SIGNATURE,
              [source(s),sink(s),sink(t)],
              [arc(s,0,t,                [T,C+VAR]),
               arc(t,0,t,(C #< VALUE ->  [T,C+VAR])),
               arc(t,0,t,(C #>= VALUE -> [0,C+VAR]))],
              [T,C],[1,0],[T1,C1]),
    T1 #= 1 #/\ C1 #< VALUE #<=> FLAG.

arith_sliding_a(FLAG, VARIABLES, >=, VALUE) :- !,
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, N),
    length(SIGNATURE, N),
    domain(SIGNATURE, 0, 0),
    arith_sliding_signature(VARIABLES, VARS, SIGNATURE),
    automaton(VARS, VAR, SIGNATURE,
              [source(s),sink(s),sink(t)],
              [arc(s,0,t,                [T,C+VAR]),
               arc(t,0,t,(C #>= VALUE -> [T,C+VAR])),
               arc(t,0,t,(C #< VALUE ->  [0,C+VAR]))],
              [T,C],[1,0],[T1,C1]),
    T1 #= 1 #/\ C1 #>= VALUE #<=> FLAG.

arith_sliding_a(FLAG, VARIABLES, >, VALUE) :- !,
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, N),
    length(SIGNATURE, N),
    domain(SIGNATURE, 0, 0),
    arith_sliding_signature(VARIABLES, VARS, SIGNATURE),
    automaton(VARS, VAR, SIGNATURE,
              [source(s),sink(s),sink(t)],
              [arc(s,0,t,                [T,C+VAR]),
               arc(t,0,t,(C #> VALUE ->  [T,C+VAR])),
               arc(t,0,t,(C #=< VALUE -> [0,C+VAR]))],
              [T,C],[1,0],[T1,C1]),
    T1 #= 1 #/\ C1 #> VALUE #<=> FLAG.

arith_sliding_a(FLAG, VARIABLES, =<, VALUE) :-
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, N),
    length(SIGNATURE, N),
    domain(SIGNATURE, 0, 0),
    arith_sliding_signature(VARIABLES, VARS, SIGNATURE),
    automaton(VARS, VAR, SIGNATURE,
              [source(s),sink(s),sink(t)],
              [arc(s,0,t,                [T,C+VAR]),
               arc(t,0,t,(C #=< VALUE -> [T,C+VAR])),
               arc(t,0,t,(C #>  VALUE -> [0,C+VAR]))],
              [T,C],[1,0],[T1,C1]),
    T1 #= 1 #/\ C1 #=< VALUE #<=> FLAG.

arith_sliding_signature([], [], []).
arith_sliding_signature([[var-V]|VARs], [V|Vs], [0|Ss]) :-
        arith_sliding_signature(VARs, Vs, Ss).
