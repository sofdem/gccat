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

ctr_date(arith,['20040814','20060804']).

ctr_origin(arith, 'Used in the definition of several automata', []).

ctr_arguments(arith,
              ['VARIABLES'-collection(var-dvar),
               'RELOP'-atom                    ,
               'VALUE'-int                     ]).

ctr_exchangeable(arith,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,dontcare,in)]).

ctr_synonyms(arith,[rel]).

ctr_restrictions(arith,
                 [required('VARIABLES',var)         ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(arith,
            [size('VARIABLES') > 1,
             in_list('RELOP',[=]) ]).

ctr_contractible(arith, [], 'VARIABLES', any).

ctr_graph(arith,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          ['RELOP'(variables^var,'VALUE')],
          ['NARC' = size('VARIABLES')],
	  []).

ctr_example(arith,
            arith([[var-4],[var-5],[var-7],[var-4],[var-5]],<,9)).

ctr_draw_example(arith,
                 ['VARIABLES'],
                 [[[var-4],[var-5],[var-7],[var-4],[var-5]]],
                 ['SELF'],
                 [1-1, 2-2, 3-3, 4-4, 5-5],
                 ['NARC'],
                 '','NARC=5',
                 [2.145,2.145,2.145,0.53625]).

ctr_cond_imply(arith, range_ctr, [in_list('RELOP',[<]), minval('VARIABLES'^var) >= 0], [in_list('CTR',[<])], id). 

ctr_see_also(arith,
 [link('system of constraints', arith_sliding, '',                                               []),
  link(generalisation,          arith_or,      '%e %e %e replaced by %e %e %e $\\vee$ %e %e %e', [variable, 'RELOP', 'VALUE', variable, 'RELOP', 'VALUE', variable, 'RELOP', 'VALUE']),
  link('common keyword',        among,         '%k',                                             ['value constraint']),
  link('common keyword',        count,         '%k',                                             ['value constraint'])]).

ctr_key_words(arith,['decomposition'                   ,
                     'value constraint'                ,
                     'domain definition'               ,
                     'Berge-acyclic constraint network',
                     'automaton'                       ,
                     'automaton without counters'      ,
                     'reified automaton constraint'    ,
                     'arc-consistency'                 ]).

ctr_eval(arith, [reformulation(arith_r), automaton(arith_a)]).

arith_r(VARIABLES, RELOP, VALUE) :-
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    integer(VALUE),
    get_attr1(VARIABLES, VARS),
    arith1(VARS, RELOP, VALUE).

arith1([], _, _).
arith1([VAR|RVARS], RELOP, VALUE) :-
    call_term_relop_value(VAR, RELOP, VALUE),
    arith1(RVARS, RELOP, VALUE).

% 0: VAR not RELOP VALUE
% 1: VAR     RELOP VALUE
arith_a(FLAG, VARIABLES, RELOP, VALUE) :-
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    integer(VALUE),
    arith_signature(VARIABLES, SIGNATURE, RELOP, VALUE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,1,s)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

arith_signature([], [], _, _).
arith_signature([[var-VAR]|VARs], [S|Ss], =, VALUE) :- !,
        VAR #= VALUE #<=> S,
        arith_signature(VARs, Ss, =, VALUE).
arith_signature([[var-VAR]|VARs], [S|Ss], =\=, VALUE) :- !,
        VAR #\= VALUE #<=> S,
        arith_signature(VARs, Ss, =\=, VALUE).
arith_signature([[var-VAR]|VARs], [S|Ss], <, VALUE) :- !,
        VAR #< VALUE #<=> S,
        arith_signature(VARs, Ss, <, VALUE).
arith_signature([[var-VAR]|VARs], [S|Ss], >=, VALUE) :- !,
        VAR #>= VALUE #<=> S,
        arith_signature(VARs, Ss, >=, VALUE).
arith_signature([[var-VAR]|VARs], [S|Ss], >, VALUE) :- !,
        VAR #> VALUE #<=> S,
        arith_signature(VARs, Ss, >, VALUE).
arith_signature([[var-VAR]|VARs], [S|Ss], =<, VALUE) :-
        VAR #=< VALUE #<=> S,
        arith_signature(VARs, Ss, =<, VALUE).
