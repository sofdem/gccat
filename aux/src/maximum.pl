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

ctr_date(maximum,['20000128','20030820','20040530','20041230','20060811','20090416']).

ctr_origin(maximum, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(maximum,
              ['MAX'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(maximum,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  translate(['MAX','VARIABLES'^var])]).

ctr_synonyms(maximum,[max]).

ctr_restrictions(maximum,
                 [size('VARIABLES') > 0    ,
                  required('VARIABLES',var)]).

ctr_typical(maximum,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_pure_functional_dependency(maximum, []).
ctr_functional_dependency(maximum, 1, [2]).

% maximum('MAX1', 'VARIABLES1') and
% maximum('MAX2', 'VARIABLES2') =>
% maximum(max('MAX1','MAX2'), union('VARIABLES1','VARIABLES2'))
ctr_aggregate(maximum, [], [max, union]).

ctr_graph(maximum,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [(variables1^key = variables2^key #\/ variables1^var > variables2^var)],
          ['ORDER'(0,'MININT',var) = 'MAX'],
          []).

ctr_example(maximum,
            [maximum(7,[[var-3],[var-2],[var-7],[var-2],[var-6]]),
	     maximum(1,[[var-0],[var-0],[var-1],[var-0],[var-1]])]).

ctr_draw_example(maximum,
                 ['VARIABLES'],
                 [[[var-3],[var-2],[var-7],[var-2],[var-6]]],
                 ['CLIQUE'],
                 [1-[1,2,4],
                  2-2,
                  3-[1,2,3,4,5],
                  4-4,
                  5-[1,2,4,5]],
                 ['ORDER'([3])],
                 '','ORDER(0,MININT,var)=7',
                 []).

ctr_cond_imply(maximum, highest_peak, [first('VARIABLES'^var) < 'MAX', last('VARIABLES'^var) < 'MAX'], [], id).

ctr_see_also(maximum,
 [link('generalisation',            maximum_modulo,  '%e replaced by %e',                                [variable, variable mod constant]),
  link('specialisation',            max_n,           'maximum or order %e replaced by absolute maximum', [n]),
  link('comparison swapped',        minimum,         '',                                                 []),
  link('common keyword',            minimum,         '%k',                                               ['order constraint']),
  link('soft variant',              open_maximum,    '%k',                                               ['open constraint']),
  link('implies',                   between_min_max, '',                                                 []),
  link('implies',                   in,              '',                                                 []),
  link('implied by',                or,              '',                                                 []),
  link('uses in its reformulation', tree_range,      '',                                                 [])]).

ctr_key_words(maximum,['order constraint'                        ,
                       'maximum'                                 ,
                       'automaton'                               ,
                       'automaton without counters'              ,
                       'reified automaton constraint'            ,
                       'centered cyclic(1) constraint network(1)',
                       'reverse of a constraint'                 ,
		       'glue matrix'                             ,
                       'arc-consistency'                         ,
                       'balanced assignment'                     ,
                       'functional dependency'                   ,
		       'pure functional dependency'              ,
                       'entailment'                              ]).

ctr_persons(maximum,['Beldiceanu N.']).

ctr_eval(maximum, [builtin(maximum_b),
                   automaton(maximum_a),
		   automaton(maximum_ca)]).

ctr_sol(maximum,2,0,2,9,[0-1,1-3,2-5]).
ctr_sol(maximum,3,0,3,64,[0-1,1-7,2-19,3-37]).
ctr_sol(maximum,4,0,4,625,[0-1,1-15,2-65,3-175,4-369]).
ctr_sol(maximum,5,0,5,7776,[0-1,1-31,2-211,3-781,4-2101,5-4651]).
ctr_sol(maximum,6,0,6,117649,[0-1,1-63,2-665,3-3367,4-11529,5-31031,6-70993]).
ctr_sol(maximum,7,0,7,2097152,[0-1,1-127,2-2059,3-14197,4-61741,5-201811,6-543607,7-1273609]).
ctr_sol(maximum,8,0,8,43046721,[0-1,1-255,2-6305,3-58975,4-325089,5-1288991,6-4085185,7-11012415,8-26269505]).

maximum_b(MAX, VARIABLES) :-
    check_type(dvar, MAX),
    collection(VARIABLES, [dvar]),
    VARIABLES = [_|_],
    get_attr1(VARIABLES, VARS),
    maximum(MAX, VARS).

% 0: MAX>VAR
% 1: MAX=VAR
% 2: MAX<VAR
maximum_a(FLAG, MAX, VARIABLES) :-
    check_type(dvar, MAX),
    collection(VARIABLES, [dvar]),
    VARIABLES = [_|_],
    maximum_signature(VARIABLES, SIGNATURE, MAX),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,1,t),
                           arc(t,0,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

maximum_signature([], [], _).
maximum_signature([[var-VAR]|VARs], [S|Ss], MAX) :-
    S in 0..2,
    MAX #> VAR #<=> S #= 0,
    MAX #= VAR #<=> S #= 1,
    MAX #< VAR #<=> S #= 2,
    maximum_signature(VARs, Ss, MAX).

maximum_ca(FLAG, MAX, VARIABLES) :-
    check_type(dvar, MAX),
    collection(VARIABLES, [dvar]),
    maximum_signature1(VARIABLES, VARS, Zeros),
    VARS = [VAR1|_],
    automaton(VARS, VARi, Zeros, 
              [source(s),sink(s)],
              [arc(s,0,s,[max(C,VARi)])],
              [C],[VAR1],[CC]),
    CC #= MAX #<=> FLAG.

maximum_signature1([], [], []).
maximum_signature1([[var-VAR]|VARs], [VAR|R], [0|S]) :-
    maximum_signature1(VARs, R, S).
