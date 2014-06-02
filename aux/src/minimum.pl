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

ctr_date(minimum,['20000128','20030820','20040530','20041230','20060811','20090416']).

ctr_origin(minimum, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(minimum,
              ['MIN'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(minimum,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,in),
                  translate(['MIN','VARIABLES'^var])]).

ctr_synonyms(minimum,[min]).

ctr_restrictions(minimum,
                 [size('VARIABLES') > 0    ,
                  required('VARIABLES',var)]).

ctr_typical(minimum,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_pure_functional_dependency(minimum, []).
ctr_functional_dependency(minimum, 1, [2]).

% minimum('MIN1', 'VARIABLES1') and
% minimum('MIN2', 'VARIABLES2') =>
% minimum(min('MIN1','MIN2'), union('VARIABLES1','VARIABLES2'))
ctr_aggregate(minimum, [], [min, union]).

ctr_graph(minimum,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^key = variables2^key #\/ variables1^var < variables2^var],
          ['ORDER'(0,'MAXINT',var) = 'MIN'],
          []).

ctr_example(minimum,
            [minimum(2,[[var-3],[var-2],[var-7],[var-2],[var-6]]),
	     minimum(7,[[var-8],[var-8],[var-7],[var-8],[var-7]])]).

ctr_draw_example(minimum,
                 ['VARIABLES'],
                 [[[var-3],[var-2],[var-7],[var-2],[var-6]]],
                 ['CLIQUE'],
                 [1-[1,3,5],
                  2-[1,2,3,5],
                  3-3,
                  4-[1,3,4,5],
                  5-[3,5]],
                 ['ORDER'([2,4])],
                 '','ORDER(0,MAXINT,var)=2',
                 []).

ctr_cond_imply(minimum, deepest_valley, [first('VARIABLES'^var) > 'MIN', last('VARIABLES'^var) > 'MIN'], [], id).

ctr_see_also(minimum,
 [link('generalisation',            minimum_modulo,   '%e replaced by %e',                                [variable, variable mod constant]),
  link('specialisation',            min_n,            'minimum or order %e replaced by absolute minimum', [n]),
  link('comparison swapped',        maximum,          '',                                                 []),
  link('common keyword',            maximum,          '%k',                                               ['order constraint']),
  link('soft variant',              open_minimum,     '%k',                                               ['open constraint']),
  link('soft variant',              minimum_except_0, 'value %e is ignored',                              [0]),
  link('implies',                   between_min_max,  '',                                                 []),
  link('implies',                   in,               '',                                                 []),
  link('implied by',                and,              '',                                                 []),
  link('uses in its reformulation', cycle,            '',                                                 [])]).

ctr_key_words(minimum,['order constraint'                        ,
                       'minimum'                                 ,
                       'maxint'                                  ,
                       'automaton'                               ,
                       'automaton without counters'              ,
                       'reified automaton constraint'            ,
                       'reverse of a constraint'                 ,
		       'glue matrix'                             ,
                       'centered cyclic(1) constraint network(1)',
                       'arc-consistency'                         ,
                       'functional dependency'                   ,
		       'pure functional dependency'              ,
                       'entailment'                              ]).

ctr_persons(minimum,['Beldiceanu N.']).

ctr_eval(minimum, [builtin(minimum_b),
                   automaton(minimum_a),
		   automaton(minimum_ca)]).

ctr_sol(minimum,2,0,2,9,[0-5,1-3,2-1]).
ctr_sol(minimum,3,0,3,64,[0-37,1-19,2-7,3-1]).
ctr_sol(minimum,4,0,4,625,[0-369,1-175,2-65,3-15,4-1]).
ctr_sol(minimum,5,0,5,7776,[0-4651,1-2101,2-781,3-211,4-31,5-1]).
ctr_sol(minimum,6,0,6,117649,[0-70993,1-31031,2-11529,3-3367,4-665,5-63,6-1]).
ctr_sol(minimum,7,0,7,2097152,[0-1273609,1-543607,2-201811,3-61741,4-14197,5-2059,6-127,7-1]).
ctr_sol(minimum,8,0,8,43046721,[0-26269505,1-11012415,2-4085185,3-1288991,4-325089,5-58975,6-6305,7-255,8-1]).

minimum_b(MIN, VARIABLES) :-
    check_type(dvar, MIN),
    collection(VARIABLES, [dvar]),
    VARIABLES = [_|_],
    get_attr1(VARIABLES, VARS),
    minimum(MIN, VARS).

% 0: MIN<VAR
% 1: MIN=VAR
% 2: MIN>VAR
minimum_a(FLAG, MIN, VARIABLES) :-
    check_type(dvar, MIN),
    collection(VARIABLES, [dvar]),
    VARIABLES = [_|_],
    minimum_signature(VARIABLES, SIGNATURE, MIN),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,1,t),
                           arc(t,0,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1,2], AUTOMATON).

minimum_signature([], [], _).
minimum_signature([[var-VAR]|VARs], [S|Ss], MIN) :-
    S in 0..2,
    MIN #< VAR #<=> S #= 0,
    MIN #= VAR #<=> S #= 1,
    MIN #> VAR #<=> S #= 2,
    minimum_signature(VARs, Ss, MIN).

minimum_ca(FLAG, MIN, VARIABLES) :-
    check_type(dvar, MIN),
    collection(VARIABLES, [dvar]),
    maximum_signature1(VARIABLES, VARS, Zeros),
    VARS = [VAR1|_],
    automaton(VARS, VARi, Zeros, 
              [source(s),sink(s)],
              [arc(s,0,s,[min(C,VARi)])],
              [C],[VAR1],[CC]),
    CC #= MIN #<=> FLAG.

minimum_signature1([], [], []).
minimum_signature1([[var-VAR]|VARs], [VAR|R], [0|S]) :-
    minimum_signature1(VARs, R, S).
