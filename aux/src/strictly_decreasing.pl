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

ctr_date(strictly_decreasing,['20040814','20060817']).

ctr_origin(strictly_decreasing, 'Derived from %c.', [strictly_increasing]).

ctr_arguments(strictly_decreasing,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(strictly_decreasing,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(strictly_decreasing,
                 [required('VARIABLES',var)]).

ctr_typical(strictly_decreasing,
            [size('VARIABLES') > 2]).

ctr_contractible(strictly_decreasing, [], 'VARIABLES', any).

ctr_graph(strictly_decreasing,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables1^var > variables2^var],
          ['NARC' = size('VARIABLES')-1],
          []).

ctr_example(strictly_decreasing,
            strictly_decreasing([[var-8],[var-4],[var-3],[var-1]])).

ctr_draw_example(strictly_decreasing,
                 ['VARIABLES'],
                 [[[var-8],[var-4],[var-3],[var-1]]],
                 ['PATH'],
                 [1-2,2-3,3-4],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,2]).

ctr_see_also(strictly_decreasing,
 [link('implies',            alldifferent,        '',   []),
  link('implies',            decreasing,          '',   []),
  link('comparison swapped', strictly_increasing, '',   []),
  link('common keyword',     increasing,          '%k', ['order constraint'])]).

ctr_key_words(strictly_decreasing,['decomposition'                          ,
                                   'order constraint'                       ,
                                   'automaton'                              ,
                                   'automaton without counters'             ,
                                   'reified automaton constraint'           ,
                                   'sliding cyclic(1) constraint network(1)',
                                   'arc-consistency'                        ]).

ctr_eval(strictly_decreasing, [  checker(strictly_decreasing_c),
			       automaton(strictly_decreasing_a)]).

ctr_sol(strictly_decreasing,2,0,2,3,-).
ctr_sol(strictly_decreasing,3,0,3,4,-).
ctr_sol(strictly_decreasing,4,0,4,5,-).
ctr_sol(strictly_decreasing,5,0,5,6,-).
ctr_sol(strictly_decreasing,6,0,6,7,-).
ctr_sol(strictly_decreasing,7,0,7,8,-).
ctr_sol(strictly_decreasing,8,0,8,9,-).
ctr_sol(strictly_decreasing,9,0,9,10,-).
ctr_sol(strictly_decreasing,10,0,10,11,-).

strictly_decreasing_c([[var-X],[var-Y]|_]) :-    
    X =< Y,
    !,
    fail.
strictly_decreasing_c([]) :- !.
strictly_decreasing_c(VARIABLES) :-
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    strictly_decreasing_c1(VARS).

strictly_decreasing_c1([X,Y|R]) :-
    !,
    X > Y,
    strictly_decreasing_c1([Y|R]).
strictly_decreasing_c1([_]) :- !.
strictly_decreasing_c1([]).

% 0: VAR1> VAR2
% 1: VAR1=<VAR2
strictly_decreasing_a(1, []) :- !.
strictly_decreasing_a(0, []) :- !, fail.
strictly_decreasing_a(FLAG, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    strictly_decreasing_signature(VARIABLES, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,0,s)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

strictly_decreasing_signature([_], []) :- !.
strictly_decreasing_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss]) :-
    S in 0..1,
    VAR1 #=< VAR2 #<=> S,
    strictly_decreasing_signature([[var-VAR2]|VARs], Ss).
