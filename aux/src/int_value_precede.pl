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

ctr_date(int_value_precede,['20041003']).

ctr_origin(int_value_precede, '\\cite{YatChiuLawJimmyLee04}', []).

ctr_arguments(int_value_precede,
              ['S'-int                         ,
               'T'-int                         ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(int_value_precede,
                 [vals(['VARIABLES'^var],int(notin(['S','T'])),=\=,dontcare,dontcare),
                  vals(['S','T','VARIABLES'^var],int(['S','T']),=\=,all,in)]).

ctr_synonyms(int_value_precede,[precede      ,
				precedence   ,
				value_precede]).

ctr_restrictions(int_value_precede,
                 ['S' =\= 'T'              ,
                  required('VARIABLES',var)]).

ctr_typical(int_value_precede,
            ['S' < 'T'                 ,
             size('VARIABLES') > 1     ,
             atleast(1,'VARIABLES','S'),
             atleast(1,'VARIABLES','T')]).

ctr_contractible(int_value_precede, [], 'VARIABLES', suffix).

% int_value_precede('S', 'T', 'VARIABLES1') and
% int_value_precede('S', 'T', 'VARIABLES2') =>
% int_value_precede('S', 'T', union('VARIABLES1','VARIABLES2'))
ctr_aggregate(int_value_precede, [], [id, id, union]).

ctr_example(int_value_precede,
            int_value_precede(0, 1, [[var-4], [var-0], [var-6], [var-1], [var-0]])).

ctr_see_also(int_value_precede,
 [link(generalisation, int_value_precede_chain, '%e of %e %e replaced by %e of at least %e %e', [sequence, 2, values, sequence, 2, values]),
  link(generalisation, set_value_precede,       '%e of %e replaced by %e of %e %e',             [sequence, 'domain~variables', sequence, set, variables])]).

ctr_key_words(int_value_precede,['order constraint'                ,
                                 'symmetry'                        ,
                                 'indistinguishable values'        ,
                                 'value precedence'                ,
                                 'Berge-acyclic constraint network',
                                 'automaton'                       ,
                                 'automaton without counters'      ,
                                 'reified automaton constraint'    ,
                                 'arc-consistency'                 ]).

ctr_persons(int_value_precede,['Law Y. C.'   ,
                               'Lee J. H. M.']).

ctr_eval(int_value_precede, [automaton(int_value_precede_a)]).

% 1: VAR = S
% 2: VAR = T
% 3: VAR =\= S and VAR =\= T
int_value_precede_a(1, S, T, []) :- !,
    check_type(int, S),
    check_type(int, T),
    S =\= T.
int_value_precede_a(0, _S, _T, []) :- !,
    fail.
int_value_precede_a(FLAG, S, T, VARIABLES) :-
    check_type(int, S),
    check_type(int, T),
    S =\= T,
    collection(VARIABLES, [dvar]),
    int_value_precede_signature(VARIABLES, SIGNATURE, S, T),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s),sink(t)],
                          [arc(s,3,s),
                           arc(s,1,t),
                           arc(t,1,t),
                           arc(t,2,t),
                           arc(t,3,t)],
                          [], [], []),
    automaton_bool(FLAG, [1,2,3], AUTOMATON).

int_value_precede_signature([], [], _, _).
int_value_precede_signature([[var-VAR]|VARs], [SI|SIs], S, T) :-
    SI in 1..3,
    VAR #=  S               #<=> SI #= 1,
    VAR #=  T               #<=> SI #= 2,
    VAR #\= S #/\ VAR #\= T #<=> SI #= 3,
    int_value_precede_signature(VARs, SIs, S, T).
