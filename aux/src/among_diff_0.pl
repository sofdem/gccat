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

:-dynamic among_diff_0_a/2.

ctr_date(among_diff_0,['20040807','20060804']).

ctr_origin(among_diff_0, 'Used in the automaton of %c.', [nvalue]).

ctr_arguments(among_diff_0,
              ['NVAR'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(among_diff_0,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int(=\=(0)),=\=,dontcare,dontcare)]).

ctr_restrictions(among_diff_0,
                 ['NVAR' >= 0                ,
                  'NVAR' =< size('VARIABLES'),
                  required('VARIABLES',var)  ]).

ctr_typical(among_diff_0,
            ['NVAR' > 0                                          ,
             'NVAR' < size('VARIABLES')                          ,
              size('VARIABLES') > 1                              ,
              atleast(1,'VARIABLES',0)                           ,
              2*among_diff_0('VARIABLES'^var) > size('VARIABLES')]).

ctr_pure_functional_dependency(among_diff_0, []).
ctr_functional_dependency(among_diff_0, 1, [2]).

ctr_contractible(among_diff_0, ['NVAR'=0], 'VARIABLES', any).
ctr_contractible(among_diff_0, ['NVAR'=size('VARIABLES')], 'VARIABLES', any).

% among_diff_0('NVAR1', 'VARIABLES1') and
% among_diff_0('NVAR2', 'VARIABLES2') =>
% among_diff_0('VAR1'+'VAR2', union('VARIABLES1','VARIABLES2'))
ctr_aggregate(among_diff_0, [], [+, union]).

ctr_graph(among_diff_0,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var =\= 0],
          ['NARC' = 'NVAR'],
	  []).

ctr_example(among_diff_0,
            [among_diff_0(3, [[var-0],[var-5],[var-5],[var-0],[var-1]]),
	     among_diff_0(0, [[var-0],[var-0],[var-0],[var-0],[var-0]]),
	     among_diff_0(1, [[var-0],[var-0],[var-0],[var-6],[var-0]])]).

ctr_draw_example(among_diff_0,
                 ['VARIABLES'],
                 [[[var-0],[var-5],[var-5],[var-0],[var-1]]],
                 ['SELF'],
                 [2-2,3-3,5-5],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,0.53625]).

ctr_see_also(among_diff_0,
 [link(generalisation  , among,  '%e replaced by %e', [variable #\= 0,in_list(variable,values)]),
  link('common keyword', nvalue, '%k',                ['counting constraint'])]).

ctr_key_words(among_diff_0,['value constraint'                   ,
                            'counting constraint'                ,
                            'joker value'                        ,
                            'automaton'                          ,
                            'automaton with counters'            ,
                            'alpha-acyclic constraint network(2)',
                            'arc-consistency'                    ,
                            'functional dependency'              ,
		            'pure functional dependency'         ]).

ctr_eval(among_diff_0, [      checker(among_diff_0_c),
			reformulation(among_diff_0_r),
			    automaton(among_diff_0_a)]).

ctr_sol(among_diff_0,2,0,2,9,[0-1,1-4,2-4]).
ctr_sol(among_diff_0,3,0,3,64,[0-1,1-9,2-27,3-27]).
ctr_sol(among_diff_0,4,0,4,625,[0-1,1-16,2-96,3-256,4-256]).
ctr_sol(among_diff_0,5,0,5,7776,[0-1,1-25,2-250,3-1250,4-3125,5-3125]).
ctr_sol(among_diff_0,6,0,6,117649,[0-1,1-36,2-540,3-4320,4-19440,5-46656,6-46656]).
ctr_sol(among_diff_0,7,0,7,2097152,[0-1,1-49,2-1029,3-12005,4-84035,5-352947,6-823543,7-823543]).
ctr_sol(among_diff_0,8,0,8,43046721,[0-1,1-64,2-1792,3-28672,4-286720,5-1835008,6-7340032,7-16777216,8-16777216]).

among_diff_0_c(NVAR, VARIABLES) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    length(VARS, N),
    NVAR #>= 0,
    NVAR #=< N,
    among_diff_0_c(VARS, 0, NVAR).

among_diff_0_c([V|R], C, NVAR) :-
    !,
    (V = 0 ->
	among_diff_0_c(R, C, NVAR)
    ;
	C1 is C+1,
	among_diff_0_c(R, C1, NVAR)
    ).
among_diff_0_c([], NVAR, NVAR).

among_diff_0_counters_check([V|R], C, [D|S]) :-
    !,
    (V = 0 -> D = C ; D is C+1),
    among_diff_0_counters_check(R, D, S).
among_diff_0_counters_check([], _, []).

among_diff_0_r(NVAR, VARIABLES) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    NVAR #>= 0,
    NVAR #=< N,
    among_diff_01(VARS, SUM_BVARS),
    call(NVAR #= SUM_BVARS).

among_diff_01([], 0).
among_diff_01([V|R], B+S) :-
    V #\= 0 #<=> B,
    among_diff_01(R, S).

% 0: VAR = 0
% 1: VAR=\=0
among_diff_0_a(FLAG, NVAR, VARIABLES) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    NVAR #>= 0,
    NVAR #=< N,
    among_diff_0_signature(VARIABLES, SIGNATURE),
    automaton(SIGNATURE, _,
              SIGNATURE,
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NVAR #<=> FLAG.

among_diff_0_signature([], []).
among_diff_0_signature([[var-VAR]|VARs], [S|Ss]) :-
        VAR #\= 0 #<=> S,
        among_diff_0_signature(VARs, Ss).
