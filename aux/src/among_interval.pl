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

:-dynamic among_interval_a/4.

ctr_date(among_interval,['20030820','20040530','20060804']).

ctr_origin(among_interval, 'Derived from %c.', [among]).

ctr_arguments(among_interval,
              ['NVAR'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'LOW'-int                       ,
               'UP'-int                        ]).

ctr_exchangeable(among_interval,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],comp(in('LOW','UP')),=,dontcare,dontcare)]).

ctr_restrictions(among_interval,
                 ['NVAR' >= 0                ,
                  'NVAR' =< size('VARIABLES'),
                  required('VARIABLES',var)  ,
                  'LOW' =< 'UP'              ]).

ctr_typical(among_interval,
            ['NVAR' > 0                      ,
             'NVAR' < size('VARIABLES')      ,
             size('VARIABLES') > 1           ,
             'LOW' < 'UP'                    ,
             'LOW' =< maxval('VARIABLES'^var),
             'UP'  >= minval('VARIABLES'^var)]).

ctr_pure_functional_dependency(among_interval, []).
ctr_functional_dependency(among_interval, 1, [2,3,4]).

ctr_contractible(among_interval, ['NVAR'=0], 'VARIABLES', any).
ctr_contractible(among_interval, ['NVAR'=size('VARIABLES')], 'VARIABLES', any).

% among_interval('NVAR1', 'VARIABLES1', 'LOW', 'UP') and
% among_interval('NVAR2', 'VARIABLES2', 'LOW', 'UP') =>
% among_interval('VAR1'+'VAR2', union('VARIABLES1','VARIABLES2'), 'LOW', 'UP')
ctr_aggregate(among_interval, [], [+, union, id, id]).

ctr_graph(among_interval,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          ['LOW' =< variables^var,
           variables^var =< 'UP'],
          ['NARC' = 'NVAR'],
	  []).

ctr_example(among_interval,
            among_interval(3,
                           [[var-4],[var-5],[var-8],[var-4],[var-1]],
                           3,
                           5)).

ctr_draw_example(among_interval,
                 ['VARIABLES'],
                 [[[var-4],[var-5],[var-8],[var-4],[var-1]]],
                 ['SELF'],
                 [1-1,2-2,4-4],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,0.53625]).

ctr_see_also(among_interval,
 [link(generalisation, among, '%e in %e replaced by %e', [variable,interval,in_list(variable,values)])]).

ctr_key_words(among_interval,['value constraint'                   ,
                              'counting constraint'                ,
                              'interval'                           ,
                              'automaton'                          ,
                              'automaton with counters'            ,
                              'alpha-acyclic constraint network(2)',
                              'arc-consistency'                    ,
                              'functional dependency'              ,
		              'pure functional dependency'         ]).

ctr_eval(among_interval, [reformulation(among_interval_r), automaton(among_interval_a)]).

among_interval_r(NVAR, VARIABLES, LOW, UP) :-
	check_type(dvar, NVAR),
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	integer(LOW),
	integer(UP),
	length(VARIABLES, N),
	NVAR #>= 0,
	NVAR #=< N,
	LOW =< UP,
      among_interval1(VARS, SUM_BVARS, LOW, UP),
	call(NVAR #= SUM_BVARS).

among_interval1([], 0, _, _).
among_interval1([V|R], B+S, LOW, UP) :-
    V #>= LOW #/\ V #=< UP #<=> B,
    among_interval1(R, S, LOW, UP).

% 0: LOW >  VAR  or   VAR >  UP
% 1: LOW =< VAR  and  VAR =< UP
among_interval_a(FLAG, NVAR, VARIABLES, LOW, UP) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [dvar]),
    integer(LOW),
    integer(UP),
    length(VARIABLES, N),
    NVAR #>= 0,
    NVAR #=< N,
    LOW =< UP,
    among_interval_signature(VARIABLES, SIGNATURE, LOW, UP),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NVAR #<=> FLAG.

among_interval_signature([], [], _, _).
among_interval_signature([[var-VAR]|VARs], [S|Ss], LOW, UP) :-
    LOW #=< VAR #/\ VAR #=< UP #<=> S,
    among_interval_signature(VARs, Ss, LOW, UP).
