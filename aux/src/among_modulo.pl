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

:-dynamic among_modulo_a/4.

ctr_date(among_modulo,['20030820','20040530','20060804']).

ctr_origin(among_modulo, 'Derived from %c.', [among]).

ctr_arguments(among_modulo,
              ['NVAR'-dvar,
               'VARIABLES'-collection(var-dvar),
               'REMAINDER'-int,
               'QUOTIENT'-int]).

ctr_exchangeable(among_modulo,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],comp(mod('QUOTIENT','REMAINDER')),=,dontcare,dontcare)]).

ctr_restrictions(among_modulo,
                 ['NVAR' >= 0                ,
                  'NVAR' =< size('VARIABLES'),
                  required('VARIABLES',var)  ,
                  'REMAINDER' >= 0           ,
                  'REMAINDER' < 'QUOTIENT'   ,
                  'QUOTIENT'  > 0            ]).

ctr_typical(among_modulo,
            ['NVAR'             > 0                      ,
             'NVAR'             < size('VARIABLES')      ,
              size('VARIABLES') > 1                      ,
              'QUOTIENT'        > 1                      ,
              'QUOTIENT'        < maxval('VARIABLES'^var)]).

ctr_pure_functional_dependency(among_modulo, []).
ctr_functional_dependency(among_modulo, 1, [2,3,4]).

ctr_contractible(among_modulo, ['NVAR'=0], 'VARIABLES', any).
ctr_contractible(among_modulo, ['NVAR'=size('VARIABLES')], 'VARIABLES', any).

% among_modulo('NVAR1', 'VARIABLES1', 'REMAINDER', 'QUOTIENT') and
% among_modulo('NVAR2', 'VARIABLES2', 'REMAINDER', 'QUOTIENT') =>
% among_modulo('NVAR1'+'NVAR2', union('VARIABLES1','VARIABLES2'), 'REMAINDER', 'QUOTIENT')
ctr_aggregate(among_modulo, [], [+, union, id, id]).

ctr_graph(among_modulo,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var mod 'QUOTIENT' = 'REMAINDER'],
          ['NARC' = 'NVAR'],
	  []).

ctr_example(among_modulo,
            among_modulo(3,
                         [[var-4],[var-5],[var-8],[var-4],[var-1]],
                         0,
                         2)).

ctr_draw_example(among_modulo,
                 ['VARIABLES'],
                 [[[var-4],[var-5],[var-8],[var-4],[var-1]]],
                 ['SELF'],
                 [1-1,3-3,4-4],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,1.6,0.53625]).

ctr_see_also(among_modulo,
 [link(generalisation, among, 'list of %e %e such that %e = %e replaced by list of %e', [values,v,v mod 'QUOTIENT','REMAINDER',values])]).

ctr_key_words(among_modulo,['value constraint'                   ,
                            'counting constraint'                ,
                            'modulo'                             ,
                            'automaton'                          ,
                            'automaton with counters'            ,
                            'alpha-acyclic constraint network(2)',
                            'arc-consistency'                    ,
                            'functional dependency'              ,
		            'pure functional dependency'         ]).

ctr_eval(among_modulo, [reformulation(among_modulo_r), automaton(among_modulo_a)]).

among_modulo_r(NVAR, VARIABLES, REMAINDER, QUOTIENT) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    length(VARIABLES, N),
    integer(REMAINDER),
    integer(QUOTIENT),
    NVAR #>= 0,
    NVAR #=< N,
    REMAINDER >= 0,
    REMAINDER <  QUOTIENT,
    QUOTIENT  >  0,
    gen_remainder(VARS, QUOTIENT, REMVARS),
    among_modulo1(REMVARS, REMAINDER, SUM_BVARS),
    call(NVAR #= SUM_BVARS).

among_modulo1([], _, 0).
among_modulo1([V|R], REMAINDER, B+S) :-
    V #= REMAINDER #<=> B,
    among_modulo1(R, REMAINDER, S).

% 0: VAR mod QUOTIENT =\= REMAINDER
% 1: VAR mod QUOTIENT  =  REMAINDER
among_modulo_a(FLAG, NVAR, VARIABLES, REMAINDER, QUOTIENT) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [dvar]),
    integer(REMAINDER),
    integer(QUOTIENT),
    length(VARIABLES, N),
    NVAR #>= 0,
    NVAR #=< N,
    REMAINDER >= 0,
    REMAINDER <  QUOTIENT,
    QUOTIENT  >  0,
    among_modulo_signature(VARIABLES, SIGNATURE, REMAINDER, QUOTIENT),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NVAR #<=> FLAG.

among_modulo_signature([], [], _, _).
among_modulo_signature([[var-VAR]|VARs], [S|Ss], REMAINDER, QUOTIENT) :-
	VAR mod QUOTIENT #= REMAINDER #<=> S,
	among_modulo_signature(VARs, Ss, REMAINDER, QUOTIENT).
