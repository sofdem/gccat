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

:-dynamic smooth_a/3.

ctr_date(smooth,['20000128','20030820','20040530','20060815']).

ctr_origin(smooth, 'Derived from %c.', [change]).

ctr_arguments(smooth,
              ['NCHANGE'-dvar                  ,
               'TOLERANCE'-int                 ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(smooth,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(smooth,
                 ['NCHANGE'   >=  0               ,
                  'NCHANGE'   <  size('VARIABLES'),
                  'TOLERANCE' >=   0              ,
                  required('VARIABLES',var)       ]).

ctr_typical(smooth,
            ['TOLERANCE'            > 0,
             size('VARIABLES')      > 3,
             range('VARIABLES'^var) > 1]).

ctr_pure_functional_dependency(smooth, []).
ctr_functional_dependency(smooth, 1, [2,3]).

ctr_contractible(smooth, ['NCHANGE'=0], 'VARIABLES', prefix).
ctr_contractible(smooth, ['NCHANGE'=0], 'VARIABLES', suffix).
ctr_contractible(smooth, ['NCHANGE'=size('VARIABLES')-1], 'VARIABLES', prefix).
ctr_contractible(smooth, ['NCHANGE'=size('VARIABLES')-1], 'VARIABLES', suffix).

ctr_graph(smooth,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [abs(variables1^var - variables2^var) > 'TOLERANCE'],
          ['NARC' = 'NCHANGE'],
          []).

ctr_example(smooth,
            smooth(1,2,[[var-1],[var-3],[var-4],[var-5],[var-2]])).

ctr_draw_example(smooth,
                 ['VARIABLES'],
                 [[[var-1],[var-3],[var-4],[var-5],[var-2]]],
                 ['PATH'],
                 [4-5],
                 ['NARC'],
                 '','NARC=1',
                 [2.4,2.145,1,1]).

ctr_see_also(smooth,
 [link('common keyword', change,   '%k in a sequence with respect to a binary constraint', ['number of changes']),
  link('related',        distance, '',                                                     [])]).

ctr_key_words(smooth,['timetabling constraint'                 ,
                      'number of changes'                      ,
                      'automaton'                              ,
                      'automaton with counters'                ,
		      'glue matrix'                            ,
                      'non-deterministic automaton'            ,
                      'sliding cyclic(1) constraint network(2)',
                      'Berge-acyclic constraint network'       ,
                      'non-deterministic automaton'            ,
                      'dynamic programming'                    ,
                      'n-Amazons'                              ,
                      'functional dependency'                  ,
		      'pure functional dependency'             ]).

ctr_persons(smooth,['Beldiceanu N.',
                    'Hellsten L.'  ,
                    'Pesant G.'    ]).

ctr_eval(smooth, [checker(smooth_c),
		  automaton(smooth_a)]).

% 0: |VAR1-VAR2|=<TOLERANCE
% 1: |VAR1-VAR2|>TOLERANCE
smooth_a(FLAG, NCHANGE, TOLERANCE, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    integer(TOLERANCE),
    TOLERANCE >= 0,
    smooth_signature(VARIABLES, SIGNATURE, TOLERANCE),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,1,s,[C+1]),
               arc(s,0,s      )],
              [C],[0],[COUNT]),
    COUNT #= NCHANGE #<=> FLAG.

smooth_signature([], [], _).
smooth_signature([_], [], _) :- !.
smooth_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], TOLERANCE) :-
    abs(VAR1-VAR2) #> TOLERANCE #<=> S #= 1,
    smooth_signature([[var-VAR2]|VARs], Ss, TOLERANCE).

smooth_c(NCHANGE, TOLERANCE, VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    integer(NCHANGE),
    NCHANGE >= 0,
    NCHANGE <  N,    
    integer(TOLERANCE),
    TOLERANCE >= 0,
    get_attr1(VARIABLES, VARS),
    smooth_check(VARS, TOLERANCE, NCHANGE).

smooth_check([], _, 0) :- !.
smooth_check([_], _, 0) :- !.
smooth_check([VAR1,VAR2|R], TOLERANCE, NCHANGE) :-
    D is abs(VAR1-VAR2),
    (D > TOLERANCE -> NCHANGE1 is NCHANGE-1 ; NCHANGE1 is NCHANGE),
    NCHANGE1 >= 0,
    smooth_check([VAR2|R], TOLERANCE, NCHANGE1).
