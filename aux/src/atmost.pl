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
    ctr_total_relation/1,
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

:-dynamic atmost_a/4.

ctr_date(atmost,['20030820','20040807','20060804']).

ctr_origin(atmost, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(atmost,
              ['N'-int                         ,
               'VARIABLES'-collection(var-dvar),
               'VALUE'-int                     ]).

ctr_exchangeable(atmost,
                 [items('VARIABLES',all),
                  vals(['N'],int,<,dontcare,dontcare),
                  vals(['VARIABLES'^var],comp('VALUE'),=<,dontcare,dontcare)]).

ctr_synonyms(atmost,[count]).

ctr_restrictions(atmost,
                 ['N' >= 0                 ,
                  required('VARIABLES',var)]).

ctr_typical(atmost,
            ['N'               > 0                ,
             'N'               < size('VARIABLES'),
             size('VARIABLES') > 1                ,
             atleast(1,'VARIABLES','VALUE')       ]).

ctr_contractible(atmost, [], 'VARIABLES', any).

ctr_total_relation(atmost).

ctr_graph(atmost,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var = 'VALUE'],
          ['NARC' =< 'N'],
          []).

ctr_example(atmost,
            atmost(1,[[var-4],[var-2],[var-4],[var-5]],2)).

ctr_draw_example(atmost,
                 ['VARIABLES'],
                 [[[var-4],[var-2],[var-4],[var-5]]],
                 ['SELF'],
                 [2-2],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,2.145,0.6,0.6]).

ctr_see_also(atmost,
 [link('comparison swapped', atleast,     '',                            []),
  link('common keyword',     among,       '%k',                          ['value constraint']),
  link('implied by',         exactly,     '$\\leq$%e replaced by $=$%e', ['N','N']),
  link('soft variant',       open_atmost, '%k',                          ['open constraint']),
  link(generalisation,       cumulative,  '%e replaced by %e',           [variable,task]),
  link('related',            roots,       '',                            [])]).

ctr_key_words(atmost,['value constraint'                   ,
                      'at most'                            ,
                      'automaton'                          ,
                      'automaton with counters'            ,
                      'alpha-acyclic constraint network(2)',
                      'arc-consistency'                    ]).

ctr_eval(atmost, [checker(atmost_c),
		  reformulation(atmost_r),
		  automaton(atmost_a)]).

atmost_c(N, VARIABLES, VALUE) :-
    integer(N),
    integer(VALUE),
    N >= 0,
    atmost_c1(VARIABLES, N, VALUE).

atmost_c1([[var-V]|R], N, VALUE) :-
    !,
    integer(V),
    (V = VALUE -> N1 is N-1, N1 >= 0 ; N1 is N),
    atmost_c1(R, N1, VALUE).
atmost_c1([], _, _).

atmost_r(N, VARIABLES, VALUE) :-
    integer(N),
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    N >= 0,
    get_attr1(VARIABLES, VARS),
    atmost1_(VARS, VALUE, SUM_BVARS),
    call(SUM_BVARS #=< N).

atmost1_([], _, 0).
atmost1_([V|R], VALUE, B+S) :-
    V #= VALUE #<=> B,
    atmost1_(R, VALUE, S).

% 0: VAR<>VALUE
% 1: VAR= VALUE
atmost_a(FLAG, N, VARIABLES, VALUE) :-
    integer(N),
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    N >= 0,
    atmost_signature(VARIABLES, SIGNATURE, VALUE),
    length(VARIABLES, M),
    MN is min(M,N),
    NVAR in 0..MN,
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NVAR #<=> FLAG.

atmost_signature([], [], _).
atmost_signature([[var-VAR]|VARs], [S|Ss], VALUE) :-
    VAR #= VALUE #<=> S,
    atmost_signature(VARs, Ss, VALUE).
