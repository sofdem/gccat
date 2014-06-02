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

:-dynamic atleast_a/4.

ctr_date(atleast,['20030820','20040807','20060804']).

ctr_origin(atleast, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(atleast,
              ['N'-int                         ,
               'VARIABLES'-collection(var-dvar),
               'VALUE'-int                     ]).

ctr_exchangeable(atleast,
                 [items('VARIABLES',all),
                  vals(['N'],int(>=(0)),>,dontcare,dontcare),
                  vals(['VARIABLES'^var],comp('VALUE'),>=,dontcare,dontcare)]).

ctr_synonyms(atleast,[count]).

ctr_restrictions(atleast,
                 ['N' >= 0                 ,
                  'N' =< size('VARIABLES') ,
                  required('VARIABLES',var)]).

ctr_typical(atleast,
            ['N' > 0                ,
             'N' < size('VARIABLES'),
             size('VARIABLES') > 1  ]).

ctr_extensible(atleast, [], 'VARIABLES', any).

ctr_total_relation(atleast).

ctr_graph(atleast,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var = 'VALUE'],
          ['NARC' >= 'N'],
          []).

ctr_example(atleast,
            atleast(2,[[var-4],[var-2],[var-4],[var-5]],4)).

ctr_draw_example(atleast,
                 ['VARIABLES'],
                 [[[var-4],[var-2],[var-4],[var-5]]],
                 ['SELF'],
                 [1-1,3-3],
                 ['NARC'],
                 '','NARC=2',
                 [2.145,2.145,1,0.6]).

ctr_see_also(atleast,
 [link('implied by',         exactly,      '$\\geq$ %e replaced by $=$ %e', ['N','N']),
  link('comparison swapped', atmost,       '',                              []),
  link('common keyword',     among,        '%k',                            ['value constraint']),
  link('soft variant',       open_atleast, '%k',                            ['open constraint']),
  link('related',            roots,        '',                              [])]).

ctr_key_words(atleast,['value constraint'                   ,
                       'at least'                           ,
                       'automaton'                          ,
                       'automaton with counters'            ,
                       'alpha-acyclic constraint network(2)',
                       'arc-consistency'                    ]).

ctr_eval(atleast, [checker(atleast_c),
		   reformulation(atleast_r),
		   automaton(atleast_a)]).

atleast_c(N, VARIABLES, VALUE) :-
    integer(N),
    integer(VALUE),
    atleast_c1(VARIABLES, N, VALUE).

atleast_c1([[var-V]|R], N, VALUE) :-
    !,
    integer(V),
    (V = VALUE -> N1 is N-1 ; N1 is N),
    atleast_c1(R, N1, VALUE).
atleast_c1([], N, _) :-
    N =< 0.

atleast_r(N, VARIABLES, VALUE) :-
    integer(N),
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, NVARIABLES),
    N >= 0,
    N =< NVARIABLES,
    get_attr1(VARIABLES, VARS),
    atleast1(VARS, VALUE, SUM_BVARS),
    call(SUM_BVARS #>= N).

atleast1([], _, 0).
atleast1([V|R], VALUE, B+S) :-
    V #= VALUE #<=> B,
    atleast1(R, VALUE, S).

% 0: VAR<>VALUE
% 1: VAR= VALUE
atleast_a(FLAG, N, VARIABLES, VALUE) :-
    integer(N),
    collection(VARIABLES, [dvar]),
    integer(VALUE),
    length(VARIABLES, M),
    N >= 0,
    N =< M,
    atleast_signature(VARIABLES, SIGNATURE, VALUE),
    NVAR in N..M,
    automaton(SIGNATURE, _,
              SIGNATURE,
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NVAR #<=> FLAG.

atleast_signature([], [], _).
atleast_signature([[var-VAR]|VARs], [S|Ss], VALUE) :-
    VAR #= VALUE #<=> S,
    atleast_signature(VARs, Ss, VALUE).
