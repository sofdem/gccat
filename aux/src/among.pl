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

:-dynamic among_a/3.

ctr_date(among,['20000128','20030820','20040807','20060804']).

ctr_origin(among, '\\cite{BeldiceanuContejean94}', []).

ctr_arguments(among,
              ['NVAR'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(among,
                 [items('VARIABLES',all),
                  items('VALUES',all),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_synonyms(among,[between, count]).

ctr_restrictions(among,
                 ['NVAR' >= 0                ,
                  'NVAR' =< size('VARIABLES'),
                  required('VARIABLES',var)  ,
                  required('VALUES',val)     ,
                  distinct('VALUES',val)     ]).

ctr_typical(among,
            ['NVAR' > 0                         ,
             'NVAR' < size('VARIABLES')         ,
              size('VARIABLES') > 1             ,
              size('VALUES')    > 1             ,
              size('VARIABLES') > size('VALUES')]).

ctr_pure_functional_dependency(among, []).
ctr_functional_dependency(among, 1, [2,3]).

ctr_contractible(among, ['NVAR'=0], 'VARIABLES', any).
ctr_contractible(among, ['NVAR'=size('VARIABLES')], 'VARIABLES', any).

% among('NVAR1', 'VARIABLES1', 'VALUES1') and
% among('NVAR2', 'VARIABLES2', 'VALUES2') =>
% among('VAR1'+'VAR2', union('VARIABLES1','VARIABLES2'), sunion('VALUES1','VALUES2'))
ctr_aggregate(among, [], [+, union, sunion]).

ctr_graph(among,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [in(variables^var,'VALUES')],
          ['NARC' = 'NVAR'],
	  []).

ctr_example(among,
            among(3,
                  [[var-4],[var-5],[var-5],[var-4],[var-1]],
                  [[val-1],[val-5],[val-8]])).

ctr_draw_example(among,
                 ['VARIABLES'],
                 [[[var-4],[var-5],[var-5],[var-4],[var-1]]],
                 ['SELF'],
                 [2-2,3-3,5-5],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,0.53625]).

ctr_see_also(among,
 [link('implies',                   among_var,           '',                                                          []),
  link('implies',                   cardinality_atmost,  '',                                                          []),
  link(generalisation,              among_var,           '%e replaced by %e',                                         [constant,variable]),
  link(specialisation,              among_low_up,        '%e replaced by %e',                                         [variable,interval]),
  link(specialisation,              among_diff_0,        '%e replaced by %e different from %e',                       [in_list(variable,values),variable,0]),
  link(specialisation,              among_interval,      '%e replaced by %e',                                         [in_list(variable,values),in_list(variable,interval)]),
  link(specialisation,              among_modulo,        'list of %e replaced by list of %e %e such that %e = %e',    [values,values,v,v mod 'QUOTIENT','REMAINDER']),
  link(specialisation,              exactly,             '%e replaced by %e and %e replaced by one single %e',        [variable,constant,values,value]),
  link('soft variant',              open_among,          '%k',                                                        ['open constraint']),
  link('system of constraints',     global_cardinality,  'count the number of occurrences of different values',       []),
  link('common keyword',            arith,               '%k',                                                        ['value constraint']),
  link('common keyword',            atleast,             '%k',                                                        ['value constraint']),
  link('common keyword',            atmost,              '%k',                                                        ['value constraint']),
  link('common keyword',            counts,              '%k,%k',                                                     ['value constraint','counting constraint']),  
  link('common keyword',            count,               '%k',                                                        ['counting constraint']),
  link('common keyword',            discrepancy,         '%k',                                                        ['counting constraint']),
  link('common keyword',            nvalue,              '%k',                                                        ['counting constraint']),
  link('common keyword',            max_nvalue,          '%k',                                                        ['counting constraint']),
  link('common keyword',            min_nvalue,          '%k',                                                        ['counting constraint']),
  link('shift of concept',          among_seq,           '%e replaced by %e and constraint applied in a sliding way', [variable,interval]),
  link('shift of concept',          common,              '',                                                          []),
  link('related',                   sliding_card_skip0,  'counting constraint on maximal sequences',                  []),
  link('related',                   roots,               'can be used for expressing %c',                             [among]),
  link('used in graph description', in,                  '',                                                          []),
  link('uses in its reformulation', count,               '',                                                          [])]).

ctr_key_words(among,['value constraint'                   ,
                     'counting constraint'                ,
                     'automaton'                          ,
                     'automaton with counters'            ,
                     'reverse of a constraint'            ,
		     'glue matrix'                        ,
                     'alpha-acyclic constraint network(2)',
                     'Berge-acyclic constraint network'   ,
                     'non-deterministic automaton'        ,
                     'arc-consistency'                    ,
                     'functional dependency'              ,
		     'pure functional dependency'         ,
                     'SAT'                                ]).

ctr_persons(among,['Beldiceanu N.'      ,
                   'Contejean E.'       ,
                   'Bessi\\`ere C.'     ,
                   'Hebrard E.'         ,
                   'Hnich B.'           ,
                   'K{\\i}z{\\i}ltan Z.',
                   'Walsh T.'           ,
                   'Bacchus F.'         ,
		   'Chabert G.'         ,
		   'Demassey S.'        ]).

ctr_eval(among, [checker(among_c),
		 reformulation(among_r),
		 automaton(among_a)]).

among_c(N, VARIABLES, VALUES) :-
    integer(N),
    N >= 0,
    collection(VALUES, [int]),
    get_attr1(VALUES, VALS),
    all_different(VALS),
    among_c1(VARIABLES, N, VALS).

among_c1([[var-V]|R], N, VALS) :-
    !,
    integer(V),
    (memberchk(V,VALS) -> N1 is N-1, N1 >= 0 ; N1 is N),
    among_c1(R, N1, VALS).
among_c1([], 0, _).

among_r(NVAR, VARIABLES, VALUES) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int]),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),
    length(VARIABLES, N),
    NVAR #>= 0,
    NVAR #=< N,
    all_different(VALS),
    among1(VARS, VALS, SUM_BVARS),
    call(NVAR #= SUM_BVARS).

among1([], _, 0).
among1([V|R], VALS, B+S) :-
    build_or_var_in_values(VALS, V, OR),
    call(OR #<=> B),
    among1(R, VALS, S).

% 0: not_in(VAR,VALUES)
% 1: in(VAR,VALUES)
among_a(FLAG, NVAR, VARIABLES, VALUES) :-
    check_type(dvar, NVAR),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int]),
    get_attr1(VALUES, LIST_VALUES),
    length(VARIABLES, N),
    NVAR #>= 0,
    NVAR #=< N,
    all_different(LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    among_signature(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NVAR #<=> FLAG.

among_signature([], [], _).
among_signature([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> S,
    among_signature(VARs, Ss, SET_OF_VALUES).
