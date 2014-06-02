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

:-dynamic counts_a/4.

ctr_date(counts,['20030820','20040530','20060806']).

ctr_origin(counts, 'Derived from %c.', [count]).

ctr_arguments(counts,
              ['VALUES'-collection(val-int)    ,
               'VARIABLES'-collection(var-dvar),
               'RELOP'-atom                    ,
               'LIMIT'-dvar                    ]).

ctr_exchangeable(counts,
                 [items('VALUES',all),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_restrictions(counts,
                 [required('VALUES',val)            ,
                  distinct('VALUES',val)            ,
                  required('VARIABLES',var)         ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(counts,
            [size('VALUES')         > 1             ,
             size('VARIABLES')      > 1             ,
             range('VARIABLES'^var) > 1             ,
             size('VARIABLES')      > size('VALUES'),
             in_list('RELOP',[=,<,>=,>,=<])         ,
             'LIMIT' > 0                            ,
             'LIMIT' < size('VARIABLES')            ]).

ctr_pure_functional_dependency(counts, [in_list('RELOP',[=])]).

ctr_contractible(counts, [in_list('RELOP',[<,=<])], 'VARIABLES', any).

ctr_extensible(counts, [in_list('RELOP',[>=,>])], 'VARIABLES', any).

% in_list('RELOP',[<,=<,>=,>]): counts('VALUES1', 'VARIABLES1', 'RELOP', 'LIMIT1') and
%                               counts('VALUES2', 'VARIABLES2', 'RELOP', 'LIMIT2') =>
%                               counts(sunion('VALUES1','VALUES2'), union('VARIABLES1','VARIABLES2'), 'RELOP', 'LIMIT1'+'LIMIT2')
ctr_aggregate(counts, [in_list('RELOP',[<,=<,>=,>])], [sunion, union, id, +]).

ctr_graph(counts,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var = values^val],
          ['RELOP'('NARC','LIMIT')],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(counts,
            counts([[val-1],[val-3],[val-4],[val-9]],
                   [[var-4],[var-5],[var-5],[var-4],[var-1],[var-5]],
                   =,
                   3)).

ctr_draw_example(counts,
                 ['VARIABLES','VALUES'],
                 [[[var-4],[var-5],[var-5],[var-4],[var-1],[var-5]],
                  [[val-1],[val-3],[val-4],[val-9]]],
                 ['PRODUCT'],
                 [1-3,4-3,5-1],
                 ['NARC'],
                 '','NARC=3',
                 [2.4,2.145,1.8,1.2]).

ctr_see_also(counts,
 [link(specialisation,               count,             '%e replaced by %e=%e', [in_list(variable,'VALUES'),variable,'VALUE']),
  link('assignment dimension added', assign_and_counts, '%k introduced',        ['assignment dimension']),
  link('common keyword',             among,             '%k,%k',                ['value constraint','counting constraint'])]).

ctr_key_words(counts,['value constraint'                   ,
                      'counting constraint'                ,
                      'automaton'                          ,
                      'automaton with counters'            ,
                      'alpha-acyclic constraint network(2)',
                      'acyclic'                            ,
                      'bipartite'                          ,
                      'no loop'                            ,
                      'arc-consistency'                    ]).

ctr_eval(counts, [reformulation(counts_r), automaton(counts_a)]).

counts_r(VALUES, VARIABLES, RELOP, LIMIT) :-
    collection(VALUES, [int]),
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    get_attr1(VALUES, VALS),
    all_different(VALS),
    length(VARIABLES, NVARIABLES),
    N in 0..NVARIABLES,
    eval(among(N, VARIABLES, VALUES)),
    call_term_relop_value(N, RELOP, LIMIT).

% 0: not_in(VAR,VALUES)
% 1: in(VAR,VALUES)
counts_a(FLAG, VALUES, VARIABLES, RELOP, LIMIT) :-
    collection(VALUES, [int]),
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    get_attr1(VALUES, LIST_VALUES),
    all_different(LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    counts_signature(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
             [source(s),sink(s)],
             [arc(s,0,s      ),
              arc(s,1,s,[C+1])],
             [C],[0],[NIN]),
    count_relop(RELOP, NIN, LIMIT, FLAG).

counts_signature([], [], _).
counts_signature([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> S,
    counts_signature(VARs, Ss, SET_OF_VALUES).
