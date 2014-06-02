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

:-dynamic count_a/4.

ctr_date(count,['20000128','20030820','20040530','20060806','20100204']).

ctr_origin(count, '\\cite{Sicstus95}', []).

ctr_arguments(count,
              ['VALUE'-int                     ,
               'VARIABLES'-collection(var-dvar),
               'RELOP'-atom                    ,
               'LIMIT'-dvar                    ]).

ctr_exchangeable(count,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int(=\=('VALUE')),=\=,dontcare,dontcare)]).

ctr_synonyms(count,[occurencemax, occurencemin, occurrence]).

ctr_restrictions(count,
                 [required('VARIABLES',var)         ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(count,
            [size('VARIABLES')      > 1    ,
             range('VARIABLES'^var) > 1    ,
             in_list('RELOP',[=,<,>=,>,=<]),
             'LIMIT' > 0                   ,
             'LIMIT' < size('VARIABLES')   ]).

ctr_pure_functional_dependency(count, [in_list('RELOP',[=])]).

ctr_contractible(count, [in_list('RELOP',[<,=<])], 'VARIABLES', any).

ctr_extensible(count, [in_list('RELOP',[>=,>])], 'VARIABLES', any).

% in_list('RELOP',[<,=<,>=,>]): count('VALUE', 'VARIABLES1', 'RELOP', 'LIMIT1') and
%                               count('VALUE', 'VARIABLES2', 'RELOP', 'LIMIT2') =>
%                               count('VALUE', union('VARIABLES1','VARIABLES2'), 'RELOP', 'LIMIT1'+'LIMIT2')
ctr_aggregate(count, [in_list('RELOP',[<,=<,>=,>])], [id, union, id, +]).

ctr_graph(count,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var = 'VALUE'],
          ['RELOP'('NARC','LIMIT')],
          []).

ctr_example(count,
            count(5,[[var-4],[var-5],[var-5],[var-4],[var-5]],>=,2)).

ctr_draw_example(count,
                 ['VARIABLES'],
                 [[[var-4],[var-5],[var-5],[var-4],[var-5]]],
                 ['SELF'],
                 [2-2, 3-3, 5-5],
                 ['NARC'],
                 '','NARC=3',
                 [2.4,2.2,2.2,0.4]).

ctr_see_also(count,
 [link('assignment dimension added', assign_and_counts,  '%e=%e replaced by %e and %k introduced', [variable,'VALUE',in_list(variable,'VALUES'),'assignment dimension']),
  link(generalisation,               counts,             '%e=%e replaced by %e',                   [variable,'VALUE',in_list(variable,'VALUES')]),
  link('common keyword',             global_cardinality, '%k,%k',                                  ['value constraint','counting constraint']),
  link('common keyword',             among,              '%k,%k',                                  ['value constraint','counting constraint']),
  link('common keyword',             arith,              '%k',                                     ['value constraint']),
  link('common keyword',             max_nvalue,         '%k,%k',                                  ['value constraint','counting constraint']),
  link('common keyword',             min_nvalue,         '%k,%k',                                  ['value constraint','counting constraint']),
  link('common keyword',             compare_and_count,  '%k',                                     ['counting constraint']),
  link('common keyword',             nvalue,             '%k',                                     ['counting constraint']),
  link('used in reformulation',      among,              '',                                       []),
  link('related',                    roots,              '',                                       [])]).

ctr_key_words(count,['value constraint'                   ,
                     'counting constraint'                ,
                     'automaton'                          ,
                     'automaton with counters'            ,
                     'alpha-acyclic constraint network(2)',
                     'arc-consistency'                    ]).

ctr_eval(count, [reformulation(count_r), automaton(count_a)]).

count_r(VALUE, VARIABLES, RELOP, LIMIT) :-
    check_type(int, VALUE),
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    length(VARIABLES, NVARIABLES),
    N in 0..NVARIABLES,
    eval(among(N, VARIABLES, [[val-VALUE]])),
    call_term_relop_value(N, RELOP, LIMIT).

% 0: VAR=\=VALUE
% 1: VAR=VALUE
count_a(FLAG, VALUE, VARIABLES, RELOP, LIMIT) :-
    check_type(int, VALUE),
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    count_signature(VARIABLES, SIGNATURE, VALUE),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[NIN]),
    count_relop(RELOP, NIN, LIMIT, FLAG).

count_signature([], [], _).
count_signature([[var-VAR]|VARs], [S|Ss], VALUE) :-
    VAR #= VALUE #<=> S,
    count_signature(VARs, Ss, VALUE).
