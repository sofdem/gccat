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

ctr_date(clause_or,['20090415']).

ctr_origin(clause_or, 'Logic', []).

ctr_arguments(clause_or,
              ['POSVARS'-collection(var-dvar),
               'NEGVARS'-collection(var-dvar),
               'VAR'-dvar                    ]).

ctr_exchangeable(clause_or,
                 [items('POSVARS',all),
                  items('NEGVARS',all)]).

ctr_synonyms(clause_or,[clause]).

ctr_restrictions(clause_or,
                 [size('POSVARS')+size('NEGVARS') > 0,
                  required('POSVARS',var)            ,
                  'POSVARS'^var >= 0                 ,
                  'POSVARS'^var =< 1                 ,
                  required('NEGVARS',var)            ,
                  'NEGVARS'^var >= 0                 ,
                  'NEGVARS'^var =< 1                 ,
                  'VAR' >= 0                         ,
                  'VAR' =< 1                         ]).

ctr_typical(clause_or,
            [size('POSVARS')+size('NEGVARS') > 1]).

ctr_extensible(clause_or, ['VAR'=1], 'POSVARS', any).
ctr_extensible(clause_or, ['VAR'=1], 'NEGVARS', any).

ctr_example(clause_or,
            clause_or([[var-0],[var-0]], [[var-0]], 1)).

ctr_see_also(clause_or,
 [link('common keyword', or,         '%k', ['Boolean constraint']),
  link('common keyword', clause_and, '%k', ['Boolean constraint'])]).

ctr_key_words(clause_or,['Boolean constraint'              ,
                         'disjunction'                     ,
                         'Berge-acyclic constraint network',
                         'automaton'                       ,
                         'automaton without counters'      ,
                         'reified automaton constraint'    ,
                         'arc-consistency'                 ]).

ctr_eval(clause_or, [automaton(clause_or_a)]).

% 0: VAR=0
% 1: VAR=1
clause_or_a(FLAG, POSVARS, NEGVARS, VAR) :-
    collection(POSVARS, [dvar(0,1)]),
    collection(NEGVARS, [dvar(0,1)]),
    check_type(dvar(0,1), VAR),
    length(POSVARS, LP),
    length(NEGVARS, LN),
    L is LP + LN,
    L > 0,
    get_attr1(POSVARS, LISTP),
    get_attr1(NEGVARS, LISTN),
    clause_or_negate(LISTN, LISTNN),
    append([VAR], LISTP, LIST),
    append(LIST, LISTNN, LIST_VARIABLES),
    AUTOMATON = automaton(LIST_VARIABLES, _,
                          LIST_VARIABLES,
                          [source(s),sink(i),sink(k)],
                          [arc(s,0,i),
                           arc(s,1,j),
                           arc(i,0,i),
                           arc(j,0,j),
                           arc(j,1,k),
                           arc(k,0,k),
                           arc(k,1,k)],
                          [], [], []),
    automaton_bool(FLAG, [0,1], AUTOMATON).

clause_or_negate([], []).
clause_or_negate([V|R], [U|S]) :-
    V #<=> #\U,
    clause_or_negate(R, S).
