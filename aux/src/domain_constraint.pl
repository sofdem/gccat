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

ctr_date(domain_constraint,['20030820','20040530','20060808']).

ctr_origin(domain_constraint, '\\cite{Refalo00}', []).

ctr_arguments(domain_constraint,
              ['VAR'-dvar                                ,
               'VALUES'-collection(var01-dvar, value-int)]).

ctr_exchangeable(domain_constraint,
                 [items('VALUES',all)]).

ctr_synonyms(domain_constraint,[domain]).

ctr_restrictions(domain_constraint,
                 [required('VALUES',[var01,value]),
                  'VALUES'^var01 >= 0             ,
                  'VALUES'^var01 =< 1             ,
                  distinct('VALUES',value)        ]).

ctr_typical(domain_constraint,
            [size('VALUES') > 1]).

ctr_derived_collections(domain_constraint,
                        [col('VALUE'-collection(var01-int,value-dvar),
                             [item(var01-1,value-'VAR')])]).

ctr_graph(domain_constraint,
          ['VALUE','VALUES'],
          2,
          ['PRODUCT'>>collection(value,values)],
          [value^value = values^value  #<=>  values^var01 = 1],
          ['NARC' = size('VALUES')],
          []).

ctr_example(domain_constraint,
            domain_constraint(5,[[var01-0, value-9],
                                 [var01-1, value-5],
                                 [var01-0, value-2],
                                 [var01-0, value-7]])).

ctr_draw_example(domain_constraint,
                 ['VALUE','VALUES'],
                 [[[var01-1, value-5]],
                  [[var01-0, value-9],
                   [var01-1, value-5],
                   [var01-0, value-2],
                   [var01-0, value-7]]],
                 ['PRODUCT'],
                 [1-[1,2,3,4]],
                 ['NARC'],
                 '','NARC=4',
                 [2.145,2.145,2.145,1.9]).

ctr_see_also(domain_constraint,
 [link('common keyword', link_set_to_booleans, '%k', ['channelling constraint']),
  link('related',        roots,                '',   [])]).

ctr_key_words(domain_constraint,['decomposition'                           ,
                                 'channelling constraint'                  ,
                                 'domain channel'                          ,
                                 'Boolean channel'                         ,
                                 'linear programming'                      ,
                                 'automaton'                               ,
                                 'automaton without counters'              ,
                                 'reified automaton constraint'            ,
                                 'centered cyclic(1) constraint network(1)',
                                 'derived collection'                      ,
                                 'arc-consistency'                         ]).

ctr_persons(domain_constraint,['Refalo P.']).

ctr_eval(domain_constraint, [reformulation(domain_constraint_r), automaton(domain_constraint_a)]).

domain_constraint_r(VAR, VALUES) :-
    check_type(dvar, VAR),
    collection(VALUES, [dvar(0,1),int]),
    get_attr1(VALUES, VARS01),
    get_attr2(VALUES, VALS  ),
    all_different(VALS),
    domain_constraint1(VARS01, VALS, VAR, Term),
    call(Term).

domain_constraint1([], [], _, 0).
domain_constraint1([VAR01|R], [VAL|S], VAR, (VAR #= VAL #/\ VAR01 #= 1) #\/ T) :-
    domain_constraint1(R, S, VAR, T).

% 0: VAR=VALUE is not equivalent to VAR01=1
% 1: VAR=VALUE is     equivalent to VAR01=1
domain_constraint_a(FLAG, VAR, VALUES) :-
    check_type(dvar, VAR),
    collection(VALUES, [dvar(0,1),int]),
    get_attr2(VALUES, VALS  ),
    all_different(VALS),
    domain_constraint_signature(VALUES, SIGNATURE, VAR),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,1,s)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

domain_constraint_signature([], [], _).
domain_constraint_signature([[var01-VAR01,value-VALUE]|VALUES], [S|Ss], VAR) :-
    ((VAR#=VALUE) #<=> VAR01) #<=> S,
    domain_constraint_signature(VALUES, Ss, VAR).
