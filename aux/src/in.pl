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

ctr_date(in,['20030820','20040530','20060810']).

ctr_origin(in, 'Domain definition.', []).

ctr_arguments(in,
              ['VAR'-dvar                  ,
               'VALUES'-collection(val-int)]).

ctr_exchangeable(in,
                 [items('VALUES',all),
                  vals(['VAR'],int(['VAR','VALUES'^val]),=\=,all,dontcare),
                  translate(['VAR','VALUES'^val])]).

ctr_synonyms(in,[dom, in_set, member]).

ctr_restrictions(in,
                 [size('VALUES') > 0    ,
                  required('VALUES',val),
                  distinct('VALUES',val)]).

ctr_typical(in,
            [size('VALUES') > 1]).

ctr_extensible(in, [], 'VALUES', any).

ctr_derived_collections(in,
                        [col('VARIABLES'-collection(var-dvar),
                             [item(var-'VAR')])]).

ctr_graph(in,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var = values^val],
          ['NARC' = 1],
	  []).

ctr_example(in,
            in(3, [[val-1], [val-3]])).

ctr_draw_example(in,
                 ['VARIABLES','VALUES'],
                 [[[var-3]],
                  [[val-1], [val-3]]],
                 ['PRODUCT'],
                 [1-2],
                 ['NARC'],
                 '','NARC=1',
                 [1.2,1.2,1,1]).

ctr_see_also(in,
 [link('negation',       not_in,            '',   []),
  link('implies',        between_min_max,   '',   []),
  link('implied by',     maximum,           '',   []),
  link('implied by',     minimum,           '',   []),
  link('common keyword', domain,            '%k', ['domain definition']),
  link('common keyword', in_interval,       '%k', ['value constraint']),
  link('common keyword', in_set,            '%k', ['value constraint']),
  link('common keyword', in_same_partition, '%k', ['value constraint'])]).

ctr_key_words(in,['value constraint'                        ,
                  'unary constraint'                        ,
                  'included'                                ,
                  'domain definition'                       ,
                  'automaton'                               ,
                  'automaton without counters'              ,
                  'reified automaton constraint'            ,
                  'centered cyclic(1) constraint network(1)',
                  'derived collection'                      ,
                  'arc-consistency'                         ]).

ctr_eval(in, [reformulation(in_r), automaton(in_a)]).

in_r(VAR, VALUES) :-
    check_type(dvar, VAR),
    collection(VALUES, [int]),
    length(VALUES, L),
    L > 0,
    get_attr1(VALUES, VALS),
    all_different(VALS),
    build_or_var_in_values(VALS, VAR, TERM),
    call(TERM).

% 0: VAR=\=VAL
% 1: VAR=VAL
in_a(FLAG, VAR, VALUES) :-
    check_type(dvar, VAR),
    collection(VALUES, [int]),
    length(VALUES, L),
    L > 0,
    get_attr1(VALUES, VALS),
    all_different(VALS),
    in_signature(VALUES, SIGNATURE, VAR),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,0,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

in_signature([], [], _).
in_signature([[val-VAL]|VALs], [S|Ss], VAR) :-
    VAR #= VAL #<=> S,
    in_signature(VALs, Ss, VAR).
