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

ctr_date(in_interval,['20060317','20060810']).

ctr_origin(in_interval, 'Domain definition.', []).

ctr_arguments(in_interval,
              ['VAR'-dvar,
               'LOW'-int ,
               'UP'-int  ]).

ctr_exchangeable(in_interval,
                 [vals(['LOW'],int,>,dontcare,dontcare),
                  vals(['UP'],int,<,dontcare,dontcare),
                  vals(['VAR'],int(in('LOW','UP')),=\=,dontcare,dontcare),
                  translate(['VAR','LOW','UP'])]).

ctr_synonyms(in_interval,[dom,in]).

ctr_restrictions(in_interval,
                 ['LOW' =< 'UP']).

ctr_typical(in_interval,
            ['LOW' < 'UP' ,
             'VAR' > 'LOW',
             'VAR' < 'UP' ]).

ctr_derived_collections(in_interval,
                        [col('VARIABLE'-collection(var-dvar),
                             [item(var-'VAR')]),
                         col('INTERVAL'-collection(low-int,up-int),
                             [item(low-'LOW',up-'UP')])]).

ctr_graph(in_interval,
          ['VARIABLE','INTERVAL'],
          2,
          ['PRODUCT'>>collection(variable,interval)],
          [variable^var >= interval^low,
	       variable^var =< interval^up ],
          ['NARC' = 1],
          []).

ctr_example(in_interval,
            in_interval(3, 2, 5)).

ctr_draw_example(in_interval,
                 ['VARIABLE','INTERVAL'],
                 [[[var-3]],
                  [[low-2, up-5]]],
                 ['PRODUCT'],
                 [1-1],
                 ['NARC'],
                 '','NARC=1',
                 [1.2,1.2,1,1]).

ctr_see_also(in_interval,
 [link('generalisation', in_interval_reified, 'reified version',                                []),
  link('generalisation', in_intervals,        'single interval replaced by a set of intervals', []),
  link('generalisation', in_set,              'interval replaced by set variable',              []),
  link('common keyword', in,                  '%k',                                             ['domain definition']),
  link('common keyword', domain,              '%k',                                             ['domain definition'])]).

ctr_key_words(in_interval,['value constraint'                ,
                           'unary constraint'                ,
                           'interval'                        ,
                           'domain definition'               ,
                           'automaton'                       ,
                           'automaton without counters'      ,
                           'reified automaton constraint'    ,
                           'Berge-acyclic constraint network',
                           'derived collection'              ,
                           'arc-consistency'                 ]).

ctr_eval(in_interval, [reformulation(in_interval_r),
                       automaton(in_interval_a)]).

in_interval_r(VAR, LOW, UP) :-
    check_type(fdvar, VAR),
    check_type(int, LOW),
    check_type(int, UP),
    LOW =< UP,
    VAR #>= LOW,
    VAR #=< UP.

% 0: VAR=\=VAL
% 1: VAR=VAL
in_interval_a(FLAG, VAR, LOW, UP) :-
    check_type(fdvar, VAR),
    check_type(int, LOW),
    check_type(int, UP),
    LOW =< UP,
    VAR #>= LOW #/\ VAR #=< UP #<=> S,
    AUTOMATON = automaton([S], _,
                          [S],
                          [source(s),sink(t)],
                          [arc(s,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).
