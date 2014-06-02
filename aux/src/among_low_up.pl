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

:-dynamic among_low_up_a/4.

ctr_date(among_low_up,['20030820','20040530','20060804']).

ctr_origin(among_low_up, '\\cite{BeldiceanuContejean94}', []).

ctr_arguments(among_low_up,
              ['LOW'-int                       ,
               'UP'-int                        ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int )   ]).

ctr_exchangeable(among_low_up,
                 [items('VARIABLES',all),
                  items('VALUES',all),
                  vals(['LOW'],int(>=(0)),>,dontcare,dontcare),
                  vals(['UP'],int(=<(size('VARIABLES'))),<,dontcare,dontcare),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_restrictions(among_low_up,
                 ['LOW' >= 0                ,
                  'LOW' =< size('VARIABLES'),
                  'UP'  >= 0                ,
                  'UP'  =< size('VARIABLES'),
                  'UP'  >= 'LOW'            ,
                  required('VARIABLES',var) ,
                  required('VALUES',val)    ,
                  distinct('VALUES',val)    ]).

ctr_typical(among_low_up,
            ['LOW' < size('VARIABLES')              ,
             'UP'  > 0                              ,
             'LOW' < 'UP'                           ,
              size('VARIABLES') > 1                 ,
              size('VALUES')    > 1                 ,
              size('VARIABLES') > size('VALUES')    ,
              'LOW' > 0 #\/ 'UP' < size('VARIABLES')]).

ctr_contractible(among_low_up, ['UP'=0], 'VARIABLES', any).
ctr_contractible(among_low_up, ['UP'=size('VARIABLES')], 'VARIABLES', any).

% among_low_up('LOW1', 'UP1', 'VARIABLES1', 'VALUES1') and
% among_low_up('LOW2', 'UP2', 'VARIABLES2', 'VALUES2') => 
% among_low_up('LOW1'+'LOW2', 'UP1'+'UP2', union('VARIABLES1','VARIABLES2'), sunion('VALUES1','VALUES2'))
ctr_aggregate(among_low_up, [], [+, +, union, sunion]).

ctr_graph(among_low_up,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var = values^val],
          ['NARC' >= 'LOW',
           'NARC' =< 'UP' ],
	     ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(among_low_up,
            among_low_up(1,2,
                         [[var-9],[var-2],[var-4],[var-5]],
                         [[val-0],[val-2],[val-4],[val-6],[val-8]])).

ctr_draw_example(among_low_up,
                 ['VARIABLES','VALUES'],
                 [[[var-9],[var-2],[var-4],[var-5]],
                  [[val-0],[val-2],[val-4],[val-6],[val-8]]],
                 ['PRODUCT'],
                 [2-2,3-3],
                 ['NARC'],
                 '','NARC=2',
                 [2.145,2.145,1.2,1.2]).

ctr_cond_imply(among_low_up, among_low_up, [distinct('VARIABLES',var)], [], ['LOW','UP','VALUES','VARIABLES']).

ctr_see_also(among_low_up,
 [link(generalisation,               among,              '%e replaced by %e',                                                     [interval,variable]),
  link(generalisation,               sliding_card_skip0, 'full sequence replaced by maximal sequences of non\\nobreakdash-zeros', []),
  link('assignment dimension added', interval_and_count, '%k corresponding to intervals added',                                   ['assignment dimension']),
  link('system of constraints',      among_seq,          '',                                                                      [])]).

ctr_key_words(among_low_up,['value constraint'                   ,
                            'counting constraint'                ,
                            'automaton'                          ,
                            'automaton with counters'            ,
                            'alpha-acyclic constraint network(2)',
                            'acyclic'                            ,
                            'bipartite'                          ,
                            'no loop'                            ,
                            'arc-consistency'                    ,
                            'entailment'                         ]).

ctr_persons(among_low_up,['Beldiceanu N.',
                          'Contejean E.' ]).

ctr_eval(among_low_up, [reformulation(among_low_up_r), automaton(among_low_up_a)]).

among_low_up_r(LOW, UP, VARIABLES, VALUES) :-
    integer(LOW),
    integer(UP),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int]),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),
    length(VARIABLES, N),
    LOW >= 0,
    LOW =< N,
    UP  >= 0,
    UP  =< N,
    UP  >= LOW,
    all_different(VALS),
    among_low_up1(VARS, VALS, SUM_BVARS),
    call(LOW #=< SUM_BVARS),
    call(UP  #>= SUM_BVARS).

among_low_up1([], _, 0).
among_low_up1([V|R], VALS, B+S) :-
    build_or_var_in_values(VALS, V, OR),
    call(OR #<=> B),
    among_low_up1(R, VALS, S).

% 0: not_in(VAR,VALUES)
% 1: in(VAR,VALUES)
among_low_up_a(FLAG, LOW, UP, VARIABLES, VALUES) :-
    integer(LOW),
    integer(UP),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int]),
    get_attr1(VALUES, LIST_VALUES),
    length(VARIABLES, N),
    LOW >= 0,
    LOW =< N,
    UP  >= 0,
    UP  =< N,
    UP  >= LOW,
    all_different(LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    among_low_up_signature(VARIABLES, SIGNATURE, SET_OF_VALUES),
    NVAR in LOW..UP,
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NVAR #<=> FLAG.

among_low_up_signature([], [], _).
among_low_up_signature([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> S,
    among_low_up_signature(VARs, Ss, SET_OF_VALUES).
