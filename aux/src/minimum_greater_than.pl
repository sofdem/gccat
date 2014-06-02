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

ctr_date(minimum_greater_than,['20030820','20060812']).

ctr_origin(minimum_greater_than, 'N.~Beldiceanu', []).

ctr_arguments(minimum_greater_than,
              ['VAR1'-dvar                     ,
               'VAR2'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(minimum_greater_than,
                 [items('VARIABLES',all)]).

ctr_restrictions(minimum_greater_than,
                 ['VAR1'            > 'VAR2',
                  size('VARIABLES') > 0     ,
                  required('VARIABLES',var) ]).

ctr_typical(minimum_greater_than,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

% minimum_greater_than('VAR11', 'VAR2', 'VARIABLES1') and
% minimum_greater_than('VAR12', 'VAR2', 'VARIABLES2') =>
% minimum_greater_than(min('VAR11','VAR12'), 'VAR2', union('VARIABLES1','VARIABLES2'))
ctr_aggregate(minimum_greater_than, [], [min, id, union]).

ctr_derived_collections(minimum_greater_than,
                        [col('ITEM'-collection(var-dvar),
                             [item(var-'VAR2')])]).

ctr_graph(minimum_greater_than,
          ['ITEM','VARIABLES'],
          2,
          ['PRODUCT'>>collection(item,variables)],
          [item^var < variables^var],
          ['NARC' > 0],
          [],
          ['SUCC'>>[source,variables]],
          [minimum('VAR1',variables)]).

ctr_example(minimum_greater_than,
            minimum_greater_than(5,3,[[var-8],[var-5],[var-3],[var-8]])).

ctr_draw_example(minimum_greater_than,
                 ['ITEM','VARIABLES'],
                 [[[var-3]],
                  [[var-8],[var-5],[var-3],[var-8]]],
                 ['PRODUCT'],
                 [1-[1,2,4]],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,1.5]).

ctr_see_also(minimum_greater_than,
 [link('related',        next_element,         'identify an element in a table', []),
  link('implied by',     next_greater_element, '',                               []),
  link('common keyword', next_greater_element, '%k',                             ['order constraint'])]).

ctr_key_words(minimum_greater_than,['order constraint'                        ,
                                    'minimum'                                 ,
                                    'automaton'                               ,
                                    'automaton without counters'              ,
                                    'reified automaton constraint'            ,
                                    'centered cyclic(2) constraint network(1)',
                                    'derived collection'                      ]).

ctr_persons(minimum_greater_than,['Beldiceanu N.']).

ctr_eval(minimum_greater_than, [reformulation(minimum_greater_than_r),
                                automaton(minimum_greater_than_a)]).

minimum_greater_than_r(VAR1, VAR2, VARIABLES) :-
    check_type(dvar, VAR1),
    check_type(dvar, VAR2),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    get_attr1(VARIABLES, VARS),
    maximum(MAX, VARS),
    VAR1 #>  VAR2,
    VAR1 #=< MAX ,
    minimum_greater_than1(VARS, VAR2, MAX, UARS),
    minimum(VAR1, UARS).

minimum_greater_than1([], _, _, []).
minimum_greater_than1([V|R], VAR2, MAX, [U|S]) :-
    fd_min(V  , Min),
    fd_max(MAX, Max),
    U in Min..Max,
    V #=< VAR2 #=> U #= MAX,
    V #>  VAR2 #=> U #= V  ,
    minimum_greater_than1(R, VAR2, MAX, S).

% 0: VAR<VAR1 and VAR<=VAR2
% 1: VAR=VAR1 and VAR<=VAR2
% 2: VAR>VAR1 and VAR<=VAR2
% 3: VAR<VAR1 and VAR> VAR2
% 4: VAR=VAR1 and VAR> VAR2
% 5: VAR>VAR1 and VAR> VAR2
minimum_greater_than_a(FLAG, VAR1, VAR2, VARIABLES) :-
    check_type(dvar, VAR1),
    check_type(dvar, VAR2),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    VAR1 #> VAR2,
    minimum_greater_than_signature(VARIABLES, SIGNATURE, VAR1, VAR2),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE,
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,s),
                           arc(s,2,s),
                           arc(s,5,s),
                           arc(s,4,t),
                           arc(t,0,t),
                           arc(t,1,t),
                           arc(t,2,t),
                           arc(t,4,t),
                           arc(t,5,t)],
                           [],[],[]),
    automaton_bool(FLAG, [0,1,2,3,4,5], AUTOMATON).

minimum_greater_than_signature([], [], _, _).
minimum_greater_than_signature([[var-VAR]|VARs], [S|Ss], VAR1, VAR2) :-
    S in 0..5,
    VAR #< VAR1 #/\ VAR #=< VAR2 #<=> S #= 0,
    VAR #= VAR1 #/\ VAR #=< VAR2 #<=> S #= 1,
    VAR #> VAR1 #/\ VAR #=< VAR2 #<=> S #= 2,
    VAR #< VAR1 #/\ VAR #>  VAR2 #<=> S #= 3,
    VAR #= VAR1 #/\ VAR #>  VAR2 #<=> S #= 4,
    VAR #> VAR1 #/\ VAR #>  VAR2 #<=> S #= 5,
    minimum_greater_than_signature(VARs, Ss, VAR1, VAR2).
