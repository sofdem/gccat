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

ctr_date(next_greater_element,['20030820','20040530','20060812']).

ctr_origin(next_greater_element, 'M.~Carlsson', []).

ctr_arguments(next_greater_element,
              ['VAR1'-dvar                       ,
               'VAR2'-dvar                       ,
               'VARIABLES'-collection(var-dvar)]).

ctr_restrictions(next_greater_element,
                 ['VAR1'            < 'VAR2',
                  size('VARIABLES') > 0     ,
                  required('VARIABLES',var) ]).

ctr_typical(next_greater_element,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_derived_collections(next_greater_element,
                        [col('V'-collection(var-dvar),
                             [item(var-'VAR1')])]).

ctr_graph(next_greater_element,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables1^var < variables2^var],
          ['NARC' = size('VARIABLES')-1],
          []).

ctr_graph(next_greater_element,
          ['V','VARIABLES'],
          2,
          ['PRODUCT'>>collection(v,variables)],
          [v^var < variables^var],
          ['NARC' > 0],
          [],
          ['SUCC'>>[source,variables]],
          [minimum('VAR2',variables)]).

ctr_example(next_greater_element,
            next_greater_element(7,8,
                                 [[var-3],[var-5],[var-8],[var-9]])).

ctr_draw_example(next_greater_element,
                 ['V','VARIABLES'],
                 [[[var-7]],
                  [[var-3],[var-5],[var-8],[var-9]]],
                 ['PRODUCT'],
                 [1-[3,4]],
                 ['NARC'],
                 '','NARC=2',
                 [2.4,1.9,1.2,1.2]).

ctr_see_also(next_greater_element,
 [link('implies',        minimum_greater_than, '',                                            []),
  link('common keyword', minimum_greater_than, '%k',                                          ['order constraint']),
  link('related',        next_element,         'allow to iterate over the values of a table', [])]).

ctr_key_words(next_greater_element,['order constraint'  ,
                                    'minimum'           ,
                                    'data constraint'   ,
                                    'table'             ,
                                    'derived collection']).

ctr_persons(next_greater_element,['Carlsson M.'  ,
                                  'Beldiceanu N.']).

ctr_eval(next_greater_element, [reformulation(next_greater_element_r)]).

next_greater_element_r(VAR1, VAR2, VARIABLES) :-
    check_type(dvar, VAR1),
    check_type(dvar, VAR2),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N > 0,
    get_attr1(VARIABLES, VARS),
    maximum(MAX, VARS),
    VAR2 #>  VAR1,
    VAR2 #=< MAX ,
    next_greater_element1(VARS, VAR1, MAX, UARS),
    minimum(VAR2, UARS).

next_greater_element1([V], VAR1, MAX, [U]) :- !,
    fd_min(V  , Min),
    fd_max(MAX, Max),
    U in Min..Max,
    V #=< VAR1 #=> U #= MAX,
    V #>  VAR1 #=> U #= V  .
next_greater_element1([V1,V2|R], VAR1, MAX, [U1|S]) :-
    V1 #< V2,
    fd_min(V1 , Min),
    fd_max(MAX, Max),
    U1 in Min..Max,
    V1 #=< VAR1 #=> U1 #= MAX,
    V1 #>  VAR1 #=> U1 #= V1 ,
    next_greater_element1([V2|R], VAR1, MAX, S).
