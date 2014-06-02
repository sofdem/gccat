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

ctr_date(min_nvalue,['20000128','20030820','20060811']).

ctr_origin(min_nvalue, 'N.~Beldiceanu', []).

ctr_arguments(min_nvalue,
              ['MIN'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(min_nvalue,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(min_nvalue,
                 ['MIN' >= 1                ,
                  'MIN' =< size('VARIABLES'),
                  required('VARIABLES',var) ]).

ctr_typical(min_nvalue,
            [2*'MIN'                =< size('VARIABLES'),
             size('VARIABLES')      >  1                ,
             range('VARIABLES'^var) >  1                ]).

ctr_pure_functional_dependency(min_nvalue, []).
ctr_functional_dependency(min_nvalue, 1, [2]).

ctr_graph(min_nvalue,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['MIN_NSCC' = 'MIN'],
          []).

ctr_example(min_nvalue,
            [min_nvalue(2,[[var-9],[var-1],[var-7],[var-1],[var-1],[var-7],[var-7],[var-7],[var-7],[var-9]]),
	     min_nvalue(5,[[var-8],[var-8],[var-8],[var-8],[var-8]]),
	     min_nvalue(2,[[var-1],[var-8],[var-1],[var-8],[var-1]])]).

ctr_draw_example(min_nvalue,
                 ['VARIABLES'],
                 [[[var-9],[var-1],[var-7],[var-1],[var-1],
                   [var-7],[var-7],[var-7],[var-7],[var-9]]],
                 ['CLIQUE'],
                 [ 1-[1,10],
                   2-[2,4,5],
                   3-[3,6,7,8,9],
                   4-[2,4,5],
                   5-[2,4,5],
                   6-[3,6,7,8,9],
                   7-[3,6,7,8,9],
                   8-[3,6,7,8,9],
                   9-[3,6,7,8,9],
                  10-[1,10]],
                 ['MIN_NSCC'([1,10])],
                 '','MIN_NSCC=2',
                 [3,3,3.5,3]).

ctr_cond_imply(min_nvalue, atleast_nvalue, ['MIN' < size('VARIABLES')], ['NVAL' = 2], [none, 'VARIABLES']).

ctr_see_also(min_nvalue,
 [link('common keyword', among,              '%k',    ['counting constraint']),
  link('common keyword', nvalue,             '%k',    ['counting constraint']),
  link('common keyword', max_nvalue,         '%k',    ['counting constraint']),
  link('common keyword', global_cardinality, '%k,%k', ['value constraint','counting constraint']),
  link('common keyword', count,              '%k,%k', ['value constraint','counting constraint'])]).

ctr_key_words(min_nvalue,['value constraint'                ,
                          'counting constraint'             ,
                          'assignment'                      ,
                          'minimum number of occurrences'   ,
                          'minimum'                         ,
                          'automaton'                       ,
                          'automaton with array of counters',
                          'equivalence'                     ,
                          'functional dependency'           ,
		          'pure functional dependency'      ]).

ctr_persons(min_nvalue,['Beldiceanu N.']).

ctr_eval(min_nvalue, [checker(min_nvalue_c),
		      reformulation(min_nvalue_r)]).

ctr_sol(min_nvalue,2,0,2,9,[1-6,2-3]).
ctr_sol(min_nvalue,3,0,3,64,[1-60,3-4]).
ctr_sol(min_nvalue,4,0,4,625,[1-560,2-60,4-5]).
ctr_sol(min_nvalue,5,0,5,7776,[1-7470,2-300,5-6]).
ctr_sol(min_nvalue,6,0,6,117649,[1-113442,2-3780,3-420,6-7]).
ctr_sol(min_nvalue,7,0,7,2097152,[1-2058728,2-36456,3-1960,7-8]).
ctr_sol(min_nvalue,8,0,8,43046721,[1-42473664,2-566496,3-4032,4-2520,8-9]).

min_nvalue_c(0, []) :- !.
min_nvalue_c(MIN, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(1,N), MIN),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    samsort(VARS, SVARS),
    SVARS = [V|R],
    min_nvalue_seq_size(R, 1, V, N, M),
    MIN #= M.

min_nvalue_seq_size([], C, _, Best, Res) :-
    !,
    Res is min(C,Best).
min_nvalue_seq_size([V|R], C, V, Best, Res) :-
    !,
    C1 is C+1,
    min_nvalue_seq_size(R, C1, V, Best, Res).
min_nvalue_seq_size([V|R], C, Prev, Best, Res) :-
    C > 0,
    V =\= Prev,
    NewBest is min(C,Best),
    min_nvalue_seq_size(R, 1, V, NewBest, Res).

min_nvalue_r(0, []) :- !.
min_nvalue_r(MIN, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(1,N), MIN),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    union_dom_list_int(VARS, UnionDomainsVARS),
    NSquare is N*N,
    length(UnionDomainsVARS, SizeUnion),
    (SizeUnion =< NSquare ->
	balance1(UnionDomainsVARS, N, VALS, _OCCS, OCCS1),
	eval(global_cardinality(VARIABLES, VALS))
    ;
	balance2(VARS, N, VARS, OCCS1)
    ),
    eval(minimum(MIN,OCCS1)).
