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

ctr_date(max_nvalue,['20000128','20030820','20060811']).

ctr_origin(max_nvalue, 'Derived from %c.', [nvalue]).

ctr_arguments(max_nvalue,
              ['MAX'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(max_nvalue,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(max_nvalue,
                 ['MAX' >= 1                ,
                  'MAX' =< size('VARIABLES'),
                  required('VARIABLES',var) ]).

ctr_typical(max_nvalue,
            ['MAX'                  > 1                ,
             'MAX'                  < size('VARIABLES'),
             size('VARIABLES')      > 1                ,
             range('VARIABLES'^var) > 1                ]).

ctr_pure_functional_dependency(max_nvalue, []).
ctr_functional_dependency(max_nvalue, 1, [2]).

ctr_graph(max_nvalue,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['MAX_NSCC' = 'MAX'],
          []).

ctr_example(max_nvalue,
            [max_nvalue(3,[[var-9],[var-1],[var-7],[var-1],[var-1],[var-6],[var-7],[var-7],[var-4],[var-9]]),
             max_nvalue(1,[[var-9],[var-1],[var-7],[var-3],[var-2],[var-6]]),
	     max_nvalue(6,[[var-5],[var-5],[var-5],[var-5],[var-5],[var-5]])]).

ctr_draw_example(max_nvalue,
                 ['VARIABLES'],
                 [[[var-9],[var-1],[var-7],[var-1],[var-1],
                   [var-6],[var-7],[var-7],[var-4],[var-9]]],
                 ['CLIQUE'],
                 [1-[1,10],
                  2-[2,4,5],
                  3-[3,7,8],
                  4-[2,4,5],
                  5-[2,4,5],
                  6-6,
                  7-[3,7,8],
                  8-[3,7,8],
                  9-9,
                  10-[1,10]],
                 ['MAX_NSCC'([3,7,8])],
                 '','MAX_NSCC=3',
                 [3,3,3.5,3]).

ctr_see_also(max_nvalue,
 [link('common keyword', count,              '%k,%k', ['value constraint','counting constraint']),
  link('common keyword', among,              '%k',    ['counting constraint']),
  link('common keyword', nvalue,             '%k',    ['counting constraint']),
  link('common keyword', min_nvalue,         '%k',    ['counting constraint']),
  link('common keyword', global_cardinality, '%k,%k', ['value constraint', 'counting constraint'])]).

ctr_key_words(max_nvalue,['value constraint'                ,
                          'counting constraint'             ,
                          'assignment'                      ,
                          'maximum number of occurrences'   ,
                          'maximum'                         ,
                          'automaton'                       ,
                          'automaton with array of counters',
                          'equivalence'                     ,
                          'functional dependency'           ,
		          'pure functional dependency'      ]).

ctr_eval(max_nvalue, [      checker(max_nvalue_c),
		      reformulation(max_nvalue_r)]).

ctr_sol(max_nvalue,2,0,2,9,[1-6,2-3]).
ctr_sol(max_nvalue,3,0,3,64,[1-24,2-36,3-4]).
ctr_sol(max_nvalue,4,0,4,625,[1-120,2-420,3-80,4-5]).
ctr_sol(max_nvalue,5,0,5,7776,[1-720,2-5400,3-1500,4-150,5-6]).
ctr_sol(max_nvalue,6,0,6,117649,[1-5040,2-78750,3-29820,4-3780,5-252,6-7]).
ctr_sol(max_nvalue,7,0,7,2097152,[1-40320,2-1305360,3-646800,4-96040,5-8232,6-392,7-8]).
ctr_sol(max_nvalue,8,0,8,43046721,[1-362880,2-24449040,3-15382080,4-2577960,5-258048,6-16128,7-576,8-9]).

max_nvalue_c(0, []) :- !.
max_nvalue_c(MAX, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(1,N), MAX),
    collection(VARIABLES, [int]),
    get_attr1(VARIABLES, VARS),
    samsort(VARS, SVARS),
    SVARS = [V|R],
    max_nvalue_seq_size(R, 1, V, 1, M),
    MAX #= M.

max_nvalue_r(0, []) :- !.
max_nvalue_r(MAX, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(1,N), MAX),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    union_dom_list_int(VARS, UnionDomainsVARS),
    NSquare is N*N,
    length(UnionDomainsVARS, SizeUnion),
    (SizeUnion =< NSquare ->
	balance1(UnionDomainsVARS, N, VALS, OCCS, _OCCS1),
	eval(global_cardinality(VARIABLES, VALS))
    ;
	balance2(VARS, N, VARS, OCCS)
    ),
    eval(maximum(MAX,OCCS)).
