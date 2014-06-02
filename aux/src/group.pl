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

:-dynamic group_a/8.

ctr_date(group,['20000128','20030820','20040530','20060809']).

ctr_origin(group, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(group,
              ['NGROUP'-dvar                   ,
               'MIN_SIZE'-dvar                 ,
               'MAX_SIZE'-dvar                 ,
               'MIN_DIST'-dvar                 ,
               'MAX_DIST'-dvar                 ,
               'NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(group,
                 [items('VARIABLES',reverse),
                  items('VALUES',all),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_restrictions(group,
                 ['NGROUP'   >= 0                ,
                  'MIN_SIZE' >= 0                ,
                  'MAX_SIZE' >= 'MIN_SIZE'       ,
                  'MIN_DIST' >= 0                ,
                  'MAX_DIST' >= 'MIN_DIST'       ,
                  'MAX_DIST' =< size('VARIABLES'),
                  'NVAL'     >= 'MAX_SIZE'       ,
                  'NVAL'     >= 'NGROUP'         ,
                  'NVAL'     =< size('VARIABLES'),
                  required('VARIABLES',var)      ,
                  required('VALUES',val)         ,
                  distinct('VALUES',val)         ]).

ctr_typical(group,
            ['NGROUP'               > 0                ,
             'MIN_SIZE'             > 0                ,
             'MAX_SIZE'             > 'MIN_SIZE'       ,
             'MIN_DIST'             > 0                ,
             'MAX_DIST'             > 'MIN_DIST'       ,
             'MAX_DIST'             < size('VARIABLES'),
             'NVAL'                 > 'MAX_SIZE'       ,
             'NVAL'                 > 'NGROUP'         ,
             'NVAL'                 < size('VARIABLES'),
             size('VARIABLES')      > 1                ,
             range('VARIABLES'^var) > 1                ,
             size('VALUES')         > 0                ,
             size('VARIABLES')      > size('VALUES')   ]).

ctr_pure_functional_dependency(group, []).
ctr_functional_dependency(group, 1, [7,8]).
ctr_functional_dependency(group, 2, [7,8]).
ctr_functional_dependency(group, 3, [7,8]).
ctr_functional_dependency(group, 4, [7,8]).
ctr_functional_dependency(group, 5, [7,8]).
ctr_functional_dependency(group, 6, [7,8]).

ctr_graph(group,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2),
           'LOOP'>>collection(variables1,variables2)],
          [in(variables1^var,'VALUES'),
           in(variables2^var,'VALUES')],
          ['NCC'     = 'NGROUP'  ,
           'MIN_NCC' = 'MIN_SIZE',
           'MAX_NCC' = 'MAX_SIZE',
           'NVERTEX' = 'NVAL'    ],
          []).

ctr_graph(group,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2),
           'LOOP'>>collection(variables1,variables2)],
          [not_in(variables1^var,'VALUES'),
           not_in(variables2^var,'VALUES')],
          ['MIN_NCC' = 'MIN_DIST',
           'MAX_NCC' = 'MAX_DIST'],
          []).

ctr_example(group,
            group(2,1,2,2,4,3,
                  [[var-2],[var-8],[var-1],[var-7],[var-4],
                   [var-5],[var-1],[var-1],[var-1]],
                  [[val-0],[val-2],[val-4],[val-6],[val-8]])).

ctr_draw_example(group,
                 ['VARIABLES'],
                 [[[var-2],[var-8],[var-1],[var-7],[var-4],
                   [var-5],[var-1],[var-1],[var-1]]],
                 ['PATH','LOOP'],
                 [1-1,1-2,2-2,5-5],
                 ['MIN_NCC'([5]),'MAX_NCC'([1,2]),'NVERTEX'],
                 '','NCC=2\\nMIN_NCC=1\\nMAX_NCC=2\\nNVERTEX=3',
                 [1.8,3.5,1.7,1.7]).

ctr_see_also(group,
 [link('common keyword',            global_contiguity,          '%k',    ['sequence'],'\\\\ '),
  link('common keyword',            multi_global_contiguity,    '%k',    ['sequence'],'\\\\ '),
  link('common keyword',            group_skip_isolated_item,   '%k,%k', ['timetabling constraint', 'sequence'],'\\\\ '),
  link('common keyword',            change_continuity,          '%k,%k', ['timetabling constraint', 'sequence'],'\\\\ '),
  link('common keyword',            full_group,                 '%k,%k', ['timetabling constraint', 'sequence']),
  link('common keyword',            stretch_path,               '%k,%k', ['timetabling constraint', 'sequence']),
  link('common keyword',            stretch_circuit,            '%k',    ['timetabling constraint'],'\\\\ '),
  link('common keyword',            pattern,                    '%k',    ['timetabling constraint']),
  link('shift of concept',          consecutive_groups_of_ones, '',      []),
  link('used in graph description', in,                         '',      []),
  link('used in graph description', not_in,                     '',      [])]).

ctr_key_words(group,['timetabling constraint'             ,
                     'sequence'                           ,
                     'connected component'                ,
                     'automaton'                          ,
                     'automaton with counters'            ,
                     'automaton with same input symbol'   ,
                     'reverse of a constraint'            ,
		     'glue matrix'                        ,
                     'alpha-acyclic constraint network(2)',
                     'alpha-acyclic constraint network(3)',
                     'vpartition'                         ,
                     'consecutive loops are connected'    ,
                     'functional dependency'              ,
		     'pure functional dependency'         ]).

ctr_eval(group, [checker(group_c) ,
		 automata(group_a)]).

group_a(NGROUP, MIN_SIZE, MAX_SIZE, MIN_DIST, MAX_DIST, NVAL, VARIABLES, VALUES) :-
    check_type(dvar, NGROUP),
    check_type(dvar, MIN_SIZE),
    check_type(dvar, MAX_SIZE),
    check_type(dvar, MIN_DIST),
    check_type(dvar, MAX_DIST),
    check_type(dvar, NVAL),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int]),
    length(VARIABLES, N),
    get_attr1(VALUES, VALS),
    NGROUP   #>= 0,
    MIN_SIZE #>= 0,
    MAX_SIZE #>= MIN_SIZE,
    MIN_DIST #>= 0,
    MAX_DIST #>= MIN_DIST,
    MAX_DIST #=< N,
    NVAL     #>= MAX_SIZE,
    NVAL     #>= NGROUP,
    NVAL     #=< N,
    all_different(VALS),
    group_ngroup(NGROUP, VARIABLES, VALUES),
    group_min_size(MIN_SIZE, VARIABLES, VALUES),
    group_max_size(MAX_SIZE, VARIABLES, VALUES),
    group_min_dist(MIN_DIST, VARIABLES, VALUES),
    group_max_dist(MAX_DIST, VARIABLES, VALUES),
    group_nval(NVAL, VARIABLES, VALUES).

group_ngroup(NGROUP, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,i,[C+1]),
               arc(i,1,i      ),
               arc(i,0,s      )],
              [C],[0],[NGROUP]).

group_min_size(MIN_SIZE, VARIABLES, VALUES) :-
    length(VARIABLES, NVAR),
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    MIN_SIZE #= min(C1,D1),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(s)],
              [arc(s,0,s               ),
               arc(s,1,i,[C       ,1  ]),
               arc(i,1,i,[C       ,D+1]),
               arc(i,0,s,[min(C,D),D  ])],
              [C,D],[NVAR,0],[C1,D1]).

group_max_size(MAX_SIZE, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    MAX_SIZE #= max(C1,D1),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,1,s,[C       ,D+1]),
               arc(s,0,s,[max(C,D),0  ])],
              [C,D],[0,0],[C1,D1]).

group_min_dist(MIN_DIST, VARIABLES, VALUES) :-
    length(VARIABLES, NVAR),
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    group_signature_not_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    MIN_DIST #= min(C1,D1),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(s)],
              [arc(s,0,s               ),
               arc(s,1,i,[C       ,1  ]),
               arc(i,1,i,[C       ,D+1]),
               arc(i,0,s,[min(C,D),D  ])],
              [C,D],[NVAR,0],[C1,D1]).

group_max_dist(MAX_DIST, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    group_signature_not_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    MAX_DIST #= max(C1,D1),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,1,s,[C       ,D+1]),
               arc(s,0,s,[max(C,D),0  ])],
              [C,D],[0,0],[C1,D1]).

group_nval(NVAL, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE,
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[NVAL]).

group_signature_in([], [], _).
group_signature_in([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> S,
    group_signature_in(VARs, Ss, SET_OF_VALUES).

group_signature_not_in([], [], _).
group_signature_not_in([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> #\ S,
    group_signature_not_in(VARs, Ss, SET_OF_VALUES).

group_c(NGROUP, MIN_SIZE, MAX_SIZE, MIN_DIST, MAX_DIST, NVAL, VARIABLES, VALUES) :-
    check_type(dvar, NGROUP),
    check_type(dvar, MIN_SIZE),
    check_type(dvar, MAX_SIZE),
    check_type(dvar, MIN_DIST),
    check_type(dvar, MAX_DIST),
    check_type(dvar, NVAL),
    collection(VARIABLES, [int]),
    collection(VALUES, [int]),
    length(VARIABLES, N),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),    
    NGROUP   #>= 0,
    MIN_SIZE #>= 0,
    MAX_SIZE #>= MIN_SIZE,
    MIN_DIST #>= 0,
    MAX_DIST #>= MIN_DIST,
    MAX_DIST #=< N,
    NVAL     #>= MAX_SIZE,
    NVAL     #>= NGROUP,
    NVAL     #=< N,
    sort(VALS, SVALS),
    length(VALS, M),
    length(SVALS, M),
    group_convert(VARS, BOOLS, NBOOLS, VALS),    
    group_ngroup_c(BOOLS, s, 0, NGROUP),
    group_min_size_c(BOOLS, s, N, 0, MIN_SIZE),
    group_max_size_c(BOOLS, 0, 0, MAX_SIZE),
    group_min_size_c(NBOOLS, s, N, 0, MIN_DIST),
    group_max_size_c(NBOOLS, 0, 0, MAX_DIST),
    group_nval_c(BOOLS, 0, NVAL).

group_ngroup_c([0|R], s, C, NGROUP) :-
    !,
    group_ngroup_c(R, s, C, NGROUP).
group_ngroup_c([1|R], s, C, NGROUP) :-
    !,
    C1 is C+1,
    group_ngroup_c(R, i, C1, NGROUP).
group_ngroup_c([1|R], i, C, NGROUP) :-
    !,
    group_ngroup_c(R, i, C, NGROUP).
group_ngroup_c([0|R], i, C, NGROUP) :-
    !,
    group_ngroup_c(R, s, C, NGROUP).
group_ngroup_c([], _, C, C).

group_min_size_c([0|R], s, C, D, MIN_SIZE) :-
    !,
    group_min_size_c(R, s, C, D, MIN_SIZE).
group_min_size_c([1|R], s, C, _D, MIN_SIZE) :-
    !,
    group_min_size_c(R, i, C, 1, MIN_SIZE).
group_min_size_c([1|R], i, C, D, MIN_SIZE) :-
    !,
    D1 is D+1,
    group_min_size_c(R, i, C, D1, MIN_SIZE).
group_min_size_c([0|R], i, C, D, MIN_SIZE) :-
    !,
    C1 is min(C,D),
    group_min_size_c(R, s, C1, D, MIN_SIZE).
group_min_size_c([], _, C, D, MIN_SIZE) :-
    M is min(C,D),
    MIN_SIZE #= M.

group_max_size_c([1|R], C, D, MAX_SIZE) :-
    !,
    D1 is D+1,
    group_max_size_c(R, C, D1, MAX_SIZE).
group_max_size_c([0|R], C, D, MAX_SIZE) :-
    !,
    C1 is max(C,D),
    group_max_size_c(R, C1, 0, MAX_SIZE).
group_max_size_c([], C, D, MAX_SIZE) :-
    M is max(C,D),
    MAX_SIZE #= M.

group_nval_c([0|R], C, NVAL) :-
    !,
    group_nval_c(R, C, NVAL).
group_nval_c([1|R], C, NVAL) :-
    !,
    C1 is C+1,
    group_nval_c(R, C1, NVAL).
group_nval_c([], C, C).
