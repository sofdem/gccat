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

:-dynamic full_group_a/8.

ctr_date(full_group,['20121025']).

ctr_origin(full_group, 'Inspired by %c', [group]).

ctr_arguments(full_group,
              ['NGROUP'-dvar                   ,
               'MIN_SIZE'-dvar                 ,
               'MAX_SIZE'-dvar                 ,
               'MIN_DIST'-dvar                 ,
               'MAX_DIST'-dvar                 ,
               'NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(full_group,
                 [items('VARIABLES',reverse),
                  items('VALUES',all),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_synonyms(full_group, [group_without_border]).

ctr_restrictions(full_group,
                 ['NGROUP'   >= 0                  ,
                  'MIN_SIZE' >= 0                  ,
                  'MAX_SIZE' >= 'MIN_SIZE'         ,
                  'MIN_DIST' >= 0                  ,
                  'MAX_DIST' >= 'MIN_DIST'         ,
                  'MAX_DIST' =< size('VARIABLES')-2,
                  'NVAL'     >= 'MAX_SIZE'         ,
                  'NVAL'     >= 'NGROUP'           ,
                  'NVAL'     =< size('VARIABLES')-2,
                  required('VARIABLES',var)        ,
                  required('VALUES',val)           ,
                  distinct('VALUES',val)           ]).

ctr_typical(full_group,
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

ctr_pure_functional_dependency(full_group, []).
ctr_functional_dependency(full_group, 1, [7,8]).
ctr_functional_dependency(full_group, 2, [7,8]).
ctr_functional_dependency(full_group, 3, [7,8]).
ctr_functional_dependency(full_group, 4, [7,8]).
ctr_functional_dependency(full_group, 5, [7,8]).
ctr_functional_dependency(full_group, 6, [7,8]).

ctr_example(full_group,
            full_group(2,2,3,1,1,5,
                       [[var-0],[var-1],[var-2],[var-6],[var-2],[var-7],[var-4],[var-8],[var-9]],
                       [[val-0],[val-2],[val-4],[val-6],[val-8]])).

ctr_see_also(full_group,
 [link('common keyword', group, '%k,%k', ['timetabling constraint', 'sequence'])]).

ctr_key_words(full_group,['timetabling constraint'             ,
                          'sequence'                           ,
                          'automaton'                          ,
                          'automaton with counters'            ,
			  'automaton with same input symbol'   ,
                          'reverse of a constraint'            ,
		          'glue matrix'                        ,
                          'alpha-acyclic constraint network(2)',
                          'alpha-acyclic constraint network(3)',
                          'functional dependency'              ,
		          'pure functional dependency'         ]).

ctr_eval(full_group, [checker(full_group_c) ,
		      automata(full_group_a)]).

full_group_a(NGROUP, MIN_SIZE, MAX_SIZE, MIN_DIST, MAX_DIST, NVAL, VARIABLES, VALUES) :-
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
    MAX_DIST #=< N-2,
    NVAL     #>= MAX_SIZE,
    NVAL     #>= NGROUP,
    NVAL     #=< N-2,
    all_different(VALS),
    full_group_ngroup(NGROUP, VARIABLES, VALUES),
    full_group_min_size(MIN_SIZE, VARIABLES, VALUES),
    full_group_max_size(MAX_SIZE, VARIABLES, VALUES),
    full_group_min_dist(MIN_DIST, VARIABLES, VALUES),
    full_group_max_dist(MAX_DIST, VARIABLES, VALUES),
    full_group_nval(NVAL, VARIABLES, VALUES).

full_group_ngroup(NGROUP, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    full_group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(j),sink(s)],
              [arc(s,1,s      ),
               arc(s,0,i      ),
               arc(i,0,i      ),
               arc(i,1,j      ),
               arc(j,1,j      ),
	       arc(j,0,i,[C+1])],
              [C],[0],[NGROUP]).

full_group_min_size(MIN_SIZE, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    full_group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(j),sink(k),sink(l),sink(s)],
              [arc(s,1,s               ),
               arc(s,0,i               ),
	       arc(i,0,i               ),
               arc(i,1,j,[C       ,D+1]),
               arc(j,1,j,[C       ,D+1]),
               arc(j,0,k,[D       ,0  ]),
               arc(k,0,k               ),
               arc(k,1,l,[C       ,D+1]),
               arc(l,1,l,[C       ,D+1]),
               arc(l,0,k,[min(C,D),0  ])],
              [C,D],[0,0],[MIN_SIZE,_]).

full_group_max_size(MAX_SIZE, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    full_group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(j),sink(k),sink(l),sink(s)],
              [arc(s,1,s               ),
               arc(s,0,i               ),
	       arc(i,0,i               ),
               arc(i,1,j,[C       ,D+1]),
               arc(j,1,j,[C       ,D+1]),
               arc(j,0,k,[D       ,0  ]),
               arc(k,0,k               ),
               arc(k,1,l,[C       ,D+1]),
               arc(l,1,l,[C       ,D+1]),
               arc(l,0,k,[max(C,D),0  ])],
              [C,D],[0,0],[MAX_SIZE,_]).

full_group_min_dist(MIN_DIST, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    full_group_signature_not_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(j),sink(k),sink(l),sink(s)],
              [arc(s,1,s               ),
               arc(s,0,i               ),
	       arc(i,0,i               ),
               arc(i,1,j,[C       ,D+1]),
               arc(j,1,j,[C       ,D+1]),
               arc(j,0,k,[D       ,0  ]),
               arc(k,0,k               ),
               arc(k,1,l,[C       ,D+1]),
               arc(l,1,l,[C       ,D+1]),
               arc(l,0,k,[min(C,D),0  ])],
              [C,D],[0,0],[MIN_DIST,_]).

full_group_max_dist(MAX_DIST, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    full_group_signature_not_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(j),sink(k),sink(l),sink(s)],
              [arc(s,1,s               ),
               arc(s,0,i               ),
	       arc(i,0,i               ),
               arc(i,1,j,[C       ,D+1]),
               arc(j,1,j,[C       ,D+1]),
               arc(j,0,k,[D       ,0  ]),
               arc(k,0,k               ),
               arc(k,1,l,[C       ,D+1]),
               arc(l,1,l,[C       ,D+1]),
               arc(l,0,k,[max(C,D),0  ])],
              [C,D],[0,0],[MAX_DIST,_]).

full_group_nval(NVAL, VARIABLES, VALUES) :-
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    full_group_signature_in(VARIABLES, SIGNATURE, SET_OF_VALUES),
    automaton(SIGNATURE, _,
              SIGNATURE,
              [source(s),sink(i),sink(j),sink(s)],
              [arc(s,1,s        ),
               arc(s,0,i        ),
	       arc(i,0,i        ),
	       arc(i,1,j,[C,D+1]),
	       arc(j,1,j,[C,D+1]),
	       arc(j,0,i,[C+D,0])],
              [C,D],[0,0],[NVAL,_]).

full_group_signature_in([], [], _).
full_group_signature_in([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> S,
    full_group_signature_in(VARs, Ss, SET_OF_VALUES).

full_group_signature_not_in([], [], _).
full_group_signature_not_in([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> #\ S,
    full_group_signature_not_in(VARs, Ss, SET_OF_VALUES).

full_group_c(NGROUP, MIN_SIZE, MAX_SIZE, MIN_DIST, MAX_DIST, NVAL, VARIABLES, VALUES) :-
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
    MAX_DIST #=< N-2,
    NVAL     #>= MAX_SIZE,
    NVAL     #>= NGROUP,
    NVAL     #=< N-2,
    sort(VALS, SVALS),
    length(VALS, M),
    length(SVALS, M),
    group_convert(VARS, BOOLS, NBOOLS, VALS),
    full_group_ngroup_c(BOOLS, s, 0, NGROUP),
    full_group_min_size_c(BOOLS, s, 0, 0, MIN_SIZE),
    full_group_max_size_c(BOOLS, s, 0, 0, MAX_SIZE),
    full_group_min_size_c(NBOOLS, s, 0, 0, MIN_DIST),
    full_group_max_size_c(NBOOLS, s, 0, 0, MAX_DIST),
    full_group_nval_c(BOOLS, s, 0, 0, NVAL).

full_group_ngroup_c([1|R], s, C, NGROUP) :-
    !,
    full_group_ngroup_c(R, s, C, NGROUP).
full_group_ngroup_c([0|R], s, C, NGROUP) :-
    !,
    full_group_ngroup_c(R, i, C, NGROUP).
full_group_ngroup_c([0|R], i, C, NGROUP) :-
    !,
    full_group_ngroup_c(R, i, C, NGROUP).
full_group_ngroup_c([1|R], i, C, NGROUP) :-
    !,
    full_group_ngroup_c(R, j, C, NGROUP).
full_group_ngroup_c([1|R], j, C, NGROUP) :-
    !,
    full_group_ngroup_c(R, j, C, NGROUP).
full_group_ngroup_c([0|R], j, C, NGROUP) :-
    !,
    C1 is C+1,
    full_group_ngroup_c(R, i, C1, NGROUP).
full_group_ngroup_c([], _, C, C).

full_group_min_size_c([1|R], s, C, D, MIN_SIZE) :-
    !,
    full_group_min_size_c(R, s, C, D, MIN_SIZE).
full_group_min_size_c([0|R], s, C, D, MIN_SIZE) :-
    !,
    full_group_min_size_c(R, i, C, D, MIN_SIZE).
full_group_min_size_c([0|R], i, C, D, MIN_SIZE) :-
    !,
    full_group_min_size_c(R, i, C, D, MIN_SIZE).
full_group_min_size_c([1|R], i, C, D, MIN_SIZE) :-
    !,
    D1 is D+1,
    full_group_min_size_c(R, j, C, D1, MIN_SIZE).
full_group_min_size_c([1|R], j, C, D, MIN_SIZE) :-
    !,
    D1 is D+1,
    full_group_min_size_c(R, j, C, D1, MIN_SIZE).
full_group_min_size_c([0|R], j, _C, D, MIN_SIZE) :-
    !,
    full_group_min_size_c(R, k, D, 0, MIN_SIZE).
full_group_min_size_c([0|R], k, C, D, MIN_SIZE) :-
    !,
    full_group_min_size_c(R, k, C, D, MIN_SIZE).
full_group_min_size_c([1|R], k, C, D, MIN_SIZE) :-
    !,
    D1 is D+1,
    full_group_min_size_c(R, l, C, D1, MIN_SIZE).
full_group_min_size_c([1|R], l, C, D, MIN_SIZE) :-
    !,
    D1 is D+1,
    full_group_min_size_c(R, l, C, D1, MIN_SIZE).
full_group_min_size_c([0|R], l, C, D, MIN_SIZE) :-
    !,
    C1 is min(C,D),
    full_group_min_size_c(R, k, C1, 0, MIN_SIZE).
full_group_min_size_c([], _, C, _, C).

full_group_max_size_c([1|R], s, C, D, MAX_SIZE) :-
    !,
    full_group_max_size_c(R, s, C, D, MAX_SIZE).
full_group_max_size_c([0|R], s, C, D, MAX_SIZE) :-
    !,
    full_group_max_size_c(R, i, C, D, MAX_SIZE).
full_group_max_size_c([0|R], i, C, D, MAX_SIZE) :-
    !,
    full_group_max_size_c(R, i, C, D, MAX_SIZE).
full_group_max_size_c([1|R], i, C, D, MAX_SIZE) :-
    !,
    D1 is D+1,
    full_group_max_size_c(R, j, C, D1, MAX_SIZE).
full_group_max_size_c([1|R], j, C, D, MAX_SIZE) :-
    !,
    D1 is D+1,
    full_group_max_size_c(R, j, C, D1, MAX_SIZE).
full_group_max_size_c([0|R], j, _C, D, MAX_SIZE) :-
    !,
    full_group_max_size_c(R, k, D, 0, MAX_SIZE).
full_group_max_size_c([0|R], k, C, D, MAX_SIZE) :-
    !,
    full_group_max_size_c(R, k, C, D, MAX_SIZE).
full_group_max_size_c([1|R], k, C, D, MAX_SIZE) :-
    !,
    D1 is D+1,
    full_group_max_size_c(R, l, C, D1, MAX_SIZE).
full_group_max_size_c([1|R], l, C, D, MAX_SIZE) :-
    !,
    D1 is D+1,
    full_group_max_size_c(R, l, C, D1, MAX_SIZE).
full_group_max_size_c([0|R], l, C, D, MAX_SIZE) :-
    !,
    C1 is max(C,D),
    full_group_max_size_c(R, k, C1, 0, MAX_SIZE).
full_group_max_size_c([], _, C, _, C).

full_group_nval_c([1|R], s, C, D, NVAL) :-
    !,
    full_group_nval_c(R, s, C, D, NVAL).
full_group_nval_c([0|R], s, C, D, NVAL) :-
    !,
    full_group_nval_c(R, i, C, D, NVAL).
full_group_nval_c([0|R], i, C, D, NVAL) :-
    !,
    full_group_nval_c(R, i, C, D, NVAL).
full_group_nval_c([1|R], i, C, D, NVAL) :-
    !,
    D1 is D+1,
    full_group_nval_c(R, j, C, D1, NVAL).
full_group_nval_c([1|R], j, C, D, NVAL) :-
    !,
    D1 is D+1,
    full_group_nval_c(R, j, C, D1, NVAL).
full_group_nval_c([0|R], j, C, D, NVAL) :-
    !,
    C1 is C+D,
    full_group_nval_c(R, i, C1, 0, NVAL).
full_group_nval_c([], _, C, _, C).
