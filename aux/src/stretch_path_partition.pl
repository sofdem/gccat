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

ctr_date(stretch_path_partition,['20091106']).

ctr_origin(stretch_path_partition, 'Derived from %c.', [stretch_path]).

ctr_types(stretch_path_partition,
          ['VALUES'-collection(val-int)]).

ctr_arguments(stretch_path_partition,
              ['VARIABLES'-collection(var-dvar)                       ,
               'PARTLIMITS'-collection(p-'VALUES', lmin-int, lmax-int)]).

ctr_exchangeable(stretch_path_partition,
                 [items('VARIABLES',reverse),
                  items('PARTLIMITS',all),
                  items('PARTLIMITS'^p,all),
                  vals(['VARIABLES'^var,'PARLIMITS'^p^val],int,=\=,all,dontcare)]).

ctr_synonyms(stretch_path_partition,[stretch]).

ctr_restrictions(stretch_path_partition,
                 [size('VALUES')         >= 1                ,
                  required('VALUES',val)                     ,
                  distinct('VALUES',val)                     ,
                  size('VARIABLES')       > 0                ,
                  required('VARIABLES',var)                  ,
                  size('PARTLIMITS')      > 0                ,
                  required('PARTLIMITS',[p,lmin,lmax])       ,
                  'PARTLIMITS'^lmin      >= 0                ,
                  'PARTLIMITS'^lmin      =< 'PARTLIMITS'^lmax,
                  'PARTLIMITS'^lmin      =< size('VARIABLES')]).

ctr_typical(stretch_path_partition,
            [size('VARIABLES')      > 1                 ,
             range('VARIABLES'^var) > 1                 ,
             size('VARIABLES')      > size('PARTLIMITS'),
             size('PARTLIMITS')     > 1                 ,
             sum('PARTLIMITS'^lmin) =< size('VARIABLES'),
             'PARTLIMITS'^lmax      =< size('VARIABLES')]).

ctr_example(stretch_path_partition,
            stretch_path_partition([[var-1],[var-2],[var-0],[var-0],
                                    [var-2],[var-2],[var-2],[var-0]],
                                   [[p-[[val-1], [val-2]], lmin-2, lmax-4],
                                    [p-[[val-3]],          lmin-0, lmax-2]])).

ctr_see_also(stretch_path_partition,
 [link('specialisation', stretch_path, '%e replaced by %e', [in_list(variable,partition),variable]),
  link('common keyword', pattern,      '%k',                ['sliding sequence constraint'])]).

ctr_key_words(stretch_path_partition,['timetabling constraint'          ,
                                      'sliding sequence constraint'     ,
                                      'sequence'                        ,
                                      'arc-consistency'                 ,
                                      'consecutive loops are connected' ,
                                      'Berge-acyclic constraint network',
                                      'automaton'                       ,
                                      'automaton without counters'      ,
                                      'reified automaton constraint'    ,
                                      'partition'                       ]).

ctr_eval(stretch_path_partition, [reformulation(stretch_path_partition_r),
                                      automaton(stretch_path_partition_a)]).

stretch_path_partition_r(VARIABLES, PARTLIMITS) :-
    length(VARIABLES, N),
    N > 0,
    collection(VARIABLES, [dvar]),
    collection(PARTLIMITS, [col_len_gteq(1, [int]),int(0,N),int]),
    get_attr1(VARIABLES, VARS),
    get_col_attr1(PARTLIMITS, 1, PVALS),
    get_attr2(PARTLIMITS, LMINS),
    get_attr3(PARTLIMITS, LMAXS),
    length(PVALS, M),
    M > 0,
    check_lesseq(LMINS, LMAXS),
    flattern(PVALS, VALS),
    all_different(VALS),
    get_partition_var(VARS, PVALS, PVARS, M),
    gen_collection(PVARS, var, PVARIABLES),
    stretch_path_partition_values(PARTLIMITS, 1, VALUES),
    eval(stretch_path(PVARIABLES, VALUES)).

stretch_path_partition_values([], _, []) :-
    !.
stretch_path_partition_values([[_, lmin-LMIN, lmax-LMAX]|R], V, [[val-V, lmin-LMIN, lmax-LMAX]|S]) :-
    V1 is V+1,
    stretch_path_partition_values(R, V1, S).

stretch_path_partition_a(FLAG, VARIABLES, PARTLIMITS) :-
    stretch_path_partition_get_a(VARIABLES, PARTLIMITS, AUTOMATON, ALPHABET),
    automaton_bool(FLAG, ALPHABET, AUTOMATON).

stretch_path_partition_get_a(VARIABLES, PARTLIMITS, AUTOMATON, ALPHABET) :-
    length(VARIABLES, N),
    N > 0,
    collection(VARIABLES, [dvar]),
    collection(PARTLIMITS, [col_len_gteq(1, [int]),int(0,N),int]),
    get_attr1(VARIABLES, VARS),
    get_col_attr1(PARTLIMITS, 1, PVALS),
    get_attr2(PARTLIMITS, LMINS),
    get_attr3(PARTLIMITS, LMAXS),
    length(PVALS, M),
    M > 0,
    check_lesseq(LMINS, LMAXS),
    flattern(PVALS, VALS),
    all_different(VALS),
    stretch_lmin(LMINS, LMINS1),
    stretch_reduce_lmax(LMAXS, N, LMAXSR),
    stretch_gen_states(LMINS1, LMAXSR, N, 1, STATES),
    stretch_gen_transitions(1, M, LMINS1, LMAXSR, LMINS1, LMAXSR, N, TRANSITIONS),
    get_minimum(VARS, MINVARS),
    get_maximum(VARS, MAXVARS),
    sort(VALS, SVALS),
    SVALS = [MINVALS|_],
    last(SVALS, MAXVALS),
    VALS_RANGE is MAXVALS-MINVALS+1,
    (VALS_RANGE =  M,
     MINVALS    =< MINVARS,
     MAXVARS    =< MAXVALS
        -> COMP_VALS = []
        ;  stretch_path_partition_complement(MINVARS, MAXVARS, VALS, COMP_VALS)),
    stretch_path_partition_expand_transitions(TRANSITIONS, COMP_VALS, PVALS, EXPANDED_TRANSITIONS),
    AUTOMATON = automaton(VARS, _, VARS, STATES, EXPANDED_TRANSITIONS, [], [], []),
    append(VARS, SVALS, ALL_VALS),
    union_dom_list_int(ALL_VALS, ALPHABET).

stretch_path_partition_complement(MIN, MAX, _, []) :-
    MIN > MAX,
    !.
stretch_path_partition_complement(MIN, MAX, VALS, C) :-
    member(MIN, VALS),
    !,
    MIN1 is MIN+1,
    stretch_path_partition_complement(MIN1, MAX, VALS, C).
stretch_path_partition_complement(MIN, MAX, VALS, [MIN|C]) :-
    MIN1 is MIN+1,
    stretch_path_partition_complement(MIN1, MAX, VALS, C).

stretch_path_partition_expand_transitions([], _, _, []) :-
    !.
stretch_path_partition_expand_transitions([arc(_,0,_)|R], [], PVALS, S) :-
    !,
    stretch_path_partition_expand_transitions(R, [], PVALS, S).
stretch_path_partition_expand_transitions([arc(Si,0,Sj)|R], [CV|CR], PVALS, TS) :-    
    !,
    stretch_path_partition_tr([CV|CR], arc(Si,0,Sj), T),
    stretch_path_partition_expand_transitions(R, [CV|CR], PVALS, S),
    append(T, S, TS).
stretch_path_partition_expand_transitions([arc(Si,E,Sj)|R], CL, PVALS, TS) :-
    nth1(E, PVALS, VALS),
    stretch_path_partition_tr(VALS, arc(Si,E,Sj), T),
    stretch_path_partition_expand_transitions(R, CL, PVALS, S),
    append(T, S, TS).

stretch_path_partition_tr([], _, []).
stretch_path_partition_tr([VAL|R], arc(Si,E,Sj), [arc(Si,VAL,Sj)|S]) :-
    stretch_path_partition_tr(R, arc(Si,E,Sj), S).
