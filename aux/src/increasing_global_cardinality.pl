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

ctr_date(increasing_global_cardinality,['20091015']).

ctr_origin(increasing_global_cardinality, 'Conjoin %c and %c.', [global_cardinality_low_up,increasing]).

ctr_synonyms(increasing_global_cardinality, [increasing_global_cardinality_low_up,
                                             increasing_gcc                      ,
                                             increasing_gcc_low_up               ]).

ctr_arguments(increasing_global_cardinality,
              ['VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int, omin-int, omax-int)]).

ctr_exchangeable(increasing_global_cardinality,
                 [items('VALUES',all)]).

ctr_restrictions(increasing_global_cardinality,
                 [required('VARIABLES',var)         ,
                  increasing('VARIABLES')           ,
                  size('VALUES') > 0                ,
                  required('VALUES',[val,omin,omax]),
                  distinct('VALUES',val)            ,
                  'VALUES'^omin >= 0                ,
                  'VALUES'^omax =< size('VARIABLES'),
                  'VALUES'^omin =< 'VALUES'^omax    ]).

ctr_typical(increasing_global_cardinality,
            [size('VARIABLES')      > 1                ,
             range('VARIABLES'^var) > 1                ,
             size('VALUES')         > 1                ,
             'VALUES'^omin         =< size('VARIABLES'),
             'VALUES'^omax          > 0                ,
             'VALUES'^omax         =< size('VARIABLES'),
             size('VARIABLES')      > size('VALUES')   ]).

ctr_functional_dependency(increasing_nvalue, 1, [2]).

ctr_graph(increasing_global_cardinality,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var = 'VALUES'^val],
          ['NVERTEX' >= 'VALUES'^omin,
           'NVERTEX' =< 'VALUES'^omax],
          []).

ctr_example(increasing_global_cardinality,
            increasing_global_cardinality([[var-3],[var-3],[var-6],[var-8]],
                                          [[val-3, omin-2, omax-3],
                                           [val-5, omin-0, omax-1],
                                           [val-6, omin-1, omax-2]])).

ctr_draw_example(increasing_global_cardinality,
                 ['VARIABLES'],
                 [[[var-3],[var-3],[var-6],[var-8]]],
                 ['SELF'],
                 [1-1,2-2,3-3],
                 ['NVERTEX',
                  'FOREACH'('VALUES',[3-[1,2],5-[],6-[3]])],
                 '','3:NVERTEX=2, 5:NVERTEX=0, 6:NVERTEX=1',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(increasing_global_cardinality,
 [link('implies', increasing,                 '', []),
  link('implies', global_cardinality_low_up,  '', []),
  link('related', ordered_global_cardinality, '', [])]).

ctr_key_words(increasing_global_cardinality,['value constraint'                ,
                                             'assignment'                      ,
                                             'Berge-acyclic constraint network',
                                             'automaton'                       ,
                                             'automaton without counters'      ,
                                             'reified automaton constraint'    ,
                                             'arc-consistency'                 ,
                                             'order constraint'                ,
                                             'symmetry'                        ,
                                             'matrix symmetry'                 ]).

ctr_persons(increasing_global_cardinality,['Beldiceanu N.']).

ctr_eval(increasing_global_cardinality, [reformulation(increasing_global_cardinality_r),
                                             automaton(increasing_global_cardinality_a)]).

increasing_global_cardinality_r(VARIABLES, VALUES) :-
    eval(increasing(VARIABLES)),
    eval(global_cardinality_low_up(VARIABLES, VALUES)).

increasing_global_cardinality_a(FLAG, VARIABLES, VALUES) :-
    increasing_global_cardinality_get_a(VARIABLES, VALUES, AUTOMATON, ALPHABET),
    automaton_bool(FLAG, ALPHABET, AUTOMATON).

increasing_global_cardinality_get_a(VARIABLES, VALUES, AUTOMATON, ALPHABET) :-
    length(VARIABLES, N),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int,int(0,N),int(0,N)]),
    length(VALUES, M),
    M > 0,
    sort_collection(VALUES, val, SVALUES),
    get_attr1(VARIABLES, VARS ),
    get_attr1(SVALUES  , VALS ),
    get_attr2(SVALUES  , OMINS),
    get_attr3(SVALUES  , OMAXS),
    all_different(VALS),
    check_lesseq(OMINS, OMAXS),
    increasing_gcc_normalize(VALS, OMAXS, VARS),
    get_minimum(VARS, MINVARS),
    get_maximum(VARS, MAXVARS),
    get_minimum(VALS, MINVALS),
    get_maximum(VALS, MAXVALS),
    MIN is min(MINVARS,MINVALS),
    MAX is max(MAXVARS,MAXVALS),
    get_sum(OMINS, SUM_OMINS),
    REST is N-SUM_OMINS,
    REST >= 0,
    increasing_global_cardinality_complete_values(MIN, MAX, SVALUES, REST, CVALUES, SUM_OMAXS),
    reverse(CVALUES, RVALUES),
    increasing_global_cardinality_term_states(RVALUES, SUM_OMAXS, TERMINALS),
    append([source(0)],TERMINALS,STATES),
    increasing_global_cardinality_source_trans(CVALUES, 1, TRANSITIONS_FROM_SOURCE),
    increasing_global_cardinality_horiz_trans(CVALUES, 1, TRANSITIONS_HORIZONTAL),
    increasing_global_cardinality_vert_trans(CVALUES, 1, TRANSITIONS_VERTICAL),
    append(TRANSITIONS_FROM_SOURCE, TRANSITIONS_HORIZONTAL, T1),
    append(T1, TRANSITIONS_VERTICAL, ALL_TRANSITIONS),
    AUTOMATON = automaton(VARS, _, VARS, STATES, ALL_TRANSITIONS, [], [], []),
    append(VARS, VALS, ALL),
    union_dom_list_int(ALL, ALPHABET).

increasing_gcc_normalize([], [], _) :- !.
increasing_gcc_normalize([VAL|R], [OMAX|S], VARS) :-
    (OMAX = 0 -> remove_value_from_vars(VARS, VAL) ; true),
    increasing_gcc_normalize(R, S, VARS).

increasing_global_cardinality_complete_values(MIN, MAX, VALUES, _REST, VALUES, 0) :-
    MIN > MAX,
    (VALUES = [] -> true ; write(problem), nl, abort),
    !.
increasing_global_cardinality_complete_values(MIN, MAX, [], REST, [[constrained-CTR, val-MIN,omin-0,omax-OOMAX]|S], SUM) :-
    MIN =< MAX,
    !,
    (REST > 1 -> CTR = 0, OOMAX = 1 ; CTR = 1, OOMAX is max(1,REST)),
    MIN1 is MIN+1,
    increasing_global_cardinality_complete_values(MIN1, MAX, [], REST, S, TSUM),
    SUM is TSUM+OOMAX.
increasing_global_cardinality_complete_values(MIN, MAX, [[val-VAL,omin-OMIN,omax-OMAX]|R], REST, [[constrained-CTR, val-VAL,omin-OMIN,omax-OOMAX]|S], SUM) :-
    MIN =< MAX,
    MIN = VAL,
    !,
    (OMAX > 1, OMAX >= REST+OMIN -> CTR = 0, OOMAX is max(1,OMIN) ; CTR = 1, OOMAX is max(1,OMAX)),
    MIN1 is MIN+1,
    increasing_global_cardinality_complete_values(MIN1, MAX, R, REST, S, TSUM),
    SUM is TSUM+OOMAX.
increasing_global_cardinality_complete_values(MIN, MAX, [[val-VAL,omin-OMIN,omax-OMAX]|R], REST, [[constrained-CTR, val-MIN,omin-0,omax-OOMAX]|S], SUM) :-
    MIN =< MAX,
    MIN < VAL,
    (REST > 1 -> CTR = 0, OOMAX = 1 ; CTR = 1, OOMAX is max(1,REST)),
    MIN1 is MIN+1,
    increasing_global_cardinality_complete_values(MIN1, MAX, [[val-VAL,omin-OMIN,omax-OMAX]|R], REST, S, TSUM),
    SUM is TSUM+OOMAX.

increasing_global_cardinality_term_states([], _, []).
increasing_global_cardinality_term_states([[constrained-_, val-_VAL,omin-OMIN,omax-OMAX]|R], LAST_STATE_ID, RES) :-
    I is LAST_STATE_ID-OMAX+max(1,OMIN),
    increasing_global_cardinality_term_states1(I, LAST_STATE_ID, TERMS),    
    LAST_STATE_ID1 is LAST_STATE_ID-OMAX,
    (OMIN=0 -> increasing_global_cardinality_term_states(R, LAST_STATE_ID1, S), append(S, TERMS, RES) ; RES=TERMS).

increasing_global_cardinality_term_states1(I, MAX, []) :-
    I > MAX,
    !.
increasing_global_cardinality_term_states1(I, MAX, [sink(I)|R]) :-
    I =< MAX,
    I1 is I+1,
    increasing_global_cardinality_term_states1(I1, MAX, R).

increasing_global_cardinality_source_trans([], _, []).
increasing_global_cardinality_source_trans([[constrained-_, val-VAL,omin-OMIN,omax-OMAX]|R], CUR_ID, [arc(0,VAL,CUR_ID)|S]) :-
    CUR_ID1 is CUR_ID+OMAX,
    (OMIN=0 -> increasing_global_cardinality_source_trans(R, CUR_ID1, S) ; S = []).

increasing_global_cardinality_horiz_trans([], _, []).
increasing_global_cardinality_horiz_trans([[constrained-CTR, val-VAL,omin-_,omax-OMAX]|R], CUR_ID, RESULT) :-
    increasing_global_cardinality_horiz_trans1(1, OMAX, CTR, VAL, CUR_ID, TR),
    CUR_ID1 is CUR_ID + OMAX,
    increasing_global_cardinality_horiz_trans(R, CUR_ID1, S),
    append(TR, S, RESULT).

increasing_global_cardinality_horiz_trans1(I, OMAX, 1, _, _, []) :-
    I >= OMAX,
    !.
increasing_global_cardinality_horiz_trans1(I, OMAX, 0, VAL, ID, [arc(ID,VAL,ID)]) :-
    I >= OMAX,
    !.
increasing_global_cardinality_horiz_trans1(I, OMAX, CTR, VAL, ID, [arc(ID,VAL,ID1)|R]) :-
    I < OMAX,
    ID1 is ID+1,
    I1 is I+1,
    increasing_global_cardinality_horiz_trans1(I1, OMAX, CTR, VAL, ID1, R).    

increasing_global_cardinality_vert_trans([_], _, []) :-
    !.
increasing_global_cardinality_vert_trans([[constrained-_, val-_VAL,omin-OMIN,omax-OMAX]|R], CUR_ID, RESULT) :-
    I is CUR_ID+max(0,OMIN-1),
    CUR_ID1 is CUR_ID+OMAX,
    increasing_global_cardinality_vert_trans1(R, CUR_ID1, I, CUR_ID1, S),
    increasing_global_cardinality_vert_trans(R, CUR_ID1, T),
    append(S, T, RESULT).

increasing_global_cardinality_vert_trans1([], _, _, _, []).
increasing_global_cardinality_vert_trans1([[constrained-_, val-VAL,omin-OMIN,omax-OMAX]|R], CUR_ID, I, MAX, RESULT) :-
    increasing_global_cardinality_vert_trans2(I, MAX, CUR_ID, VAL, RES1),
    CUR_ID1 is CUR_ID+OMAX,
    (OMIN=0 -> increasing_global_cardinality_vert_trans1(R, CUR_ID1, I, MAX, RES2), append(RES1, RES2, RESULT) ; RESULT=RES1).

increasing_global_cardinality_vert_trans2(MAX, MAX, _, _, []) :-
    !.
increasing_global_cardinality_vert_trans2(I, MAX, CUR_ID, VAL, [arc(I,VAL,CUR_ID)|R]) :-
    I < MAX,
    I1 is I+1,
    increasing_global_cardinality_vert_trans2(I1, MAX, CUR_ID, VAL, R).
