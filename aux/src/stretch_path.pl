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

ctr_date(stretch_path,['20030820','20060817','20090712']).

ctr_origin(stretch_path, '\\cite{Pesant01}', []).

ctr_usual_name(stretch_path, stretch).

ctr_arguments(stretch_path,
              ['VARIABLES'-collection(var-dvar)                ,
               'VALUES'-collection(val-int, lmin-int, lmax-int)]).

ctr_exchangeable(stretch_path,
                 [items('VARIABLES',reverse),
                  items('VALUES',all),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(stretch_path,
                 [size('VARIABLES') > 0                  ,
                  required('VARIABLES',var)              ,
                  size('VALUES') > 0                     ,
                  required('VALUES',[val,lmin,lmax])     ,
                  distinct('VALUES',val)                 ,
                  'VALUES'^lmin      >= 0                ,
                  'VALUES'^lmin      =< 'VALUES'^lmax    ,
                  'VALUES'^lmin      =< size('VARIABLES')]).

ctr_typical(stretch_path,
            [size('VARIABLES')      > 1                ,
             range('VARIABLES'^var) > 1                ,
             size('VARIABLES')      > size('VALUES')   ,
             size('VALUES')         > 1                ,
             sum('VALUES'^lmin)    =< size('VARIABLES'),
             'VALUES'^lmax         =< size('VARIABLES')]).

ctr_graph(stretch_path,
          ['VARIABLES'],
          2,
          foreach('VALUES',['PATH'>>collection(variables1,variables2),
                            'LOOP'>>collection(variables1,variables2)]),
          [variables1^var = 'VALUES'^val,
           variables2^var = 'VALUES'^val],
          [not_in('MIN_NCC',1,'VALUES'^lmin-1),
           'MAX_NCC' =< 'VALUES'^lmax         ],
          []).

ctr_example(stretch_path,
            stretch_path([[var-6],[var-6],[var-3],[var-1],
                          [var-1],[var-1],[var-6],[var-6]],
                         [[val-1, lmin-2, lmax-4],
                          [val-2, lmin-2, lmax-3],
                          [val-3, lmin-1, lmax-6],
                          [val-6, lmin-2, lmax-2]])).

ctr_draw_example(stretch_path,
                 ['VARIABLES'],
                 [[[var-6],[var-6],[var-3],[var-1],
                   [var-1],[var-1],[var-6],[var-6]]],
                 ['PATH','LOOP'],
                 [1-[1,2],
                  2-2,
                  3-3,
                  4-[4,5],
                  5-[5,6],
                  6-6,
                  7-[7,8],
                  8-8],
                 ['FOREACH'('VALUES',[1-[4,5,6],3-[3],6-[1,2,7,8]])],
                 '','1:MIN_NCC=3,MAX_NCC=3\\n3:MIN_NCC=1,MAX_NCC=1\\n6:MIN_NCC=2,MAX_NCC=2',
                 [2.4,2.5,2.6,2.525]).

ctr_see_also(stretch_path,
 [link('generalisation',            stretch_path_partition,     '%e replaced by %e', [variable,in_list(variable,partition)]),
  link('common keyword',            change_continuity,          '%k',                ['timetabling constraint']),
  link('common keyword',            group_skip_isolated_item,   '%k,%k',             ['timetabling constraint', 'sequence']),
  link('common keyword',            stretch_circuit,            '%k,%k',             ['sliding sequence constraint', 'timetabling constraint']),
  link('common keyword',            pattern,                    '%k,%k',             ['sliding sequence constraint', 'timetabling constraint']),
  link('common keyword',            sliding_distribution,       '%k',                ['sliding sequence constraint']),
  link('common keyword',            group,                      '%k',                ['timetabling constraint']),
  link('common keyword',            min_size_full_zero_stretch, '%k',                ['sequence']),
  link('uses in its reformulation', stretch_circuit,            '',                  [])]).

ctr_key_words(stretch_path,['timetabling constraint'          ,
                            'sequence'                        ,
                            'sliding sequence constraint'     ,
                            'dynamic programming'             ,
                            'arc-consistency'                 ,
                            'consecutive loops are connected' ,
                            'Berge-acyclic constraint network',
                            'automaton'                       ,
                            'automaton without counters'      ,
                            'reified automaton constraint'    ]).

ctr_persons(stretch_path,['Pesant G.'  ,
                          'Hellsten L.',
                          'van Beek P.']).

ctr_eval(stretch_path, [automaton(stretch_path_a)]).

stretch_path_a(FLAG, VARIABLES, VALUES) :-
    stretch_path_get_a(VARIABLES, VALUES, AUTOMATON, ALPHABET),
    automaton_bool(FLAG, ALPHABET, AUTOMATON).

stretch_path_get_a(VARIABLES, VALUES, AUTOMATON, ALPHABET) :-
    length(VARIABLES, N),
    N > 0,
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int,int(0,N),int]),
    get_attr1(VARIABLES, VARS       ),
    get_attr1(VALUES   , VALS       ),
    get_attr2(VALUES   , LMINS      ),
    get_attr3(VALUES   , LMAXS      ),
    length(VALS, M),
    M > 0,
    all_different(VALS),
    check_lesseq(LMINS, LMAXS),
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
     MAXVARS    =< MAXVALS ->
        stretch_path_simplify_transitions(TRANSITIONS, MINVALS, SIMPLIFIED_TRANSITIONS),
        AUTOMATON = automaton(VARS, _, VARS, STATES, SIMPLIFIED_TRANSITIONS, [], [], []),
        SIG in MINVALS..MAXVALS
        ;
        stretch_path_signature(VARS, VALS, M, SIGNATURE),
        AUTOMATON = automaton(SIGNATURE, _, SIGNATURE, STATES, TRANSITIONS, [], [], []),
        SIG in 0..M
    ),
    union_dom_list_int([SIG], ALPHABET).

stretch_path_simplify_transitions([], _, []) :-
    !.
stretch_path_simplify_transitions([arc(_,0,_)|R], MINVALS, S) :-
    !,
    stretch_path_simplify_transitions(R, MINVALS, S).
stretch_path_simplify_transitions([arc(Si,E,Sj)|R],MINVALS, [arc(Si,NE,Sj)|S]) :-
    NE is MINVALS+E-1,
    stretch_path_simplify_transitions(R, MINVALS, S).

stretch_path_signature([], _, _, []).
stretch_path_signature([VAR|VARs], VALS, M, [S|Ss]) :-
    S in 0..M,
    stretch_path_signature1(VALS, VALS, VAR, 1, S),
    stretch_path_signature(VARs, VALS, M, Ss).

stretch_path_signature1([], VALS, VAR, _, S) :-
    stretch_path_signature2(VALS, VAR, DIFF),
    call(DIFF #<=> S #= 0).
stretch_path_signature1([VAL|VALs], VALS, VAR, I, S) :-
    VAR #= VAL #<=> S #= I,
    I1 is I+1,
    stretch_path_signature1(VALs, VALS, VAR, I1, S).

stretch_path_signature2([], _, 1).
stretch_path_signature2([VAL|VALs], VAR, VAR #\= VAL #/\ R) :-
    stretch_path_signature2(VALs, VAR, R).