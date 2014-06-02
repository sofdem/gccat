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

:-dynamic change_continuity_a/10.

ctr_date(change_continuity,['20000128','20030820','20040530','20060805']).

ctr_origin(change_continuity, 'N.~Beldiceanu', []).

ctr_arguments(change_continuity,
              ['NB_PERIOD_CHANGE'-dvar         ,
               'NB_PERIOD_CONTINUITY'-dvar     ,
               'MIN_SIZE_CHANGE'-dvar          ,
               'MAX_SIZE_CHANGE'-dvar          ,
               'MIN_SIZE_CONTINUITY'-dvar      ,
               'MAX_SIZE_CONTINUITY'-dvar      ,
               'NB_CHANGE'-dvar                ,
               'NB_CONTINUITY'-dvar            ,
               'VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ]).

ctr_exchangeable(change_continuity,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(change_continuity,
                 ['NB_PERIOD_CHANGE'     >= 0                    ,
                  'NB_PERIOD_CONTINUITY' >= 0                    ,
                  'MIN_SIZE_CHANGE'      >= 0                    ,
                  'MAX_SIZE_CHANGE'      >= 'MIN_SIZE_CHANGE'    ,
                  'MIN_SIZE_CONTINUITY'  >= 0                    ,
                  'MAX_SIZE_CONTINUITY'  >= 'MIN_SIZE_CONTINUITY',
                  'NB_CHANGE'            >= 0                    ,
                  'NB_CONTINUITY'        >= 0                    ,
                  required('VARIABLES',var)                      ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])               ]).

ctr_typical(change_continuity,
            ['NB_PERIOD_CHANGE'     > 0,
             'NB_PERIOD_CONTINUITY' > 0,
             'MIN_SIZE_CHANGE'      > 0,
             'MIN_SIZE_CONTINUITY'  > 0,
             'NB_CHANGE'            > 0,
             'NB_CONTINUITY'        > 0,
             size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1,
             in_list('CTR',[=\=])      ]).

ctr_functional_dependency(change_continuity, 1, [9,10]).
ctr_functional_dependency(change_continuity, 2, [9,10]).
ctr_functional_dependency(change_continuity, 3, [9,10]).
ctr_functional_dependency(change_continuity, 4, [9,10]).
ctr_functional_dependency(change_continuity, 5, [9,10]).
ctr_functional_dependency(change_continuity, 6, [9,10]).
ctr_functional_dependency(change_continuity, 7, [9,10]).
ctr_functional_dependency(change_continuity, 8, [9,10]).

ctr_graph(change_continuity,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          ['CTR'(variables1^var,variables2^var)],
          ['NCC'     = 'NB_PERIOD_CHANGE',
           'MIN_NCC' = 'MIN_SIZE_CHANGE' ,
           'MAX_NCC' = 'MAX_SIZE_CHANGE' ,
           'NARC'    = 'NB_CHANGE'       ],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_graph(change_continuity,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [#\'CTR'(variables1^var,variables2^var)],
          ['NCC'     = 'NB_PERIOD_CONTINUITY',
           'MIN_NCC' = 'MIN_SIZE_CONTINUITY' ,
           'MAX_NCC' = 'MAX_SIZE_CONTINUITY' ,
           'NARC'    = 'NB_CONTINUITY'       ],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(change_continuity,
            change_continuity(3,2,2,4,2,4,6,4,
                              [[var-1],[var-3],[var-1],[var-8],[var-8],[var-4],
                               [var-7],[var-7],[var-7],[var-7],[var-2]],
                              =\=)).

ctr_draw_example(change_continuity,
                 ['VARIABLES'],
                 [[[var-1],[var-3],[var-1],[var-8],[var-8],[var-4],
                   [var-7],[var-7],[var-7],[var-7],[var-2]]],
                 ['PATH'],
                 [1-2,2-3,3-4,5-6,10-11],
                 ['MIN_NCC'([10,11]),'MAX_NCC'([1,2,3,4]),'NARC'],
                 '','NCC=3\\nMIN_NCC=2\\nMAX_NCC=4\\nNARC=5',
                 [2.145,5,2.145,2.4]).

ctr_see_also(change_continuity,
 [link('common keyword', group,                    '%k', ['timetabling constraint']),
  link('common keyword', group_skip_isolated_item, '%k', ['timetabling constraint']),
  link('common keyword', stretch_path,             '%k', ['timetabling constraint'])]).

ctr_key_words(change_continuity,['timetabling constraint'                 ,
                                 'sequence'                               ,
                                 'run of a permutation'                   ,
                                 'permutation'                            ,
                                 'connected component'                    ,
                                 'automaton'                              ,
                                 'automaton with counters'                ,
			         'automaton with same input symbol'       ,
                                 'reverse of a constraint'                ,
                                 'glue matrix'                            ,
                                 'sliding cyclic(1) constraint network(2)',
                                 'sliding cyclic(1) constraint network(3)',
                                 'apartition'                             ,
                                 'acyclic'                                ,
                                 'bipartite'                              ,
                                 'no loop'                                ,
                                 'functional dependency'                  ]).

ctr_eval(change_continuity, [checker(change_continuity_c) ,
			     automata(change_continuity_a)]).

change_continuity_a(NB_PERIOD_CHANGE    ,
                    NB_PERIOD_CONTINUITY,
                    MIN_SIZE_CHANGE     ,
                    MAX_SIZE_CHANGE     ,
                    MIN_SIZE_CONTINUITY ,
                    MAX_SIZE_CONTINUITY ,
                    NB_CHANGE           ,
                    NB_CONTINUITY       ,
                    VARIABLES           ,
                    CTR                 ) :-
    check_type(dvar, NB_PERIOD_CHANGE),
    check_type(dvar, NB_PERIOD_CONTINUITY),
    check_type(dvar, MIN_SIZE_CHANGE),
    check_type(dvar, MAX_SIZE_CHANGE),
    check_type(dvar, MIN_SIZE_CONTINUITY),
    check_type(dvar, MAX_SIZE_CONTINUITY),
    check_type(dvar, NB_CHANGE),
    check_type(dvar, NB_CONTINUITY),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    length(VARIABLES, N),
    (N = 0 ->
	NB_PERIOD_CHANGE     #= 0,
        NB_PERIOD_CONTINUITY #= 0,
        MIN_SIZE_CHANGE      #= 0,
        MAX_SIZE_CHANGE      #= 0,
        MIN_SIZE_CONTINUITY  #= 0,
        MAX_SIZE_CONTINUITY  #= 0,
        NB_CHANGE            #= 0,
        NB_CONTINUITY        #= 0
    ;
        collection(VARIABLES, [dvar]),
        NB_PERIOD_CHANGE     #>= 0                  ,
        NB_PERIOD_CONTINUITY #>= 0                  ,
        MIN_SIZE_CHANGE      #>= 0                  ,
        MAX_SIZE_CHANGE      #>= MIN_SIZE_CHANGE    ,
        MIN_SIZE_CONTINUITY  #>= 0                  ,
        MAX_SIZE_CONTINUITY  #>= MIN_SIZE_CONTINUITY,
        NB_CHANGE            #>= 0                  ,
        NB_CONTINUITY        #>= 0                  ,
        change_continuity_signature(VARIABLES, SIGNATURE_CTR    , 1, CTR),
        change_continuity_signature(VARIABLES, SIGNATURE_NOT_CTR, 0, CTR),
        change_continuity_nb_period(NB_PERIOD_CHANGE    , SIGNATURE_CTR    ),
        change_continuity_nb_period(NB_PERIOD_CONTINUITY, SIGNATURE_NOT_CTR),
        change_continuity_min_size(MIN_SIZE_CHANGE      , SIGNATURE_CTR    ),
        change_continuity_min_size(MIN_SIZE_CONTINUITY  , SIGNATURE_NOT_CTR),
        change_continuity_max_size(MAX_SIZE_CHANGE      , SIGNATURE_CTR    ),
        change_continuity_max_size(MAX_SIZE_CONTINUITY  , SIGNATURE_NOT_CTR),
        change_continuity_nb(NB_CHANGE                  , SIGNATURE_CTR    ),
        change_continuity_nb(NB_CONTINUITY              , SIGNATURE_NOT_CTR)
    ).

change_continuity_nb_period(NB_PERIOD, SIGNATURE) :-
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s),sink(i)],
              [arc(s,0,s      ),
               arc(s,1,i,[C+1]),
               arc(i,1,i      ),
               arc(i,0,s      )],
              [C],[0],[NB_PERIOD]).

change_continuity_min_size(MIN_SIZE, SIGNATURE) :-
    MIN_SIZE #= min(C1,D1),
    length(SIGNATURE, N),
    N1 is N + 1,
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s),sink(i)],
              [arc(s,0,s               ),
               arc(s,1,i,[C       ,2  ]),
               arc(i,0,s,[min(C,D),D  ]),
               arc(i,1,i,[C       ,D+1])],
               [C,D],[N1,0],[C1,D1]).

change_continuity_max_size(MAX_SIZE, SIGNATURE) :-
    MAX_SIZE #= max(C1,D1),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(s)],
              [arc(s,0,s,[C       ,D  ]),
               arc(s,1,i,[C       ,2  ]),
               arc(i,0,i,[max(C,D),1  ]),
               arc(i,1,i,[C       ,D+1])],
              [C,D],[0,0],[C1,D1]).

change_continuity_nb(NB, SIGNATURE) :-
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[NB]).

change_continuity_signature([], [], _, _).
change_continuity_signature([_], [], _, _) :- !.
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 1, =) :- !,
    VAR1 #= VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 1, =).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 1, =\=) :- !,
    VAR1 #\= VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 1, =\=).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 1, <) :- !,
    VAR1 #< VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 1, <).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 1, >=) :- !,
    VAR1 #>= VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 1, >=).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 1, >) :- !,
    VAR1 #> VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 1, >).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 1, =<) :- !,
    VAR1 #=< VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 1, =<).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 0, =) :- !,
    VAR1 #\= VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 0, =).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 0, =\=) :- !,
    VAR1 #= VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 0, =\=).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 0, <) :- !,
    VAR1 #>= VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 0, <).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 0, >=) :- !,
    VAR1 #< VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 0, >=).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 0, >) :- !,
    VAR1 #=< VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 0, >).
change_continuity_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], 0, =<) :- !,
    VAR1 #> VAR2 #<=> S,
    change_continuity_signature([[var-VAR2]|VARs], Ss, 0, =<).

change_continuity_c(NB_PERIOD_CHANGE    ,
                    NB_PERIOD_CONTINUITY,
                    MIN_SIZE_CHANGE     ,
                    MAX_SIZE_CHANGE     ,
                    MIN_SIZE_CONTINUITY ,
                    MAX_SIZE_CONTINUITY ,
                    NB_CHANGE           ,
                    NB_CONTINUITY       ,
                    VARIABLES           ,
                    CTR                 ) :-
    check_type(dvar, NB_PERIOD_CHANGE),
    check_type(dvar, NB_PERIOD_CONTINUITY),
    check_type(dvar, MIN_SIZE_CHANGE),
    check_type(dvar, MAX_SIZE_CHANGE),
    check_type(dvar, MIN_SIZE_CONTINUITY),
    check_type(dvar, MAX_SIZE_CONTINUITY),
    check_type(dvar, NB_CHANGE),
    check_type(dvar, NB_CONTINUITY),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    length(VARIABLES, N),
    (N = 0 ->
	NB_PERIOD_CHANGE     #= 0,
        NB_PERIOD_CONTINUITY #= 0,
        MIN_SIZE_CHANGE      #= 0,
        MAX_SIZE_CHANGE      #= 0,
        MIN_SIZE_CONTINUITY  #= 0,
        MAX_SIZE_CONTINUITY  #= 0,
        NB_CHANGE            #= 0,
        NB_CONTINUITY        #= 0
    ;
	collection(VARIABLES, [int]),
	NB_PERIOD_CHANGE     #>= 0                  ,
	NB_PERIOD_CONTINUITY #>= 0                  ,
	MIN_SIZE_CHANGE      #>= 0                  ,
	MAX_SIZE_CHANGE      #>= MIN_SIZE_CHANGE    ,
	MIN_SIZE_CONTINUITY  #>= 0                  ,
	MAX_SIZE_CONTINUITY  #>= MIN_SIZE_CONTINUITY,
	NB_CHANGE            #>= 0                  ,
	NB_CONTINUITY        #>= 0                  ,
	change_continuity_signature_c(CTR, VARIABLES, SIGNATURE_CTR, SIGNATURE_NOT_CTR),
	(SIGNATURE_CTR = [] ->
	    NB_PERIOD_CONTINUITY is 0, NB_PERIOD_CHANGE is 0
	;
	    change_continuity_nb_period_c(s, SIGNATURE_CTR, 0, NB_PERIOD_CHANGE),
	    SIGNATURE_CTR = [First|_],
	    last(SIGNATURE_CTR, Last),
	    ((First = 0, Last = 0) -> NB_PERIOD_CONTINUITY is NB_PERIOD_CHANGE + 1
	    ;
		(First = 1, Last = 1) -> NB_PERIOD_CONTINUITY is NB_PERIOD_CHANGE - 1
	    ;
		NB_PERIOD_CONTINUITY is NB_PERIOD_CHANGE
	    )
	),
	change_continuity_min_size_c(s, SIGNATURE_CTR,     N, 0, MIN_SIZE_CHANGE),
	change_continuity_min_size_c(s, SIGNATURE_NOT_CTR, N, 0, MIN_SIZE_CONTINUITY),
	change_continuity_max_size_c(s, SIGNATURE_CTR,     0, 0, MAX_SIZE_CHANGE),
	change_continuity_max_size_c(s, SIGNATURE_NOT_CTR, 0, 0, MAX_SIZE_CONTINUITY),
	change_continuity_nb_c(SIGNATURE_CTR, 0, NB_CHANGE),
	NB_CONTINUITY is N - NB_CHANGE - 1
    ).

change_continuity_nb_period_c(s, [0|R], C, NB_PERIOD_CHANGE) :-
    !,
    change_continuity_nb_period_c(s, R, C, NB_PERIOD_CHANGE).
change_continuity_nb_period_c(s, [1|R], C, NB_PERIOD_CHANGE) :-
    !,
    C1 is C+1,
    change_continuity_nb_period_c(i, R, C1, NB_PERIOD_CHANGE).
change_continuity_nb_period_c(i, [1|R], C, NB_PERIOD_CHANGE) :-
    !,
    change_continuity_nb_period_c(i, R, C, NB_PERIOD_CHANGE).
change_continuity_nb_period_c(i, [0|R], C, NB_PERIOD_CHANGE) :-
    !,
    change_continuity_nb_period_c(s, R, C, NB_PERIOD_CHANGE).
change_continuity_nb_period_c(_, [], C, C).

change_continuity_min_size_c(s, [0|R], C, D, MIN_SIZE_CHANGE) :-
    !,
    change_continuity_min_size_c(s, R, C, D, MIN_SIZE_CHANGE).
change_continuity_min_size_c(s, [1|R], C, _, MIN_SIZE_CHANGE) :-
    !,
    change_continuity_min_size_c(i, R, C, 2, MIN_SIZE_CHANGE).
change_continuity_min_size_c(i, [0|R], C, D, MIN_SIZE_CHANGE) :-
    !,
    C1 is min(C,D),
    (C1 > 1 ->
	change_continuity_min_size_c(s, R, C1, D, MIN_SIZE_CHANGE)
    ;
	MIN_SIZE_CHANGE #= C1
    ).
change_continuity_min_size_c(i, [1|R], C, D, MIN_SIZE_CHANGE) :-
    !,
    D1 is D+1,
    change_continuity_min_size_c(i, R, C, D1, MIN_SIZE_CHANGE).
change_continuity_min_size_c(_, [], C, D, MIN_SIZE_CHANGE) :-
    MIN is min(C,D),
    MIN_SIZE_CHANGE #= MIN.

change_continuity_max_size_c(s, [0|R], C, D, MAX_SIZE_CHANGE) :-
    !,
    change_continuity_max_size_c(s, R, C, D, MAX_SIZE_CHANGE).
change_continuity_max_size_c(s, [1|R], C, _D, MAX_SIZE_CHANGE) :-
    !,
    D1 is 2,
    change_continuity_max_size_c(i, R, C, D1, MAX_SIZE_CHANGE).
change_continuity_max_size_c(i, [0|R], C, D, MAX_SIZE_CHANGE) :-
    !,
    C1 is max(C,D),
    change_continuity_max_size_c(i, R, C1, 1, MAX_SIZE_CHANGE).
change_continuity_max_size_c(i, [1|R], C, D, MAX_SIZE_CHANGE) :-
    !,
    D1 is D+1,
    change_continuity_max_size_c(i, R, C, D1, MAX_SIZE_CHANGE).
change_continuity_max_size_c(_, [], C, D, MAX_SIZE_CHANGE) :-
    MAX is max(C,D),
    MAX_SIZE_CHANGE #= MAX.

change_continuity_nb_c([B|R], C, NB_CHANGE) :-
    !,
    C1 is C+B,
    change_continuity_nb_c(R, C1, NB_CHANGE).
change_continuity_nb_c([], C, C).

change_continuity_signature_c(=, [[var-VAR1],[var-VAR2]|VARs], [S|Ss], [R|Rs]) :- !,
    (VAR1 = VAR2 -> S = 1, R = 0 ; S = 0, R = 1),
    change_continuity_signature_c(=, [[var-VAR2]|VARs], Ss, Rs).
change_continuity_signature_c(=\=, [[var-VAR1],[var-VAR2]|VARs], [S|Ss], [R|Rs]) :- !,
    (VAR1 =\= VAR2 -> S = 1, R = 0 ; S = 0, R = 1),
    change_continuity_signature_c(=\=, [[var-VAR2]|VARs], Ss, Rs).
change_continuity_signature_c(<, [[var-VAR1],[var-VAR2]|VARs], [S|Ss], [R|Rs]) :- !,
    (VAR1 < VAR2 -> S = 1, R = 0 ; S = 0, R = 1),
    change_continuity_signature_c(<, [[var-VAR2]|VARs], Ss, Rs).
change_continuity_signature_c(>=, [[var-VAR1],[var-VAR2]|VARs], [S|Ss], [R|Rs]) :- !,
    (VAR1 >= VAR2 -> S = 1, R = 0 ; S = 0, R = 1),
    change_continuity_signature_c(>=, [[var-VAR2]|VARs], Ss, Rs).
change_continuity_signature_c(>, [[var-VAR1],[var-VAR2]|VARs], [S|Ss], [R|Rs]) :- !,
    (VAR1 > VAR2 -> S = 1, R = 0 ; S = 0, R = 1),
    change_continuity_signature_c(>, [[var-VAR2]|VARs], Ss, Rs).
change_continuity_signature_c(=<, [[var-VAR1],[var-VAR2]|VARs], [S|Ss], [R|Rs]) :- !,
    (VAR1 =< VAR2 -> S = 1, R = 0 ; S = 0, R = 1),
    change_continuity_signature_c(=<, [[var-VAR2]|VARs], Ss, Rs).
change_continuity_signature_c(_, [_], [], []) :- !.
change_continuity_signature_c(_, [], [], []).
