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

:-dynamic change_a/4.

ctr_date(change,['20000128','20030820','20040530','20060805']).

ctr_origin(change, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(change,
              ['NCHANGE'-dvar                  ,
               'VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ]).

ctr_exchangeable(change,
                 [translate(['VARIABLES'^var])]).

ctr_synonyms(change,[nbchanges,similarity]).

ctr_restrictions(change,
                 ['NCHANGE' >=  0                 ,
                  'NCHANGE' <  size('VARIABLES')  ,
                  required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(change,
            ['NCHANGE'              > 0,
             size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1,
             in_list('CTR',[=\=])      ]).

ctr_pure_functional_dependency(change, []).
ctr_functional_dependency(change, 1, [2,3]).

ctr_contractible(change, [in_list('CTR',[=\=,<,>=,>,=<]),'NCHANGE'=0], 'VARIABLES', any).
ctr_contractible(change, [in_list('CTR',[=,<,>=,>,=<]),'NCHANGE'=size('VARIABLES')-1], 'VARIABLES', any).

ctr_graph(change,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          ['CTR'(variables1^var,variables2^var)],
          ['NARC' = 'NCHANGE'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(change,
            [change(3,[[var-4],[var-4],[var-3],[var-4],[var-1]],=\=),
             change(1,[[var-1],[var-2],[var-4],[var-3],[var-7]],>  )]).

ctr_draw_example(change,
                 ['VARIABLES'],
                 [[[var-4],[var-4],[var-3],[var-4],[var-1]]],
                 ['PATH'],
                 [2-3,3-4,4-5],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,2]).

ctr_see_also(change,
 [link(generalisation,     change_pair,         '%e replaced by %e of %e',                     [variable, pair, variables]),
  link(generalisation,     change_vectors ,     '%e replaced by %k',                           [variable, vector]),
  link('common keyword',   cyclic_change_joker, '%k',                                          ['number of changes']),
  link('common keyword',   smooth,              '%k in a sequence of %e with respect to a %e', ['number of changes',variables,'binary~constraint']),
  link('common keyword',   change_partition,    '%k in a sequence of %e with respect to a %e', ['number of changes',variables,'binary~constraint']),
  link('common keyword',   circular_change,     '%k in a sequence of %e with respect to a %e', ['number of changes',variables,'binary~constraint']),
  link('common keyword',   cyclic_change,       '%k',                                          ['number of changes']),
  link('shift of concept', longest_change,      '',                                            []),
  link('shift of concept', distance_change,     '',                                            [])]).

ctr_key_words(change,['timetabling constraint'                 ,
                      'number of changes'                      ,
                      'automaton'                              ,
                      'automaton with counters'                ,
                      'sliding cyclic(1) constraint network(2)',
                      'Berge-acyclic constraint network'       ,
                      'non-deterministic automaton'            ,
                      'acyclic'                                ,
                      'bipartite'                              ,
                      'no loop'                                ,
                      'dynamic programming'                    ,
                      'functional dependency'                  ,
		      'pure functional dependency'             ]).

ctr_persons(change,['Pachet F.'    ,
                    'Roy P.'       ,
                    'Beldiceanu N.',
                    'Hellsten L.'  ,
                    'Pesant G.'    ]).

ctr_eval(change, [  checker(change_c),
		  automaton(change_a)]).

change_c(NCHANGE, VARIABLES, =) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    (N =< 1 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
        change_eq_c(VARS, 0, NCHANGE)
    ).
change_c(NCHANGE, VARIABLES, =\=) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    (N =< 1 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
        change_neq_c(VARS, 0, NCHANGE)
    ).
change_c(NCHANGE, VARIABLES, <) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    (N =< 1 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
        change_lt_c(VARS, 0, NCHANGE)
    ).
change_c(NCHANGE, VARIABLES, >=) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    (N =< 1 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
        change_geq_c(VARS, 0, NCHANGE)
    ).
change_c(NCHANGE, VARIABLES, >) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    (N =< 1 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
        change_gt_c(VARS, 0, NCHANGE)
    ).
change_c(NCHANGE, VARIABLES, =<) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    (N =< 1 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
        change_leq_c(VARS, 0, NCHANGE)
    ).

change_neq_counters_check([V,V|R], C, [C|S]) :-
    !,
    change_neq_counters_check([V|R], C, S).
change_neq_counters_check([_,V|R], C, [C1|S]) :-
    !,
    C1 is C+1,
    change_neq_counters_check([V|R], C1, S).
change_neq_counters_check(_, _, [0]).

% CTR: =
% 0: VAR1=\=VAR2
% 1: VAR1=VAR2
%
% CTR: =\=
% 0: VAR1=VAR2
% 1: VAR1=\=VAR2
%
% CTR: <
% 0: VAR1>=VAR2
% 1: VAR1<VAR2
%
% CTR: >=
% 0: VAR1<VAR2
% 1: VAR1>=VAR2
%
% CTR: >
% 0: VAR1=<VAR2
% 1: VAR1>VAR2
%
% CTR: =<
% 0: VAR1>VAR2
% 1: VAR1=<VAR2
change_a(FLAG, NCHANGE, VARIABLES, CTR) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    fd_min(NCHANGE, MIN_NCHANGE),
    fd_max(NCHANGE, MAX_NCHANGE),
    (FLAG = 1, MIN_NCHANGE = 0, MAX_NCHANGE = 0, memberchk(CTR, [=\=]) ->
	eval(all_equal(VARIABLES))
    ;
	change_signature(VARIABLES, SIGNATURE, CTR),
	automaton(SIGNATURE, _,
		  SIGNATURE, 
		  [source(s),sink(s)],
		  [arc(s,0,s      ),
		   arc(s,1,s,[C+1])],
		  [C],[0],[COUNT]),
	NCHANGE #= COUNT #<=> FLAG
    ).

change_signature([], [], _).
change_signature([_], [], _) :- !.
change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =) :- !,
    VAR1 #= VAR2 #<=> S,
    change_signature([[var-VAR2]|VARs], Ss, =).
change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =\=) :- !,
    VAR1 #\= VAR2 #<=> S,
    change_signature([[var-VAR2]|VARs], Ss, =\=).
change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], <) :- !,
    VAR1 #< VAR2 #<=> S,
    change_signature([[var-VAR2]|VARs], Ss, <).
change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], >=) :- !,
    VAR1 #>= VAR2 #<=> S,
    change_signature([[var-VAR2]|VARs], Ss, >=).
change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], >) :- !,
    VAR1 #> VAR2 #<=> S,
    change_signature([[var-VAR2]|VARs], Ss, >).
change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =<) :- !,
    VAR1 #=< VAR2 #<=> S,
    change_signature([[var-VAR2]|VARs], Ss, =<).
