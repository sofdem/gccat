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
    ctr_total_relation/1,
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

ctr_date(min_size_full_zero_stretch,['20121023']).

ctr_origin(min_size_full_zero_stretch, 'Derived from the unit commitment problem', []).

ctr_arguments(min_size_full_zero_stretch,
              ['MINSIZE'-int                   ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(min_size_full_zero_stretch,
                 [items('VARIABLES',reverse)                               ,
		  vals(['VARIABLES'^var],int(=\=(0)),=\=,dontcare,dontcare)]).

ctr_restrictions(min_size_full_zero_stretch,
                 ['MINSIZE' >= 0                ,
                  'MINSIZE' =< size('VARIABLES'),
                  required('VARIABLES',var)     ]).

ctr_typical(min_size_full_zero_stretch,
            [size('VARIABLES')                                 > 2,
             range('VARIABLES'^var)                            > 1,
	     size('VARIABLES') - among_diff_0('VARIABLES'^var) > 1]).

ctr_total_relation(min_size_full_zero_stretch).

ctr_example(min_size_full_zero_stretch,
            min_size_full_zero_stretch(2,[[var-0],[var-2],[var-0],[var-0],[var-0],[var-2],[var-1],[var-0],[var-0],[var-3]])).

ctr_see_also(min_size_full_zero_stretch,
 [link('common keyword', stretch_path, '%k', ['sequence'])]).

ctr_key_words(min_size_full_zero_stretch,['sequence'                           ,
			 		  'joker value'                        ,
                                          'automaton'                          ,
                                          'automaton with counters'            ,
                                          'automaton with same input symbol'   ,
                                          'alpha-acyclic constraint network(3)']).

ctr_eval(min_size_full_zero_stretch, [checker(min_size_full_zero_stretch_c),
				      automaton(min_size_full_zero_stretch_a)]).

ctr_sol(min_size_full_zero_stretch,2,0,2,9,[2-9]).
ctr_sol(min_size_full_zero_stretch,3,0,3,82,[1-9,2-9,3-64]).
ctr_sol(min_size_full_zero_stretch,4,0,4,1137,[1-160,2-176,3-176,4-625]).
ctr_sol(min_size_full_zero_stretch,5,0,5,19026,[1-2575,2-2875,3-2900,4-2900,5-7776]).
ctr_sol(min_size_full_zero_stretch,6,0,6,364033,[1-45072,2-49932,3-50436,4-50472,5-50472,6-117649]).
ctr_sol(min_size_full_zero_stretch,7,0,7,7850291,[1-882441,2-966672,3-975394,4-976178,5-976227,6-976227,7-2097152]).
ctr_sol(min_size_full_zero_stretch,8,0,8,188987201,[1-19330432,2-20958912,3-21117888,4-21132416,5-21133568,6-21133632,7-21133632,8-43046721]).

% 0: VAR=0
% 1: VAR<>0
min_size_full_zero_stretch_a(FLAG, MINSIZE, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    check_type(dvar(0,L), MINSIZE),
    min_size_full_zero_stretch_signature(VARIABLES, SIGNATURE),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s),sink(i),sink(j)],
              [arc(s,0,s),
               arc(s,1,i),
               arc(i,1,i),
               arc(i,0,j,[M,C+1]),
               arc(j,0,j,[M,C+1]),
               arc(j,1,i,[min(M,C),0])],
              [M,C],[L,0],[MIN, _]),
    MINSIZE #>= MIN #<=> FLAG,
    (integer(MINSIZE) -> true ;
     FLAG = 0         -> true ;
	                 MINSIZE #= MIN). % fix MINSIZE to minimum size if not initially fixed (used in the context of learning)

min_size_full_zero_stretch_signature([], []).
min_size_full_zero_stretch_signature([[var-VAR]|VARs], [S|Ss]) :-
    VAR #\= 0 #<=> S,
    min_size_full_zero_stretch_signature(VARs, Ss).

min_size_full_zero_stretch_c(MINSIZE, VARIABLES) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, L),
    check_type(dvar(0,L), MINSIZE),
    get_attr1(VARIABLES, VARS),
    min_size_full_zero_stretch_c(VARS, s, L, 0, MINSIZE).

min_size_full_zero_stretch_c([0|R], s, M, C, MINSIZE) :-
    !,
    min_size_full_zero_stretch_c(R, s, M, C, MINSIZE).
min_size_full_zero_stretch_c([_|R], s, M, C, MINSIZE) :-
    !,
    min_size_full_zero_stretch_c(R, i, M, C, MINSIZE).
min_size_full_zero_stretch_c([0|R], i, M, C, MINSIZE) :-
    !,
    C1 is C+1,
    min_size_full_zero_stretch_c(R, j, M, C1, MINSIZE).
min_size_full_zero_stretch_c([_|R], i, M, C, MINSIZE) :-
    !,
    min_size_full_zero_stretch_c(R, i, M, C, MINSIZE).
min_size_full_zero_stretch_c([0|R], j, M, C, MINSIZE) :-
    !,
    C1 is C+1,
    min_size_full_zero_stretch_c(R, j, M, C1, MINSIZE).
min_size_full_zero_stretch_c([_|R], j, M, C, MINSIZE) :-
    !,
    M1 is min(M,C),
    min_size_full_zero_stretch_c(R, i, M1, 0, MINSIZE).
min_size_full_zero_stretch_c([], _, M, _, MINSIZE) :-
    (integer(MINSIZE) ->
	MINSIZE >= M
    ;
	MINSIZE #= M % fix MINSIZE to minimum size if not initially fixed 
    ).               % (used in the context of learning)
