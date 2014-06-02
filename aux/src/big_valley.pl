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
    ctr_automaton_signature/3,
    ctr_sol/6,
    ctr_logic/3,
    ctr_application/2.

:-dynamic big_valley_a/4.

ctr_date(big_valley,['20130127']).

ctr_origin(big_valley, 'Derived from %c.', [valley]).

ctr_arguments(big_valley,
              ['N'-dvar                        ,
               'VARIABLES'-collection(var-dvar),
	       'TOLERANCE'-int                 ]).

ctr_exchangeable(big_valley,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(big_valley,
                 ['N'   >= 0                         ,
                  2*'N' =< max(size('VARIABLES')-1,0),
                  required('VARIABLES',var)          ,
		  'TOLERANCE' >= 0                   ]).

ctr_pure_functional_dependency(big_valley, []).
ctr_functional_dependency(big_valley, 1, [2,3]).

ctr_typical(big_valley,
            ['N' >= 1                  ,
	     size('VARIABLES')      > 6,
             range('VARIABLES'^var) > 1,
	     'TOLERANCE'            > 1]).

ctr_contractible(big_valley, ['N'=0,'TOLERANCE'=0], 'VARIABLES', any).

ctr_example(big_valley,
            [big_valley(7,[[var-9],[var-11],[var-11],[var-9],[var-10],[var-5],[var-7],
			   [var-6],[var-6],[var-4],[var-8],[var-7],[var-10],[var-1],
			   [var-1],[var-7],[var-7],[var-5],[var-9],[var-8],[var-12]],0),
             big_valley(4,[[var-9],[var-11],[var-11],[var-9],[var-10],[var-5],[var-7],
			   [var-6],[var-6],[var-4],[var-8],[var-7],[var-10],[var-1],
			   [var-1],[var-7],[var-7],[var-5],[var-9],[var-8],[var-12]],1)]).

ctr_see_also(big_valley,
 [link('specialisation', valley, 'the tolerance is set to %e and removed', [0])]).

ctr_key_words(big_valley,['sequence'                  ,
                          'automaton'                 ,
                          'automaton with counters'   ,
		          'functional dependency'     ,
		          'pure functional dependency']).

ctr_eval(big_valley, [checker(big_valley_c),
		      automaton(big_valley_a),
		      automaton_with_signature(big_valley_a_s)]).

big_valley_c(N, VARIABLES, TOLERANCE) :-
	check_type(dvar_gteq(0), N),
	collection(VARIABLES, [int]),
	length(VARIABLES, L),
	MAX is max(L-1,0),
	2*N #=< MAX,
	integer(TOLERANCE),
	TOLERANCE >= 0,
        (L < 3 ->
	    N = 0
	;
	    get_attr1(VARIABLES, VARS),
	    last(VARS, Last),
	    big_valley_c(s, VARS, 0, 0, 2000000, TOLERANCE, Last, N)
	).

big_valley_c(s, [V1,V2|R], C, S, V, T, L, N) :-
	V1 =< V2,
	!,
	big_valley_c(s, [V2|R], C, S, V, T, L, N).
big_valley_c(s, [V1,V2|R], C, _, V, T, L, N) :-
	!,
	big_valley_c(u, [V2|R], C, V1, V, T, L, N).
big_valley_c(u, [V1,V2|R], C, S, V, T, L, N) :-
	V1 >= V2,
	!,
	big_valley_c(u, [V2|R], C, S, V, T, L, N).
big_valley_c(u, [V1,V2|R], C, S, V, T, L, N) :-
	D is S-V1,
	D =< T,
	!,
	big_valley_c(u, [V2|R], C, S, V, T, L, N).
big_valley_c(u, [V1,V2|R], C, S, _, T, L, N) :-
	!,
	big_valley_c(v, [V2|R], C, S, V1, T, L, N).
big_valley_c(v, [V1,V2|R], C, S, V, T, L, N) :-
	V1 =< V2,
	!,
	big_valley_c(v, [V2|R], C, S, V, T, L, N).
big_valley_c(v, [V1,V2|R], C, _, V, T, L, N) :-
	V = 2000000,
	!,
	big_valley_c(w, [V2|R], C, V1, V, T, L, N).
big_valley_c(v, [V1,V2|R], C, S, V, T, L, N) :-
	V < 2000000,
	D is V1-V,
	D =< T,
	!,
	big_valley_c(w, [V2|R], C, S, V, T, L, N).
big_valley_c(v, [V1,V2|R], C, _, _, T, L, N) :-
	!,
	C1 is C+1,
	big_valley_c(w, [V2|R], C1, V1, 2000000, T, L, N).
big_valley_c(w, [V1,V2|R], C, S, V, T, L, N) :-
	V1 >= V2,
	!,
	big_valley_c(w, [V2|R], C, S, V, T, L, N).
big_valley_c(w, [V1,V2|R], C, S, V, T, L, N) :-
	D is S-V1,
	D =< T,
	!,
	big_valley_c(v, [V2|R], C, S, V, T, L, N).
big_valley_c(w, [V1,V2|R], C, S, V, T, L, N) :-
	!,
	VV is min(V,V1),
	big_valley_c(v, [V2|R], C, S, VV, T, L, N).
big_valley_c(_, [_], C, _, V, T, L, N) :-
	!,
	D is L-V,
	(D > T -> C1 is C+1, N = C1 ; N = C).
big_valley_c(_, [], _, _, _, _, _, 0).

ctr_automaton_signature(big_valley, big_valley_a, pair_signature(2,signature)).

big_valley_a(FLAG, N, VARIABLES, TOLERANCE) :-
    length(VARIABLES, L),
    (L < 3 -> true ; pair_signature(VARIABLES, SIGNATURE)),
    big_valley_a_s(FLAG, N, VARIABLES, TOLERANCE, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
big_valley_a_s(FLAG, N, VARIABLES, TOLERANCE, SIGNATURE) :-
    check_type(dvar_gteq(0), N),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    MAX is max(L-1,0),
    2*N #=< MAX,
    integer(TOLERANCE),
    TOLERANCE >= 0,
    (L < 3 ->
	N #= 0 #<=> FLAG
    ;
	pair_first_last_signature(VARIABLES, VARS, Last),
	automaton(VARS, VARi,
		  SIGNATURE, 
		  [source(s),sink(s),sink(u),sink(v),sink(w)],
		  [arc(s,0,s),
		   arc(s,1,s),
		   arc(s,2,u,[C,VARi,V]),
		   arc(u,0,u,(S-VARi #=< TOLERANCE -> [C,S,V])),
		   arc(u,0,v,(S-VARi #>  TOLERANCE -> [C,S,VARi])),
		   arc(u,1,u),
		   arc(u,2,u),
		   arc(v,0,v),
		   arc(v,1,v),
		   arc(v,2,w,(V #= 2000000 -> [C,VARi,V])),
		   arc(v,2,w,(V #< 2000000 #/\ VARi-V #=< TOLERANCE -> [C,S,V])),
		   arc(v,2,w,(VARi-V #>  TOLERANCE -> [C+1,VARi,2000000])),
		   arc(w,0,v,(S-VARi #=< TOLERANCE -> [C,S,V])),
		   arc(w,0,v,(S-VARi #>  TOLERANCE -> [C,S,min(V,VARi)])),
		   arc(w,1,w),
		   arc(w,2,w)],
		  [C,S,V],[0,0,2000000],[CC,_,VV]),
	Inc in 0..1,
	Last #> VV + TOLERANCE #<=> Inc,
	CC+Inc #= N #<=> FLAG
    ).

big_valley_signature([[var-Last]], [], [], Last) :- !.
big_valley_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], [VAR1|Rs], Last) :-
    S in 0..2,
    VAR1 #< VAR2 #<=> S #= 0,
    VAR1 #= VAR2 #<=> S #= 1,
    VAR1 #> VAR2 #<=> S #= 2,
    big_valley_signature([[var-VAR2]|VARs], Ss, Rs, Last).
