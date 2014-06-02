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

:-dynamic big_peak_a/4.

ctr_date(big_peak,['20130125']).

ctr_origin(big_peak, 'Derived from %c.', [peak]).

ctr_arguments(big_peak,
              ['N'-dvar                        ,
               'VARIABLES'-collection(var-dvar),
	       'TOLERANCE'-int                 ]).

ctr_exchangeable(big_peak,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(big_peak,
                 ['N'   >= 0                         ,
                  2*'N' =< max(size('VARIABLES')-1,0),
                  required('VARIABLES',var)          ,
		  'TOLERANCE' >= 0                   ]).

ctr_pure_functional_dependency(big_peak, []).
ctr_functional_dependency(big_peak, 1, [2,3]).

ctr_typical(big_peak,
            ['N' >= 1                  ,
	     size('VARIABLES')      > 6,
             range('VARIABLES'^var) > 1,
	     'TOLERANCE'            > 1]).

ctr_contractible(big_peak, ['N'=0,'TOLERANCE'=0], 'VARIABLES', any).

ctr_example(big_peak,
            [big_peak(7,[[var-4],[var-2],[var-2],[var-4],[var-3],[var-8],[var-6],
			 [var-7],[var-7],[var-9],[var-5],[var-6],[var-3],[var-12],
			 [var-12],[var-6],[var-6],[var-8],[var-4],[var-5],[var-1]],0),
             big_peak(4,[[var-4],[var-2],[var-2],[var-4],[var-3],[var-8],[var-6],
			 [var-7],[var-7],[var-9],[var-5],[var-6],[var-3],[var-12],
			 [var-12],[var-6],[var-6],[var-8],[var-4],[var-5],[var-1]],1)]).

ctr_see_also(big_peak,
 [link('specialisation', peak, 'the tolerance is set to %e and removed', [0])]).

ctr_key_words(big_peak,['sequence'                  ,
                        'automaton'                 ,
                        'automaton with counters'   ,
		        'functional dependency'     ,
		        'pure functional dependency']).

ctr_eval(big_peak, [checker(big_peak_c),
		    automaton(big_peak_a),
		    automaton_with_signature(big_peak_a_s)]).

big_peak_c(N, VARIABLES, TOLERANCE) :-
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
	    big_peak_c(s, VARS, 0, 0, -2000000, TOLERANCE, Last, N)
	).

big_peak_c(s, [V1,V2|R], C, S, P, T, L, N) :-
	V1 >= V2,
	!,
	big_peak_c(s, [V2|R], C, S, P, T, L, N).
big_peak_c(s, [V1,V2|R], C, _, P, T, L, N) :-
	!,
	big_peak_c(u, [V2|R], C, V1, P, T, L, N).
big_peak_c(u, [V1,V2|R], C, S, P, T, L, N) :-
	V1 =< V2,
	!,
	big_peak_c(u, [V2|R], C, S, P, T, L, N).
big_peak_c(u, [V1,V2|R], C, S, P, T, L, N) :-
	D is V1-S,
	D =< T,
	!,
	big_peak_c(u, [V2|R], C, S, P, T, L, N).
big_peak_c(u, [V1,V2|R], C, S, _, T, L, N) :-
	!,
	big_peak_c(v, [V2|R], C, S, V1, T, L, N).
big_peak_c(v, [V1,V2|R], C, S, P, T, L, N) :-
	V1 >= V2,
	!,
	big_peak_c(v, [V2|R], C, S, P, T, L, N).
big_peak_c(v, [V1,V2|R], C, _, P, T, L, N) :-
	P = -2000000,
	!,
	big_peak_c(w, [V2|R], C, V1, P, T, L, N).
big_peak_c(v, [V1,V2|R], C, S, P, T, L, N) :-
	P > -2000000,
	D is P-V1,
	D =< T,
	!,
	big_peak_c(w, [V2|R], C, S, P, T, L, N).
big_peak_c(v, [V1,V2|R], C, _, _, T, L, N) :-
	!,
	C1 is C+1,
	big_peak_c(w, [V2|R], C1, V1, -2000000, T, L, N).
big_peak_c(w, [V1,V2|R], C, S, P, T, L, N) :-
	V1 =< V2,
	!,
	big_peak_c(w, [V2|R], C, S, P, T, L, N).
big_peak_c(w, [V1,V2|R], C, S, P, T, L, N) :-
	D is V1-S,
	D =< T,
	!,
	big_peak_c(v, [V2|R], C, S, P, T, L, N).
big_peak_c(w, [V1,V2|R], C, S, P, T, L, N) :-
	!,
	PP is max(P,V1),
	big_peak_c(v, [V2|R], C, S, PP, T, L, N).
big_peak_c(_, [_], C, _, P, T, L, N) :-
	!,
	D is P-L,
	(D > T -> C1 is C+1, N = C1 ; N = C).
big_peak_c(_, [], _, _, _, _, _, 0).

ctr_automaton_signature(big_peak, big_peak_a, pair_signature(2,signature)).

big_peak_a(FLAG, N, VARIABLES, TOLERANCE) :-
    length(VARIABLES, L),
    (L < 3 -> true ; pair_signature(VARIABLES, SIGNATURE)),
    big_peak_a_s(FLAG, N, VARIABLES, TOLERANCE, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
big_peak_a_s(FLAG, N, VARIABLES, TOLERANCE, SIGNATURE) :-
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
		  [arc(s,2,s),
		   arc(s,1,s),
		   arc(s,0,u,[C,VARi,P]),
		   arc(u,2,u,(VARi-S #=< TOLERANCE -> [C,S,P])),
		   arc(u,2,v,(VARi-S #>  TOLERANCE -> [C,S,VARi])),
		   arc(u,1,u),
		   arc(u,0,u),
		   arc(v,2,v),
		   arc(v,1,v),
		   arc(v,0,w,(P #= -2000000 -> [C,VARi,P])),
		   arc(v,0,w,(P #> -2000000 #/\ P-VARi #=< TOLERANCE -> [C,S,P])),
		   arc(v,0,w,(P-VARi #>  TOLERANCE -> [C+1,VARi,-2000000])),
		   arc(w,2,v,(VARi-S #=< TOLERANCE -> [C,S,P])),
		   arc(w,2,v,(VARi-S #>  TOLERANCE -> [C,S,max(P,VARi)])),
		   arc(w,1,w),
		   arc(w,0,w)],
		  [C,S,P],[0,0,-2000000],[CC,_,PP]),
	Inc in 0..1,
	PP #> Last+TOLERANCE #<=> Inc,
	CC+Inc #= N #<=> FLAG
    ).
