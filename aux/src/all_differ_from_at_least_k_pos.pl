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
    ctr_extensible/2,
    ctr_extensible/3,
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

ctr_date(all_differ_from_at_least_k_pos,['20030820','20040530','20060803']).

ctr_origin(all_differ_from_at_least_k_pos, 'Inspired by \\cite{Frutos97}.', []).

ctr_types(all_differ_from_at_least_k_pos, ['VECTOR'-collection(var-dvar)]).

ctr_arguments(all_differ_from_at_least_k_pos,
              ['K'-int                           ,
               'VECTORS'-collection(vec-'VECTOR')]).

ctr_exchangeable(all_differ_from_at_least_k_pos,
                 [items('VECTORS',all),
                  items_sync('VECTORS'^vec,all)]).

ctr_restrictions(all_differ_from_at_least_k_pos,
                 [required('VECTOR',var)  ,
                   size('VECTOR') >=  1   ,
                   size('VECTOR') >= 'K'  ,
                  'K'             >=  0   ,
                  required('VECTORS',vec) ,
                  same_size('VECTORS',vec)]).

ctr_typical(all_differ_from_at_least_k_pos,
            ['K'             > 0,
             size('VECTORS') > 1]).

ctr_contractible(all_differ_from_at_least_k_pos, [], 'VECTORS', any).

ctr_extensible(all_differ_from_at_least_k_pos, [], 'VECTORS'^vec, any).

ctr_graph(all_differ_from_at_least_k_pos,
          ['VECTORS'],
          2,
          ['CLIQUE'(=\=)>>collection(vectors1,vectors2)],
          [differ_from_at_least_k_pos('K',vectors1^vec,vectors2^vec)],
          ['NARC' = size('VECTORS')*size('VECTORS') - size('VECTORS')],
          ['NO_LOOP','SYMMETRIC']).

ctr_example(all_differ_from_at_least_k_pos,
            all_differ_from_at_least_k_pos(2,
                                           [[vec-[[var-2], [var-5], [var-2], [var-0]]],
                                            [vec-[[var-3], [var-6], [var-2], [var-1]]],
                                            [vec-[[var-3], [var-6], [var-1], [var-0]]]])).

ctr_draw_example(all_differ_from_at_least_k_pos,
                 ['VECTORS'],
                 [[[vec-[[var-2], [var-5], [var-2], [var-0]]],
                   [vec-[[var-3], [var-6], [var-2], [var-1]]],
                   [vec-[[var-3], [var-6], [var-1], [var-0]]]]],
                 ['CLIQUE'(=\=)],
                 [1-[2,3],2-[1,3],3-[1,2]],
                 ['NARC'],
                  '','NARC=6',
                 []).

ctr_cond_imply(all_differ_from_at_least_k_pos, atleast_nvector, ['K' =< size('VECTORS')], [], id).

ctr_see_also(all_differ_from_at_least_k_pos,
 [link('part of system of constraints', differ_from_at_least_k_pos,    '',                              []       ),
  link('used in graph description',     differ_from_at_least_k_pos,    '',                              []       ),
  link('implied by',                    all_differ_from_exactly_k_pos, '$\\geq$ %e replaced by $=$ %e', ['K','K'])]).

ctr_key_words(all_differ_from_at_least_k_pos,['system of constraints',
                                              'decomposition'        ,
                                              'disequality'          ,
                                              'bioinformatics'       ,
                                              'vector'               ,
                                              'no loop'              ,
                                              'symmetric'            ]).

ctr_persons(all_differ_from_at_least_k_pos,['Frutos A. G.'   ,
                                            'Liu Q.'         ,
                                            'Thiel A. J.'    ,
                                            'Sanner A. M. W.',
                                            'Condon A. E.'   ,
                                            'Smith L. M.'    ,
                                            'Corn R. M.'     ]).

ctr_eval(all_differ_from_at_least_k_pos, [reformulation(all_differ_from_at_least_k_pos_r),
					        checker(all_differ_from_at_least_k_pos_c),
					        density(all_differ_from_at_least_k_pos_d)]).

all_differ_from_at_least_k_pos_r(K, VECTORS) :-
    integer(K),
    K >= 0,
    all_differ_from_at_least_k_pos_rr(VECTORS, K).

all_differ_from_at_least_k_pos_c(K, []) :-
    !,
    integer(K),
    K >= 0.
% K=0: succeed
all_differ_from_at_least_k_pos_c(0, VECTORS) :- !,
    collection(VECTORS, [col([int])]),
    VECTORS = [[_-VECTOR]|_],
    length(VECTOR, N),
    N >= 1,
    same_size(VECTORS).
% K=1: like lex_alldifferent
all_differ_from_at_least_k_pos_c(1, VECTORS) :- !,
    collection(VECTORS, [col([int])]),
    VECTORS = [[_-VECTOR]|_],
    length(VECTOR, N),
    N >= 1,
    same_size(VECTORS),
    length(VECTORS, L),
    sort(VECTORS, SVECTORS),
    length(SVECTORS, L).
% K=N: like a conjunction of alldifferent on every components
all_differ_from_at_least_k_pos_c(K, VECTORS) :-
    integer(K),
    VECTORS = [[_-VECTOR]|_],
    length(VECTOR, N),
    K = N,
    !,
    N >= 1,
    collection(VECTORS, [col([int])]),
    same_size(VECTORS),
    get_attr11(VECTORS, VECTS),
    transpose(VECTS, TVECTS),
    length(VECTORS, M),
    all_differ_from_at_least_k_pos_distinct_comp(TVECTS, M).
% K>1, K<N: try first to avoid pairwise comparison by generating pairs of vectors sharing the same value on a same component
% but sometimes pairwise comparison not so bad since allow to dectect failure early
%all_differ_from_at_least_k_pos_c(K, VECTORS) :-
%    integer(K),
%    collection(VECTORS, [col([int])]),
%    VECTORS = [[_-VECTOR]|_],
%    length(VECTOR, N),
%    N >= 1,
%    N >= K,
%    same_size(VECTORS),
%    get_attr11(VECTORS, VECTS),
%    all_differ_from_at_least_k_pos_gen_triples(VECTS, 1, TRIPLES, TRIPLES),
%    sort(TRIPLES, SORTED_TRIPLES),
%    length(TRIPLES, M),
%    length(SORTED_TRIPLES, M), % since K>0
%    all_differ_from_at_least_k_pos_regroup(SORTED_TRIPLES, GROUPS),
%    all_differ_from_at_least_k_pos_length(GROUPS, 0, LEN),
%    length(VECTORS, NBVECS),
%    LIM is min(NBVECS*NBVECS, 200000),
%    (LEN < LIM ->
%	% avoid pairwise comparaison
%	write(avoid_pairwise_comparaison), nl,
%	all_differ_from_at_least_k_pos_gen_pairs(GROUPS, 1, PAIRS, PAIRS),
%        sort(PAIRS, SPAIRS),
%        LIMIT is N-K+1,
%        all_differ_from_at_least_k_pos_check_pairs(SPAIRS, 1, LIMIT)
%    ;
%	% use pairwise comparaison since too many vectors have some components in common
%	all_differ_from_at_least_k_pos_cc(VECTORS, N, K)
%    ).
% K>1, K<N
all_differ_from_at_least_k_pos_c(K, VECTORS) :-
    integer(K),
    collection(VECTORS, [col([int])]),
    VECTORS = [[_-VECTOR]|_],
    length(VECTOR, N),
    N >= 1,
    N >= K,
    same_size(VECTORS),
    get_attr11(VECTORS, VECTS),
    all_differ_from_at_least_k_pos_cc(VECTS, N, K).

% for every component check that the corresponding values are all different (used when K=N)
all_differ_from_at_least_k_pos_distinct_comp([], _) :- !.
all_differ_from_at_least_k_pos_distinct_comp([L|R], N) :-
    sort(L, S),
    length(S, N),
    all_differ_from_at_least_k_pos_distinct_comp(R, N).

% check that number of identical pairs of vectors having a same component does not exceed LIMIT
all_differ_from_at_least_k_pos_check_pairs([], _, _) :- !.
all_differ_from_at_least_k_pos_check_pairs([_], _, _) :- !.
all_differ_from_at_least_k_pos_check_pairs([t(I,J)-_,t(I,J)-K|R], CUR, LIMIT) :- !,
    NEXT is CUR+1,
    NEXT =< LIMIT,
    all_differ_from_at_least_k_pos_check_pairs([t(I,J)-K|R], NEXT, LIMIT).
all_differ_from_at_least_k_pos_check_pairs([_,t(I,J)-K|R], _CUR, LIMIT) :-
    all_differ_from_at_least_k_pos_check_pairs([t(I,J)-K|R], 1, LIMIT).

% generates pairs of vectors ids that have a common component
% add unique key so that sort dont remove duplicates
all_differ_from_at_least_k_pos_gen_pairs([], _, [], _) :- !.
all_differ_from_at_least_k_pos_gen_pairs([L|R], Id, CONT, RES) :-
    all_differ_from_at_least_k_pos_gen_pair(L, Id, Id1, CONT, NEW_CONT),
    all_differ_from_at_least_k_pos_gen_pairs(R, Id1, NEW_CONT, RES).

all_differ_from_at_least_k_pos_gen_pair([], Id, Id, CONT, CONT) :- !.
all_differ_from_at_least_k_pos_gen_pair([X|Y], Id, NewId, CONT, NEW_CONT) :-
    all_differ_from_at_least_k_pos_gen_pair(Y, X, Id, Id1, CONT, CCONT),
    all_differ_from_at_least_k_pos_gen_pair(Y, Id1, NewId, CCONT, NEW_CONT).

all_differ_from_at_least_k_pos_gen_pair([], _, Id, Id, CONT, CONT) :- !.
all_differ_from_at_least_k_pos_gen_pair([X|Y], Z, Id, NewId, CONT, CCONT) :-
    MIN is min(X,Z),
    MAX is max(X,Z),
    CONT = [t(MIN,MAX)-Id|NEW_CONT],
    Id1 is Id+1,
    all_differ_from_at_least_k_pos_gen_pair(Y, Z, Id1, NewId, NEW_CONT, CCONT).

% compute total number of pairs that will be generated from the different equivalence classes,
% where an equivalence class corresponds to a maximum set of vectors sharing a same value at a same component
all_differ_from_at_least_k_pos_length([], LEN, LEN) :- !.
all_differ_from_at_least_k_pos_length([L|R], CUR, RES) :-
    length(L, N),
    NEXT is CUR + (N*(N-1))//2,
    all_differ_from_at_least_k_pos_length(R, NEXT, RES).

% from the sorted list of triples build equivalence classes of triples t(Value,Component_number,Vector_id)
% that share the same value on a same component (remove equivalence classes that contain one single element)
all_differ_from_at_least_k_pos_regroup([], []) :- !.
all_differ_from_at_least_k_pos_regroup([t(Val,Pos,Id)|Y], RES) :-
    all_differ_from_at_least_k_pos_prefix([t(Val,Pos,Id)|Y], t(Val,Pos,Id), P, Rest),
    (length(P, 1) -> RES = R ; RES =  [P|R]),
    all_differ_from_at_least_k_pos_regroup(Rest, R).

all_differ_from_at_least_k_pos_prefix([t(Val,Pos,Id)|Y], t(Val,Pos,Jd), [Id|S], R) :- !,
	all_differ_from_at_least_k_pos_prefix(Y, t(Val,Pos,Jd), S, R).
all_differ_from_at_least_k_pos_prefix(L, _, [], L).

% generate triples of the form t(Value,Component_number,Vector_id) by scanning the different vectors
all_differ_from_at_least_k_pos_gen_triples([], _, [], _) :- !.
all_differ_from_at_least_k_pos_gen_triples([VECTOR|R], ID, CONTINUATION, TRIPLES) :-
    all_differ_from_at_least_k_pos_gen_triples1(VECTOR, 1, ID, CONTINUATION, NEW_CONTINUATION, TRIPLES),
    ID1 is ID+1,
    all_differ_from_at_least_k_pos_gen_triples(R, ID1, NEW_CONTINUATION, TRIPLES).

all_differ_from_at_least_k_pos_gen_triples1([], _, _, CONTINUATION, CONTINUATION, _) :- !.
all_differ_from_at_least_k_pos_gen_triples1([V|R], I, ID, CONTINUATION, NEW_CONTINUATION, TRIPLES) :-
    CONTINUATION = [t(V,I,ID)|CONT],
    I1 is I+1,
    all_differ_from_at_least_k_pos_gen_triples1(R, I1, ID, CONT, NEW_CONTINUATION, TRIPLES).

%------------------------------------------------------------------------------------------------------------
all_differ_from_at_least_k_pos_rr([], _) :- !.
all_differ_from_at_least_k_pos_rr([[_-VECTOR1]|R], K) :-
    length(VECTOR1, N),
    N >= 1,
    N >= K,
    all_differ_from_at_least_k_pos_rr(R, VECTOR1, K),
    all_differ_from_at_least_k_pos_rr(R, K).

all_differ_from_at_least_k_pos_rr([], _, _).
all_differ_from_at_least_k_pos_rr([[_-VECTOR2]|R], VECTOR1, K) :-
    eval(differ_from_at_least_k_pos(K, VECTOR1, VECTOR2)),
    all_differ_from_at_least_k_pos_rr(R, VECTOR1, K).
%------------------------------------------------------------------------------------------------------------
all_differ_from_at_least_k_pos_cc([], _, _) :- !.
all_differ_from_at_least_k_pos_cc([VECTOR1|R], N, K) :-
    all_differ_from_at_least_k_pos_cc(R, VECTOR1, N, K),
    all_differ_from_at_least_k_pos_cc(R, N, K).

all_differ_from_at_least_k_pos_cc([], _, _, _).
all_differ_from_at_least_k_pos_cc([VECTOR2|R], VECTOR1, N, K) :-
    all_differ_from_at_least_k_pos_check(VECTOR1, VECTOR2, N, K),
    all_differ_from_at_least_k_pos_cc(R, VECTOR1, N, K).

all_differ_from_at_least_k_pos_check([], [], _, 0) :- !.
all_differ_from_at_least_k_pos_check([U|R], [V|S], N, K) :-
    (U = V ->
	NewK is K
    ;
	NewK is K-1
    ),
    (NewK =< 0 ->
	true
    ; 
	NewN is N-1,
	NewK =< NewN,
	all_differ_from_at_least_k_pos_check(R, S, NewN, NewK)
    ).
%------------------------------------------------------------------------------------------------------------

all_differ_from_at_least_k_pos_d(0, _, []) :- !.
all_differ_from_at_least_k_pos_d(Density, K, [[_-VECTOR]|_]) :-
    length(VECTOR, Available),
    Density is K/Available.
