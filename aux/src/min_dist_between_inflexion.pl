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
    ctr_automaton_signature/3,
    ctr_sol/6,
    ctr_logic/3,
    ctr_application/2.

ctr_date(min_dist_between_inflexion,['20121023']).

ctr_origin(min_dist_between_inflexion, 'Derived from %c', [inflexion]).

ctr_arguments(min_dist_between_inflexion,
              ['MINDIST'-int                   ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(min_dist_between_inflexion,
                 [items('VARIABLES',reverse),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(min_dist_between_inflexion,
                 ['MINDIST' >= 0                ,
                  'MINDIST' =< size('VARIABLES'),
                  required('VARIABLES',var)     ]).

ctr_typical(min_dist_between_inflexion,
            ['MINDIST'              > 1,
	     size('VARIABLES')      > 3,
             range('VARIABLES'^var) > 1]).

ctr_total_relation(min_dist_between_inflexion).

ctr_example(min_dist_between_inflexion,
	    min_dist_between_inflexion(2,[[var-2],[var-2],[var-3],[var-3],[var-2],[var-2],[var-1],[var-4],[var-4],[var-3]])).

ctr_see_also(min_dist_between_inflexion,
 [link('common keyword', inflexion,                   '%k', [sequence]),
  link('common keyword', peak,                        '%k', [sequence]),
  link('common keyword', valley,                      '%k', [sequence]),
  link('common keyword', longest_decreasing_sequence, '%k', [sequence]),
  link('common keyword', longest_increasing_sequence, '%k', [sequence])]).

ctr_key_words(min_dist_between_inflexion,['sequence'                               ,
                                          'automaton'                              ,
                                          'automaton with counters'                ,
                                          'automaton with same input symbol'       ,
                                          'sliding cyclic(1) constraint network(3)']).

ctr_eval(min_dist_between_inflexion, [checker(min_dist_between_inflexion_c),
				      automaton(min_dist_between_inflexion_a),
				      automaton_with_signature(min_dist_between_inflexion_a_s)]).

ctr_sol(min_dist_between_inflexion,2,0,2,9,[2-9]).
ctr_sol(min_dist_between_inflexion,3,0,3,64,[3-64]).
ctr_sol(min_dist_between_inflexion,4,0,4,1135,[1-170,2-170,3-170,4-625]).
ctr_sol(min_dist_between_inflexion,5,0,5,25444,[1-3598,2-4690,3-4690,4-4690,5-7776]).
ctr_sol(min_dist_between_inflexion,6,0,6,574483,[1-73794,2-91098,3-97314,4-97314,5-97314,6-117649]).
ctr_sol(min_dist_between_inflexion,7,0,7,13287476,[1-1543512,2-1819764,3-1932012,4-1965012,5-1965012,6-1965012,7-2097152]).
ctr_sol(min_dist_between_inflexion,8,0,8,328156407,[1-35152278,2-39992562,3-41360676,4-42025560,5-42192870,6-42192870,7-42192870,8-43046721]).

min_dist_between_inflexion_c(MINDIST, VARIABLES) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    check_type(dvar(0,L), MINDIST),
    get_attr1(VARIABLES, VARS),
    min_dist_between_inflexion_c(VARS, s, L, 1, MINDIST).

min_dist_between_inflexion_c([V,V|VARS], s, D, C, MINDIST) :-
    !,
    min_dist_between_inflexion_c([V|VARS], s, D, C, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], s, D, C, MINDIST) :-
    Vi < Vj,
    !,
    min_dist_between_inflexion_c([Vj|VARS], i0, D, C, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], s, D, C, MINDIST) :-
    Vi > Vj,
    !,
    min_dist_between_inflexion_c([Vj|VARS], d0, D, C, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], i0, D, C, MINDIST) :-
    Vi =< Vj,
    !,
    min_dist_between_inflexion_c([Vj|VARS], i0, D, C, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], i0, D, C, MINDIST) :-
    Vi > Vj,
    !,
    min_dist_between_inflexion_c([Vj|VARS], d1, D, C, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], d0, D, C, MINDIST) :-
    Vi >= Vj,
    !,
    min_dist_between_inflexion_c([Vj|VARS], d0, D, C, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], d0, D, C, MINDIST) :-
    Vi < Vj,
    !,
    min_dist_between_inflexion_c([Vj|VARS], i1, D, C, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], d1, D, C, MINDIST) :-
    Vi >= Vj,
    !,
    C1 is C+1,
    min_dist_between_inflexion_c([Vj|VARS], d1, D, C1, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], d1, D, C, MINDIST) :-
    Vi < Vj,
    !,
    D1 is min(D,C),
    min_dist_between_inflexion_c([Vj|VARS], i1, D1, 1, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], i1, D, C, MINDIST) :-
    Vi =< Vj,
    !,
    C1 is C+1,
    min_dist_between_inflexion_c([Vj|VARS], i1, D, C1, MINDIST).
min_dist_between_inflexion_c([Vi,Vj|VARS], i1, D, C, MINDIST) :-
    Vi > Vj,
    !,
    D1 is min(D,C),
    min_dist_between_inflexion_c([Vj|VARS], d1, D1, 1, MINDIST).
min_dist_between_inflexion_c(_, _, D, _, MINDIST) :-
    (integer(MINDIST) ->
	MINDIST >= D
    ;
	MINDIST #= D % fix MINDIST to minimum distance if not initially fixed 
    ).               % (used in the context of learning)


ctr_automaton_signature(min_dist_between_inflexion, min_dist_between_inflexion_a, pair_signature(2,signature)).

min_dist_between_inflexion_a(FLAG, MINDIST, VARIABLES) :-
    pair_signature(VARIABLES, SIGNATURE),
    min_dist_between_inflexion_a_s(FLAG, MINDIST, VARIABLES, SIGNATURE).

% 0: VAR1<VAR2
% 1: VAR1=VAR2
% 2: VAR1>VAR2
min_dist_between_inflexion_a_s(FLAG, MINDIST, VARIABLES, SIGNATURE) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, L),
    check_type(dvar(0,L), MINDIST),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s),sink(i0),sink(d0),sink(i1),sink(d1)],
              [arc(s,1,s),
               arc(s,0,i0),
               arc(s,2,d0),
               arc(i0,1,i0),
               arc(i0,0,i0),
               arc(i0,2,d1),
               arc(d0,1,d0),
               arc(d0,0,i1),
               arc(d0,2,d0),
               arc(i1,1,i1,[D,C+1]),
               arc(i1,0,i1,[D,C+1]),
               arc(i1,2,d1,[min(D,C),1]),
               arc(d1,1,d1,[D,C+1]),
               arc(d1,0,i1,[min(D,C),1]),
               arc(d1,2,d1,[D,C+1])],
              [D,C],[L,1],[DIST, _]),
    MINDIST #>= DIST #<=> FLAG,
    (integer(MINDIST) -> true ;
     FLAG = 0         -> true ;
	                 MINDIST #= DIST). % fix MINDIST to minimum distance if not initially fixed (used in the context of learning)
