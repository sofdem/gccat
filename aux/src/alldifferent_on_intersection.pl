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

ctr_date(alldifferent_on_intersection,['20040530','20060803']).

ctr_origin(alldifferent_on_intersection, 'Derived from %c and %c.', [common,alldifferent]).

ctr_arguments(alldifferent_on_intersection,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(alldifferent_on_intersection,
                 [args([['VARIABLES1','VARIABLES2']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_synonyms(alldifferent_on_intersection,[alldiff_on_intersection    ,
                                           alldistinct_on_intersection]).

ctr_restrictions(alldifferent_on_intersection,
                 [required('VARIABLES1',var),
                  required('VARIABLES2',var)]).

ctr_typical(alldifferent_on_intersection,
            [size('VARIABLES1') > 1,
             size('VARIABLES2') > 1]).

ctr_contractible(alldifferent_on_intersection, [], 'VARIABLES1', any).
ctr_contractible(alldifferent_on_intersection, [], 'VARIABLES2', any).

ctr_graph(alldifferent_on_intersection,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['MAX_NCC' =< 2],
	      ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(alldifferent_on_intersection,
            alldifferent_on_intersection([[var-5],[var-9],[var-1],[var-5]],
                                         [[var-2],[var-1],[var-6],[var-9],[var-6],[var-2]])).

ctr_draw_example(alldifferent_on_intersection,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-5],[var-9],[var-1],[var-5]],
                  [[var-2],[var-1],[var-6],[var-9],[var-6],[var-2]]],
                 ['PRODUCT'],
                 [2-4,
                  3-2],
                 ['MAX_NCC'([2,8])],
                 '','MAX_NCC=2',
                 [2.145,2.145,2.145,1.5]).

ctr_see_also(alldifferent_on_intersection,
 [link('root concept',   alldifferent,           '',   []),
  link('implies',        same_intersection,      '',   []),
  link('implied by',     disjoint,               '',   []),
  link('common keyword', common,                 '%k', ['constraint on the intersection']),
  link('common keyword', nvalue_on_intersection, '%k', ['constraint on the intersection'])]).

ctr_key_words(alldifferent_on_intersection,['constraint between two collections of variables',
                                            'constraint on the intersection'                 ,
                                            'value constraint'                               ,
                                            'all different'                                  ,
                                            'connected component'                            ,
                                            'automaton'                                      ,
                                            'automaton with array of counters'               ,
                                            'acyclic'                                        ,
                                            'bipartite'                                      ,
                                            'no loop'                                        ]).

ctr_eval(alldifferent_on_intersection, [reformulation(alldifferent_on_intersection_r)]).

alldifferent_on_intersection_r(VARIABLES1, VARIABLES2) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
	alldifferent_on_intersection(VARS1, 1, VARS1, VARS2).

alldifferent_on_intersection([], _, _, _).
alldifferent_on_intersection([VAR1|R1], I, VARS1, VARS2) :-
	alldifferent_on_intersection(VARS2, 1, VAR1, I, VARS1, VARS2),
	I1 is I+1,
	alldifferent_on_intersection(R1, I1, VARS1, VARS2).

alldifferent_on_intersection([], _, _, _, _, _).
alldifferent_on_intersection([VAR2|R2], J, VAR1, I, VARS1, VARS2) :-
	alldifferent_on_intersection1(VARS1, 1, VAR1, I, VAR2, J),
	alldifferent_on_intersection1(VARS2, 1, VAR2, J, VAR1, I),
	J1 is J+1,
	alldifferent_on_intersection(R2, J1, VAR1, I, VARS1, VARS2).

alldifferent_on_intersection1([], _, _, _, _, _).
alldifferent_on_intersection1([VAR|R], K, VAR1, I, VAR2, J) :-	K =\= I, !,
	VAR1 #= VAR2 #=> VAR #\= VAR1,
	K1 is K+1,
	alldifferent_on_intersection1(R, K1, VAR1, I, VAR2, J).
alldifferent_on_intersection1([_|R], K, VAR1, I, VAR2, J) :-	K =:= I,
	K1 is K+1,
	alldifferent_on_intersection1(R, K1, VAR1, I, VAR2, J).
