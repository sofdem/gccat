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

ctr_date(golomb,['20000128','20030820','20040530','20060809']).

ctr_origin(golomb, 'Inspired by \\cite{Golomb72}.', []).

ctr_arguments(golomb,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(golomb,
                 [translate(['VARIABLES'^var])]).

ctr_restrictions(golomb,
                 [required('VARIABLES',var)       ,
                  'VARIABLES'^var >= 0            ,
                  strictly_increasing('VARIABLES')]).

ctr_typical(golomb,
            [size('VARIABLES') > 2]).

ctr_contractible(golomb, [], 'VARIABLES', any).

ctr_derived_collections(golomb,
                        [col('PAIRS'-collection(x-dvar, y-dvar),
                             [(>)-item(x-'VARIABLES'^var, y-'VARIABLES'^var)])]).

ctr_graph(golomb,
          ['PAIRS'],
          2,
          ['CLIQUE'>>collection(pairs1,pairs2)],
          [pairs1^y - pairs1^x = pairs2^y - pairs2^x],
          ['MAX_NSCC' =< 1],
          []).

ctr_example(golomb,
            golomb([[var-0],[var-1],[var-4],[var-6]])).

ctr_draw_example(golomb,
                 ['PAIRS'],
                 [[[x-1,y-0],[x-4,y-0],[x-4,y-1],
                   [x-6,y-0],[x-6,y-1],[x-6,y-4]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3,4-4,5-5,6-6],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 [2.5,2.145,4,1.8]).

ctr_cond_imply(golomb, increasing_nvalue,     [], ['NVAL'= nval('VARIABLES'^var)], [none, 'VARIABLES']).
ctr_cond_imply(golomb, soft_alldifferent_ctr, [], [], [none, 'VARIABLES']).

ctr_see_also(golomb,
 [link('implies',        strictly_increasing, '',   []),
  link('common keyword', alldifferent,        '%k', ['all different'])]).

ctr_key_words(golomb,['Golomb ruler'      ,
                      'disequality'       ,
                      'difference'        ,
                      'all different'     ,
                      'derived collection']).

ctr_persons(golomb,['Golomb S. W.' ,
                    'Shearer J. B.',
                    'Smith B. M.'  ,
                    'Stergiou K.'  ,
                    'Walsh T.'     ]).

ctr_eval(golomb, [checker(golomb_c),
		  reformulation(golomb_r)]).

ctr_sol(golomb,2,0,2,3,-).
ctr_sol(golomb,3,0,3,2,-).
ctr_sol(golomb,4,0,6,2,-).
ctr_sol(golomb,5,0,11,4,-).
ctr_sol(golomb,6,0,17,8,-).
ctr_sol(golomb,7,0,25,10,-).
ctr_sol(golomb,8,0,34,2,-).
ctr_sol(golomb,9,0,44,2,-).
ctr_sol(golomb,10,0,55,2,-).
ctr_sol(golomb,11,0,72,4,-).

golomb_c([]) :- !.
golomb_c(VARIABLES) :-
	collection(VARIABLES, [int_gteq(0)]),
	golomb_increasing(VARIABLES),
	get_attr1(VARIABLES, VARS),
	golomb3(VARS, D),
	sort(D, SD),
	length(D, N),
	length(SD, N).

golomb_increasing([]) :- !.
golomb_increasing([_]) :- !.
golomb_increasing([[var-X],[var-Y]|R]) :-
	X < Y,
	golomb_increasing([[var-Y]|R]).

golomb_r([]) :- !.
golomb_r(VARIABLES) :-
	collection(VARIABLES, [dvar_gteq(0)]),
	collection_increasing_seq(VARIABLES,[1]),
	get_attr1(VARIABLES, VARS),
	golomb1(VARS, D),
	all_different(D).

golomb1([_], []) :- !.
golomb1([U,V|R], Diffs) :-
	golomb2([V|R], U, D),
	golomb1([V|R], Diff),
	append(D, Diff, Diffs).

golomb2([], _, []).
golomb2([Vi|R], Vj, [D|S]) :-
	D #= Vi - Vj,
	golomb2(R, Vj, S).

golomb3([_], []) :- !.
golomb3([U,V|R], Diffs) :-
	golomb4([V|R], U, D),
	sort(D, SD),
	length(D, N),
	length(SD, N),
	golomb3([V|R], Diff),
	append(SD, Diff, Diffs).

golomb4([], _, []).
golomb4([Vi|R], Vj, [D|S]) :-
	D is Vi - Vj,
	golomb4(R, Vj, S).
