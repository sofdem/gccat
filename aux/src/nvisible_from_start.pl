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

ctr_date(nvisible_from_start,['20111227']).

ctr_origin(nvisible_from_start, 'Derived from a puzzle called skyscraper', []).

ctr_arguments(nvisible_from_start,
              ['N'-dvar,
	       'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(nvisible_from_start,
                 [translate(['VARIABLES'^var])]).

ctr_synonyms(nvisible_from_start,[nvisible,
				  nvisible_from_left]).

ctr_restrictions(nvisible_from_start,
                 [required('VARIABLES',var)      ,
		  'N' >= min(1,size('VARIABLES')),
		  'N' =< size('VARIABLES')       ]).

ctr_typical(nvisible_from_start,
            [size('VARIABLES')      > 2,
	     range('VARIABLES'^var) > 2]).

ctr_pure_functional_dependency(nvisible_from_start, []).
ctr_functional_dependency(nvisible_from_start, 1, [2]).

ctr_example(nvisible_from_start,
            [nvisible_from_start(3,[[var-1],[var-6],[var-2],[var-1],[var-4],[var-8],[var-2]]),
	     nvisible_from_start(1,[[var-8],[var-6],[var-2],[var-1],[var-4],[var-8],[var-2]]),
	     nvisible_from_start(7,[[var-0],[var-2],[var-3],[var-5],[var-6],[var-7],[var-9]])]).

ctr_see_also(nvisible_from_start,
 [link('implies',    atleast_nvalue,    '',                                                              []),
  link('implied by', increasing_nvalue, '',                                                              []),
  link('related',    nvisible_from_end, 'count from the end of the sequence rather than from the start', [])]).

ctr_key_words(nvisible_from_start,['sequence'                  ,
				   'functional dependency'     ,
				   'pure functional dependency']).

ctr_eval(nvisible_from_start, [  checker(nvisible_from_start_c),
			       automaton(nvisible_from_start_a)]).

ctr_sol(nvisible_from_start,2,0,2,9,[1-6,2-3]).
ctr_sol(nvisible_from_start,3,0,3,64,[1-30,2-30,3-4]).
ctr_sol(nvisible_from_start,4,0,4,625,[1-225,2-305,3-90,4-5]).
ctr_sol(nvisible_from_start,5,0,5,7776,[1-2275,2-3675,3-1610,4-210,5-6]).
ctr_sol(nvisible_from_start,6,0,6,117649,[1-29008,2-52794,3-29400,4-6020,5-420,6-7]).
ctr_sol(nvisible_from_start,7,0,7,2097152,[1-446964,2-889056,3-583548,4-158760,5-18060,6-756,7-8]).
ctr_sol(nvisible_from_start,8,0,8,43046721,[1-8080425,2-17238570,3-12780180,4-4238367,5-661500,6-46410,7-1260,8-9]).

nvisible_from_start_c(N, VARIABLES) :-
	collection(VARIABLES, [int]),
	length(VARIABLES, L),
	MIN is min(1,L),
	check_type(dvar(MIN,L), N),
	get_attr1(VARIABLES, VARS),
        nvisible_from_start(s, VARS, 0, 0, N).

nvisible_from_start_a(FLAG, N, VARIABLES) :-
	collection(VARIABLES, [dvar]),
	length(VARIABLES, L),
	MIN is min(1,L),
	check_type(dvar(MIN,L), N),
	get_attr1(VARIABLES, VARS),
	(   foreach(_,VARS),
	    foreach(0,SIGNATURE)
	do  true
	),
	automaton(VARS, Vi,
                  SIGNATURE,
		  [source(s),sink(s),sink(t)],
		  [arc(s,0,t,[Vi,1]),
		   arc(t,0,t,(M#<Vi -> [Vi,C+1] ; M#>=Vi -> [M,C]))],
		  [M,C],[0,0],[_,COUNT]),
	COUNT #= N #<=> FLAG.
