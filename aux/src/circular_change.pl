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

:-dynamic circular_change_a/3.

ctr_date(circular_change,['20030820','20040530','20060805']).

ctr_origin(circular_change, 'Derived from %c.', [change]).

ctr_arguments(circular_change,
              ['NCHANGE'-dvar                  ,
               'VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ]).

ctr_exchangeable(circular_change,
                 [items('VARIABLES',shift),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(circular_change,
                 ['NCHANGE' >=  0                 ,
                  'NCHANGE' =< size('VARIABLES')  ,
                  required('VARIABLES',var)       ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(circular_change,
            ['NCHANGE'              > 0,
             size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1,
             in_list('CTR',[=\=])      ]).

ctr_pure_functional_dependency(circular_change, []).
ctr_functional_dependency(circular_change, 1, [2,3]).

ctr_graph(circular_change,
          ['VARIABLES'],
          2,
          ['CIRCUIT'>>collection(variables1,variables2)],
          ['CTR'(variables1^var,variables2^var)],
          ['NARC' = 'NCHANGE'],
          []).

ctr_example(circular_change,
            circular_change(4,[[var-4],[var-4],[var-3],[var-4],[var-1]],=\=)).

ctr_draw_example(circular_change,
                 ['VARIABLES'],
                 [[[var-4],[var-4],[var-3],[var-4],[var-1]]],
                 ['CIRCUIT'],
                 [2-3,3-4,4-5,5-1],
                 ['NARC'],
                 '','NARC=4',
                 []).

ctr_see_also(circular_change,
 [link('common keyword', change, '%k', ['number of changes'])]).

ctr_key_words(circular_change,['timetabling constraint'                          ,
                               'number of changes'                               ,
                               'cyclic'                                          ,
                               'automaton'                                       ,
                               'automaton with counters'                         ,
                               'circular sliding cyclic(1) constraint network(2)',
                               'functional dependency'                           ,
			       'pure functional dependency'                      ]).

ctr_eval(circular_change, [  checker(circular_change_c),
			   automaton(circular_change_a)]).

circular_change_c(NCHANGE, VARIABLES, =) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), NCHANGE),
    (N = 0 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
	VARS = [V|_],
	append(VARS, [V], NEWVARS),
        change_eq_c(NEWVARS, 0, NCHANGE)
    ).
circular_change_c(NCHANGE, VARIABLES, =\=) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), NCHANGE),
    (N = 0 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
	VARS = [V|_],
	append(VARS, [V], NEWVARS),
        change_neq_c(NEWVARS, 0, NCHANGE)
    ).
circular_change_c(NCHANGE, VARIABLES, <) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), NCHANGE),
    (N = 0 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
	VARS = [V|_],
	append(VARS, [V], NEWVARS),
        change_lt_c(NEWVARS, 0, NCHANGE)
    ).
circular_change_c(NCHANGE, VARIABLES, >=) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), NCHANGE),
    (N = 0 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
	VARS = [V|_],
	append(VARS, [V], NEWVARS),
        change_geq_c(NEWVARS, 0, NCHANGE)
    ).
circular_change_c(NCHANGE, VARIABLES, >) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), NCHANGE),
    (N = 0 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
	VARS = [V|_],
	append(VARS, [V], NEWVARS),
        change_gt_c(NEWVARS, 0, NCHANGE)
    ).
circular_change_c(NCHANGE, VARIABLES, =<) :-
    collection(VARIABLES, [int]),
    length(VARIABLES, N),
    check_type(dvar(0,N), NCHANGE),
    (N = 0 ->
	NCHANGE #= 0
    ;
	get_attr1(VARIABLES, VARS),
	VARS = [V|_],
	append(VARS, [V], NEWVARS),
        change_leq_c(NEWVARS, 0, NCHANGE)
    ).

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
circular_change_a(FLAG, NCHANGE, VARIABLES, CTR) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    check_type(dvar(0,N), NCHANGE),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    VARIABLES = [V1|_],
    append(VARIABLES, [V1], CVARIABLES),
    circular_change_signature(CVARIABLES, SIGNATURE, CTR),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NCHANGE #<=> FLAG.

circular_change_signature([], [], _).
circular_change_signature([_], [], _) :- !.
circular_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =) :- !,
    VAR1 #= VAR2 #<=> S,
	circular_change_signature([[var-VAR2]|VARs], Ss, =).
circular_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =\=) :- !,
    VAR1 #\= VAR2 #<=> S,
	circular_change_signature([[var-VAR2]|VARs], Ss, =\=).
circular_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], <) :- !,
    VAR1 #< VAR2 #<=> S,
	circular_change_signature([[var-VAR2]|VARs], Ss, <).
circular_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], >=) :- !,
    VAR1 #>= VAR2 #<=> S,
	circular_change_signature([[var-VAR2]|VARs], Ss, >=).
circular_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], >) :- !,
    VAR1 #> VAR2 #<=> S,
	circular_change_signature([[var-VAR2]|VARs], Ss, >).
circular_change_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], =<) :- !,
    VAR1 #=< VAR2 #<=> S,
	circular_change_signature([[var-VAR2]|VARs], Ss, =<).
