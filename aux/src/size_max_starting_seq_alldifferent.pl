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

ctr_date(size_max_starting_seq_alldifferent,['20030820','20060814','20090524','20121124']).

ctr_origin(size_max_starting_seq_alldifferent, 'Inspired by %c.', [size_max_seq_alldifferent]).

ctr_arguments(size_max_starting_seq_alldifferent,
              ['SIZE'-dvar                     ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(size_max_starting_seq_alldifferent,
                 [translate(['VARIABLES'^var])]).

ctr_synonyms(size_max_starting_seq_alldifferent,[size_maximal_starting_sequence_alldiff     ,
                                                 size_maximal_starting_sequence_alldistinct ,
                                                 size_maximal_starting_sequence_alldifferent]).

ctr_restrictions(size_max_starting_seq_alldifferent,
                 ['SIZE' >= 0                ,
                  'SIZE' =< size('VARIABLES'),
                  required('VARIABLES',var)  ]).

ctr_typical(size_max_starting_seq_alldifferent,
            ['SIZE'                 > 2                ,
             'SIZE'                 < size('VARIABLES'),
             range('VARIABLES'^var) > 1                ]).

ctr_pure_functional_dependency(size_max_starting_seq_alldifferent, []).
ctr_functional_dependency(size_max_starting_seq_alldifferent, 1, [2]).

ctr_graph(size_max_starting_seq_alldifferent,
          ['VARIABLES'],
          *,
          ['PATH_1'>>collection],
          [alldifferent(collection)],
          ['NARC' = 'SIZE'],
          []).

ctr_example(size_max_starting_seq_alldifferent,
            [size_max_starting_seq_alldifferent(4, [[var-9],[var-2],[var-4],[var-5],[var-2],[var-7],[var-4]]),
             size_max_starting_seq_alldifferent(7, [[var-9],[var-2],[var-4],[var-5],[var-1],[var-7],[var-8]]),
             size_max_starting_seq_alldifferent(6, [[var-9],[var-2],[var-4],[var-5],[var-1],[var-7],[var-9]])]).

ctr_see_also(size_max_starting_seq_alldifferent,
 [link('implies',        atleast_nvalue,            '',      []),
  link('common keyword', alldifferent,              '%k,%k', ['all different', 'disequality']),
  link('common keyword', size_max_seq_alldifferent, '%k,%k', ['all different', 'disequality']),
  link('common keyword', open_alldifferent,         '%k,%k', ['all different', 'disequality'])]).

ctr_key_words(size_max_starting_seq_alldifferent,
              ['sliding sequence constraint',
               'open constraint'            ,
               'conditional constraint'     ,
               'sequence'                   ,
               'all different'              ,
               'disequality'                ,
               'hypergraph'                 ,
               'functional dependency'      ,
	       'pure functional dependency' ]).

ctr_persons(size_max_starting_seq_alldifferent,['Mittal S.'      ,
                                                'Falkenhainer B.']).

ctr_eval(size_max_starting_seq_alldifferent,
         [checker(size_max_starting_seq_alldifferent_c)      ,
	  reformulation(size_max_starting_seq_alldifferent_r)]).

ctr_sol(size_max_starting_seq_alldifferent,2,0,2,9,[1-3,2-6]).
ctr_sol(size_max_starting_seq_alldifferent,3,0,3,64,[1-16,2-24,3-24]).
ctr_sol(size_max_starting_seq_alldifferent,4,0,4,625,[1-125,2-200,3-180,4-120]).
ctr_sol(size_max_starting_seq_alldifferent,5,0,5,7776,[1-1296,2-2160,3-2160,4-1440,5-720]).
ctr_sol(size_max_starting_seq_alldifferent,6,0,6,117649,[1-16807,2-28812,3-30870,4-23520,5-12600,6-5040]).
ctr_sol(size_max_starting_seq_alldifferent,7,0,7,2097152,[1-262144,2-458752,3-516096,4-430080,5-268800,6-120960,7-40320]).
ctr_sol(size_max_starting_seq_alldifferent,8,0,8,43046721,[1-4782969,2-8503056,3-9920232,4-8817984,5-6123600,6-3265920,7-1270080,8-362880]).

% SIZE is the size of the maximal sequence (among all sequences of
% consecutive variables of the collection VARIABLES starting at
% position one) for which the alldifferent constraint holds.
size_max_starting_seq_alldifferent_c(SIZE, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    size_max_starting_seq_alldifferent0(VARS, [], SIZE).

size_max_starting_seq_alldifferent0([X|Xs], Set0, Size) :-
    fdset_add_element(Set0, X, Set),
    Set0\==Set, !,
    size_max_starting_seq_alldifferent0(Xs, Set, Size).
size_max_starting_seq_alldifferent0(_, Set, Size) :-
    fdset_size(Set, Size).

size_max_starting_seq_alldifferent_r(SIZE, VARIABLES) :-
    length(VARIABLES, N),
    check_type(dvar(0,N), SIZE),
    collection(VARIABLES, [dvar]),
    get_attr1(VARIABLES, VARS),
    size_max_starting_seq_alldifferent1(VARS, [], 1, SUMB),
    call(SIZE #= SUMB).

size_max_starting_seq_alldifferent1([], _, _, 0).
size_max_starting_seq_alldifferent1([VAR|RVARS], L, BPREV, B+SUM) :-
     size_max_starting_seq_alldifferent2(L, VAR, BPREV, CONJ),
     call(B #<=> CONJ),
     size_max_starting_seq_alldifferent1(RVARS, [VAR|L], B, SUM).

size_max_starting_seq_alldifferent2([], _, BPREV, BPREV).
size_max_starting_seq_alldifferent2([VAR2|RVARS], VAR1, BPREV, VAR1 #\= VAR2 #/\ R) :-
    size_max_starting_seq_alldifferent2(RVARS, VAR1, BPREV, R).
