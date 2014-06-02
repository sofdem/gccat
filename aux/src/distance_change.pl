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

:-dynamic distance_change_a/4.

ctr_date(distance_change,['20000128','20030820','20040530','20060808']).

ctr_origin(distance_change, 'Derived from %c.', [change]).

ctr_arguments(distance_change,
              ['DIST'-dvar                      ,
               'VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'CTR'-atom                       ]).

ctr_exchangeable(distance_change,
                 [args([['DIST'],['VARIABLES1','VARIABLES2'],['CTR']]),
                  translate(['VARIABLES1'^var]),
                  translate(['VARIABLES2'^var])]).

ctr_synonyms(distance_change,[distance]).

ctr_restrictions(distance_change,
                 ['DIST' >= 0                            ,
                  'DIST' < size('VARIABLES1')            ,
                  required('VARIABLES1',var)             ,
                  required('VARIABLES2',var)             ,
                  size('VARIABLES1') = size('VARIABLES2'),
                  in_list('CTR',[=,=\=,<,>=,>,=<])       ]).

ctr_typical(distance_change,
            ['DIST'             > 0,
             size('VARIABLES1') > 1,
             in_list('CTR',[=,=\=])]).

ctr_pure_functional_dependency(distance_change, []).
ctr_functional_dependency(distance_change, 1, [2,3,4]).

ctr_graph(distance_change,
          [['VARIABLES1'],['VARIABLES2']],
          2,
          ['PATH'>>collection(variables1,variables2)],
          ['CTR'(variables1^var,variables2^var)],
          ['DISTANCE' = 'DIST'],
          []).

ctr_example(distance_change,
            distance_change(1,
                            [[var-3],[var-3],[var-1],[var-2],[var-2]],
                            [[var-4],[var-4],[var-3],[var-3],[var-3]],
                            =\=)).

ctr_draw_example(distance_change,
                 [['VARIABLES1'],['VARIABLES2']],
                 [[[[var-3],[var-3],[var-1],[var-2],[var-2]]],
                  [[[var-4],[var-4],[var-3],[var-3],[var-3]]]],
                 ['PATH'],
                 [[2-3,3-4],
                  [2-3]],
                 [['DISTANCE'([3-4])],
                  ['DISTANCE'([])]],
                 '','',
                 [1.8,1.8,1,1]).

ctr_see_also(distance_change,
 [link('common keyword', distance_between, '%k', ['proximity constraint']),
  link('root concept',   change,           '',   [])]).

ctr_key_words(distance_change,['proximity constraint'                   ,
                               'automaton'                              ,
                               'automaton with counters'                ,
                               'sliding cyclic(2) constraint network(2)',
                               'functional dependency'                  ,
		               'pure functional dependency'             ]).

ctr_eval(distance_change, [reformulation(distance_change_r),
                           automaton(distance_change_a)]).

distance_change_r(DIST, VARIABLES1, VARIABLES2, CTR) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    length(VARIABLES1, L1),
    length(VARIABLES2, L2),
    L1 = L2,
    L is L1-1,
    check_type(dvar(0,L),DIST),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    distance_change1(VARS1, VARS2, CTR, TERM),
    call(DIST #= TERM).

distance_change1([], [], _, 0).
distance_change1([_], [_], _, 0) :- !.
distance_change1([UAR1,UAR2|R], [VAR1,VAR2|S], =, B12+T) :- !,
    B12 #<=> (UAR1 #= UAR2 #/\ VAR1 #\= VAR2) #\/ (UAR1 #\= UAR2 #/\ VAR1 #= VAR2),
    distance_change1([UAR2|R], [VAR2|S], =, T).
distance_change1([UAR1,UAR2|R], [VAR1,VAR2|S], =\=, B12+T) :- !,
    B12 #<=> (UAR1 #\= UAR2 #/\ VAR1 #= VAR2) #\/ (UAR1 #= UAR2 #/\ VAR1 #\= VAR2),
    distance_change1([UAR2|R], [VAR2|S],  =\=, T).
distance_change1([UAR1,UAR2|R], [VAR1,VAR2|S], <, B12+T) :- !,
    B12 #<=> (UAR1 #< UAR2 #/\ VAR1 #>= VAR2) #\/ (UAR1 #>= UAR2 #/\ VAR1 #< VAR2),
    distance_change1([UAR2|R], [VAR2|S], <, T).
distance_change1([UAR1,UAR2|R], [VAR1,VAR2|S], >=, B12+T) :- !,
    B12 #<=> (UAR1 #>= UAR2 #/\ VAR1 #< VAR2) #\/ (UAR1 #< UAR2 #/\ VAR1 #>= VAR2),
    distance_change1([UAR2|R], [VAR2|S], >=, T).
distance_change1([UAR1,UAR2|R], [VAR1,VAR2|S], >, B12+T) :- !,
    B12 #<=> (UAR1 #> UAR2 #/\ VAR1 #=< VAR2) #\/ (UAR1 #=< UAR2 #/\ VAR1 #> VAR2),
    distance_change1([UAR2|R], [VAR2|S], >, T).
distance_change1([UAR1,UAR2|R], [VAR1,VAR2|S], =<, B12+T) :-
    B12 #<=> (UAR1 #=< UAR2 #/\ VAR1 #> VAR2) #\/ (UAR1 #> UAR2 #/\ VAR1 #=< VAR2),
    distance_change1([UAR2|R], [VAR2|S], =<, T).

distance_change_a(FLAG, DIST, VARIABLES1, VARIABLES2, CTR) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    length(VARIABLES1, L1),
    length(VARIABLES2, L2),
    L1 = L2,
    L is L1-1,
    check_type(dvar(0,L),DIST),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    distance_change_signature(VARIABLES1, VARIABLES2, SIGNATURE, CTR),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= DIST #<=> FLAG.

distance_change_signature([], [], [], _).
distance_change_signature([_], [_], [], _) :- !.

distance_change_signature([[var-VAR1i],[var-VAR1j]|VAR1s],
                          [[var-VAR2i],[var-VAR2j]|VAR2s], [S|Ss], =  ) :- !,
    ((VAR1i  #=  VAR1j) #/\ (VAR2i #\= VAR2j)) #\/
    ((VAR1i #\= VAR1j) #/\ (VAR2i  #=  VAR2j)) #<=> S,
    distance_change_signature([[var-VAR1j]|VAR1s], [[var-VAR2j]|VAR2s], Ss, =).

distance_change_signature([[var-VAR1i],[var-VAR1j]|VAR1s],
                          [[var-VAR2i],[var-VAR2j]|VAR2s], [S|Ss], =\=) :- !,
    ((VAR1i #\= VAR1j) #/\ (VAR2i  #=  VAR2j)) #\/
    ((VAR1i  #=  VAR1j) #/\ (VAR2i #\= VAR2j)) #<=> S,
    distance_change_signature([[var-VAR1j]|VAR1s], [[var-VAR2j]|VAR2s], Ss, =\=).

distance_change_signature([[var-VAR1i],[var-VAR1j]|VAR1s],
                          [[var-VAR2i],[var-VAR2j]|VAR2s], [S|Ss], <  ) :- !,
    ((VAR1i #<  VAR1j) #/\ (VAR2i #>= VAR2j)) #\/
    ((VAR1i #>= VAR1j) #/\ (VAR2i #<  VAR2j)) #<=> S,
    distance_change_signature([[var-VAR1j]|VAR1s], [[var-VAR2j]|VAR2s], Ss, <).

distance_change_signature([[var-VAR1i],[var-VAR1j]|VAR1s],
                          [[var-VAR2i],[var-VAR2j]|VAR2s], [S|Ss], >= ) :- !,
    ((VAR1i #>= VAR1j) #/\ (VAR2i #<  VAR2j)) #\/
    ((VAR1i #<  VAR1j) #/\ (VAR2i #>= VAR2j)) #<=> S,
    distance_change_signature([[var-VAR1j]|VAR1s], [[var-VAR2j]|VAR2s], Ss, >=).

distance_change_signature([[var-VAR1i],[var-VAR1j]|VAR1s],
                          [[var-VAR2i],[var-VAR2j]|VAR2s], [S|Ss], >  ) :- !,
    ((VAR1i #>  VAR1j) #/\ (VAR2i #=< VAR2j)) #\/
    ((VAR1i #=< VAR1j) #/\ (VAR2i #>  VAR2j)) #<=> S,
    distance_change_signature([[var-VAR1j]|VAR1s], [[var-VAR2j]|VAR2s], Ss, >).

distance_change_signature([[var-VAR1i],[var-VAR1j]|VAR1s],
                          [[var-VAR2i],[var-VAR2j]|VAR2s], [S|Ss], =< ) :- !,
    ((VAR1i #=< VAR1j) #/\ (VAR2i #>  VAR2j)) #\/
    ((VAR1i #>  VAR1j) #/\ (VAR2i #=< VAR2j)) #<=> S,
    distance_change_signature([[var-VAR1j]|VAR1s], [[var-VAR2j]|VAR2s], Ss, =<).
