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

ctr_date(distance_between,['20000128','20030820','20060808','20090428']).

ctr_origin(distance_between, 'N.~Beldiceanu', []).

ctr_arguments(distance_between,
              ['DIST'-dvar                      ,
               'VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar),
               'CTR'-atom                       ]).

ctr_exchangeable(distance_between,
                 [args([['DIST'],['VARIABLES1','VARIABLES2'],['CTR']]),
                  items_sync('VARIABLES1','VARIABLES2',all),
                  translate(['VARIABLES1'^var]),
                  translate(['VARIABLES2'^var])]).

ctr_synonyms(distance_between,[distance]).

ctr_restrictions(distance_between,
                 ['DIST' >= 0                                                       ,
                  'DIST' =< size('VARIABLES1')*size('VARIABLES2')-size('VARIABLES1'),
                  required('VARIABLES1',var)                                        ,
                  required('VARIABLES2',var)                                        ,
                  size('VARIABLES1') = size('VARIABLES2')                           ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])                                  ]).

ctr_typical(distance_between,
            ['DIST' > 0                                                       ,
             'DIST' < size('VARIABLES1')*size('VARIABLES2')-size('VARIABLES1'),
             size('VARIABLES1') > 1                                           ,
             in_list('CTR',[=,=\=])                                           ]).

ctr_pure_functional_dependency(distance_between, []).
ctr_functional_dependency(distance_between, 1, [2,3,4]).

ctr_graph(distance_between,
          [['VARIABLES1'],['VARIABLES2']],
          2,
          ['CLIQUE'(=\=)>>collection(variables1,variables2)],
          ['CTR'(variables1^var,variables2^var)],
          ['DISTANCE' = 'DIST'],
          []).

ctr_example(distance_between,
            distance_between(2,
                             [[var-3],[var-4],[var-6],[var-2],[var-4]],
                             [[var-2],[var-6],[var-9],[var-3],[var-6]],
                             <)).

ctr_draw_example(distance_between,
                 [['VARIABLES1'],['VARIABLES2']],
                 [[[[var-3],[var-4],[var-6],[var-2],[var-4]]],
                  [[[var-2],[var-6],[var-9],[var-3],[var-6]]]],
                 ['CLIQUE'(=\=)],
                 [[1-[2,3,5],2-3,4-[1,2,3,5],5-3],
                  [1-[2,3,4,5],2-3,4-[2,3,5],5-3]],
                 [['DISTANCE'([4-1])],
                  ['DISTANCE'([1-4])]],
                 '','',
                 [1.9,1.9,1.9,1.9]).

ctr_see_also(distance_between,
 [link('common keyword', distance_change, '%k', ['proximity constraint'])]).

ctr_key_words(distance_between,['proximity constraint'      ,
                                'functional dependency'     ,
		                'pure functional dependency']).

ctr_persons(distance_between,['Beldiceanu N.']).

ctr_eval(distance_between, [reformulation(distance_between_r)]).

distance_between_r(DIST, VARIABLES1, VARIABLES2, CTR) :-
    collection(VARIABLES1, [dvar]),
    collection(VARIABLES2, [dvar]),
    length(VARIABLES1, L1),
    length(VARIABLES2, L2),
    L1 = L2,
    L12 is L1*L2-L1,
    check_type(dvar(0,L12),DIST),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    get_attr1(VARIABLES1, VARS1),
    get_attr1(VARIABLES2, VARS2),
    distance_between1(VARS1, VARS2, 1, VARS1, VARS2, CTR, TERM),
    call(DIST #= TERM).

distance_between1([], [], _, _, _, _, 0).
distance_between1([VAR1|RVARS1], [VAR2|RVARS2], IVAR, VARS1, VARS2, CTR, TERM+R) :-
    distance_between2(VARS1, VARS2, VAR1, VAR2, IVAR, 1, CTR, TERM),
    IVAR1 is IVAR+1,
    distance_between1(RVARS1, RVARS2, IVAR1, VARS1, VARS2, CTR, R).

distance_between2([], [], _, _, _, _, _, 0).
distance_between2([UAR1|RUARS1], [UAR2|RUARS2], VAR1, VAR2, IVAR, IUAR, =, B12+S) :- !,
    (IVAR =\= IUAR -> B12 #<=> (VAR1 #= UAR1 #/\ VAR2 #\= UAR2) #\/ (VAR1 #\= UAR1 #/\ VAR2 #= UAR2) ; B12 = 0),
    IUAR1 is IUAR+1,
    distance_between2(RUARS1, RUARS2, VAR1, VAR2, IVAR, IUAR1, =, S).
distance_between2([UAR1|RUARS1], [UAR2|RUARS2], VAR1, VAR2, IVAR, IUAR, =\=, B12+S) :- !,
    (IVAR =\= IUAR -> B12 #<=> (VAR1 #\= UAR1 #/\ VAR2 #= UAR2) #\/ (VAR1 #= UAR1 #/\ VAR2 #\= UAR2) ; B12 = 0),
    IUAR1 is IUAR+1,
    distance_between2(RUARS1, RUARS2, VAR1, VAR2, IVAR, IUAR1, =\=, S).
distance_between2([UAR1|RUARS1], [UAR2|RUARS2], VAR1, VAR2, IVAR, IUAR, <, B12+S) :- !,
    (IVAR =\= IUAR -> B12 #<=> (VAR1 #< UAR1 #/\ VAR2 #>= UAR2) #\/ (VAR1 #>= UAR1 #/\ VAR2 #< UAR2) ; B12 = 0),
    IUAR1 is IUAR+1,
    distance_between2(RUARS1, RUARS2, VAR1, VAR2, IVAR, IUAR1, <, S).
distance_between2([UAR1|RUARS1], [UAR2|RUARS2], VAR1, VAR2, IVAR, IUAR, >=, B12+S) :- !,
    (IVAR =\= IUAR -> B12 #<=> (VAR1 #>= UAR1 #/\ VAR2 #< UAR2) #\/ (VAR1 #< UAR1 #/\ VAR2 #>= UAR2) ; B12 = 0),
    IUAR1 is IUAR+1,
    distance_between2(RUARS1, RUARS2, VAR1, VAR2, IVAR, IUAR1, >=, S).
distance_between2([UAR1|RUARS1], [UAR2|RUARS2], VAR1, VAR2, IVAR, IUAR, >, B12+S) :- !,
    (IVAR =\= IUAR -> B12 #<=> (VAR1 #> UAR1 #/\ VAR2 #=< UAR2) #\/ (VAR1 #=< UAR1 #/\ VAR2 #> UAR2) ; B12 = 0),
    IUAR1 is IUAR+1,
    distance_between2(RUARS1, RUARS2, VAR1, VAR2, IVAR, IUAR1, >, S).
distance_between2([UAR1|RUARS1], [UAR2|RUARS2], VAR1, VAR2, IVAR, IUAR, =<, B12+S) :-
    (IVAR =\= IUAR -> B12 #<=> (VAR1 #=< UAR1 #/\ VAR2 #> UAR2) #\/ (VAR1 #> UAR1 #/\ VAR2 #=< UAR2) ; B12 = 0),
    IUAR1 is IUAR+1,
    distance_between2(RUARS1, RUARS2, VAR1, VAR2, IVAR, IUAR1, =<, S).
