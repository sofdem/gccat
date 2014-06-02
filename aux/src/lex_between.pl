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

ctr_date(lex_between,['20030820','20040530','20060811']).

ctr_origin(lex_between, '\\cite{BeldiceanuCarlsson02c}', []).

ctr_arguments(lex_between,
              ['LOWER_BOUND'-collection(var-int),
               'VECTOR'-collection(var-dvar)    ,
               'UPPER_BOUND'-collection(var-int)]).

ctr_exchangeable(lex_between,
                 [vals(['LOWER_BOUND'^var],int,>,dontcare,dontcare),
                  vals(['UPPER_BOUND'^var],int,<,dontcare,dontcare)]).

ctr_synonyms(lex_between,[between]).

ctr_restrictions(lex_between,
                 [required('LOWER_BOUND',var)            ,
                  required('VECTOR'     ,var)            ,
                  required('UPPER_BOUND',var)            ,
                  size('LOWER_BOUND') = size('VECTOR')   ,
                  size('UPPER_BOUND') = size('VECTOR')   ,
                  lex_lesseq('LOWER_BOUND','VECTOR'     ),
                  lex_lesseq('VECTOR'     ,'UPPER_BOUND')]).

ctr_typical(lex_between,
            [size('LOWER_BOUND') > 1                ,
             lex_lesseq('LOWER_BOUND','UPPER_BOUND')]).

ctr_contractible(lex_between, [], ['LOWER_BOUND','VECTOR','UPPER_BOUND'], suffix).

ctr_example(lex_between,
            lex_between([[var-5], [var-2], [var-3], [var-9]],
                        [[var-5], [var-2], [var-6], [var-2]],
                        [[var-5], [var-2], [var-6], [var-3]])).

ctr_see_also(lex_between,
 [link('part of system of constraints', lex_lesseq,          '',   []),
  link('common keyword',                lex_less,            '%k', ['lexicographic order']),
  link('common keyword',                lex_greater,         '%k', ['lexicographic order']),
  link('common keyword',                lex_greatereq,       '%k', ['lexicographic order']),
  link('common keyword',                lex_chain_greater,   '%k', ['lexicographic order']),
  link('common keyword',                lex_chain_greatereq, '%k', ['lexicographic order']),
  link('common keyword',                lex_chain_less,      '%k', ['lexicographic order']),
  link('common keyword',                lex_chain_lesseq,    '%k', ['lexicographic order'])]).

ctr_key_words(lex_between,['order constraint'                ,
                           'system of constraints'           ,
                           'vector'                          ,
                           'symmetry'                        ,
                           'lexicographic order'             ,
                           'Berge-acyclic constraint network',
                           'automaton'                       ,
                           'automaton without counters'      ,
                           'reified automaton constraint'    ,
                           'arc-consistency'                 ]).

ctr_persons(lex_between,['Beldiceanu N.',
                         'Carlsson M.'  ]).

ctr_eval(lex_between, [reformulation(lex_between_r),
                       automaton(lex_between_a)]).

lex_between_r(LOWER_BOUND, VECTOR, UPPER_BOUND) :-
    collection(LOWER_BOUND, [int]),
    collection(VECTOR, [dvar]),
    collection(UPPER_BOUND, [int]),
    length(LOWER_BOUND, LB),
    length(VECTOR, LV),
    length(UPPER_BOUND, LU),
    LB = LV,
    LU = LV,
    eval(lex_lesseq(LOWER_BOUND, VECTOR)),
    eval(lex_lesseq(VECTOR, UPPER_BOUND)).

% 0: A#<X #/\ X#<B
% 1: A#<X #/\ X#=B
% 2: A#<X #/\ X#>B
% 3: A#=X #/\ X#<B
% 4: A#=X #/\ X#=B
% 5: A#=X #/\ X#>B
% 6: A#>X #/\ X#<B
% 7: A#>X #/\ X#=B
% 8: other
lex_between_a(FLAG, LOWER_BOUND, VECTOR, UPPER_BOUND) :-
    collection(LOWER_BOUND, [int]),
    collection(VECTOR, [dvar]),
    collection(UPPER_BOUND, [int]),
    length(LOWER_BOUND, LB),
    length(VECTOR, LV),
    length(UPPER_BOUND, LU),
    LB = LV,
    LU = LV,
    lex_between_signature(LOWER_BOUND, VECTOR, UPPER_BOUND, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(a),sink(b),sink(s),sink(t)],
                          [arc(s,4,s),
                           arc(s,0,t),
                           arc(s,3,a),
                           arc(s,1,b),
                           arc(a,3,a),
                           arc(a,4,a),
                           arc(a,5,a),
                           arc(a,0,t),
                           arc(a,1,t),
                           arc(a,2,t),
                           arc(b,1,b),
                           arc(b,4,b),
                           arc(b,7,b),
                           arc(b,0,t),
                           arc(b,3,t),
                           arc(b,6,t),
                           arc(t,0,t),
                           arc(t,1,t),
                           arc(t,2,t),
                           arc(t,3,t),
                           arc(t,4,t),
                           arc(t,5,t),
                           arc(t,6,t),
                           arc(t,7,t),
                           arc(t,8,t)],
                          [], [], []),
    automaton_bool(FLAG, [0,1,2,3,4,5,6,7,8], AUTOMATON).

lex_between_signature([], [], [], []).
lex_between_signature([[var-A1]|As], [[var-X1]|Xs], [[var-B1]|Bs], [L1|Ls]) :-
        Adown is A1-1,
        Aup   is A1+1,
        Bdown is B1-1,
        Bup   is B1+1,
        (   A1+1<B1
        ->  case(X-L,[X1-L1],
                 [node(-1,X,[(inf..Adown)-6,
                             (A1..A1)-3,
                             (Aup..Bdown)-0,
                             (B1..B1)-1,
                             (Bup..sup)-2]),
                  node(0,L,[0..0]),
                  node(1,L,[1..1]),
                  node(2,L,[2..2]),
                  node(3,L,[3..3]),
                  node(6,L,[6..6])])
        ;   A1<B1
        ->  case(X-L,[X1-L1],
                 [node(-1,X,[(inf..Adown)-6,
                             (A1..A1)-3,
                             (B1..B1)-1,
                             (Bup..sup)-2]),
                  node(1,L,[1..1]),
                  node(2,L,[2..2]),
                  node(3,L,[3..3]),
                  node(6,L,[6..6])])
        ;   A1=:=B1
        ->  case(X-L,[X1-L1],
                 [node(-1,X,[(inf..Adown)-6,
                             (A1..A1)-4,
                             (Aup..sup)-2]),
                  node(2,L,[2..2]),
                  node(4,L,[4..4]),
                  node(6,L,[6..6])])
        ;   A1=:=B1+1
        ->  case(X-L,[X1-L1],
                 [node(-1,X,[(inf..Bdown)-6,
                             (B1..B1)-7,
                             (A1..A1)-5,
                             (Aup..sup)-2]),
                  node(2,L,[2..2]),
                  node(5,L,[5..5]),
                  node(6,L,[6..6]),
                  node(7,L,[7..7])])
        ;   A1>B1
        ->  case(X-L,[X1-L1],
                 [node(-1,X,[(inf..Bdown)-6,
                             (B1..B1)-7,
                             (Bup..Adown)-8,
                             (A1..A1)-5,
                             (Aup..sup)-2]),
                  node(2,L,[2..2]),
                  node(5,L,[5..5]),
                  node(6,L,[6..6]),
                  node(7,L,[7..7]),
                  node(8,L,[8..8])])
        ),
        lex_between_signature(As, Xs, Bs, Ls).
