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

ctr_date(lex_greatereq,['20030820','20040530','20060811']).

ctr_origin(lex_greatereq, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(lex_greatereq,
              ['VECTOR1'-collection(var-dvar),
               'VECTOR2'-collection(var-dvar)]).

ctr_exchangeable(lex_greatereq,
                 [vals(['VECTOR1'^var],int,<,dontcare,dontcare),
                  vals(['VECTOR2'^var],int,>,dontcare,dontcare)]).

ctr_synonyms(lex_greatereq, [lexeq, lex_chain, rel, greatereq, geq, lex_geq]).

ctr_restrictions(lex_greatereq,
                 [required('VECTOR1',var)          ,
                  required('VECTOR2',var)          ,
                  size('VECTOR1') = size('VECTOR2')]).

ctr_typical(lex_greatereq,
            [size('VECTOR1') > 1                                                                                           ,
	     size('VECTOR1') < 5 #\/ nval(['VECTOR1'^var,'VECTOR2'^var]) < 2*size('VECTOR1')                               ,
	     maxval(['VECTOR1'^var,'VECTOR2'^var]) =< 1 #\/ 2*size('VECTOR1')-max_nvalue(['VECTOR1'^var,'VECTOR2'^var]) > 2]).

ctr_contractible(lex_greatereq, [], ['VECTOR1','VECTOR2'], suffix).

ctr_derived_collections(lex_greatereq,
    [col('DESTINATION'-collection(index-int, x-int, y-int),
         [item(index-0, x-0, y-0)]),
     col('COMPONENTS'-collection(index-int, x-dvar, y-dvar),
         [item(index-'VECTOR1'^key, x-'VECTOR1'^var, y-'VECTOR2'^var)])]).

ctr_graph(lex_greatereq,
          ['COMPONENTS','DESTINATION'],
          2,
          ['PRODUCT'('PATH','VOID')>>collection(item1,item2)],
          [(                                  item2^index>0 #/\ item1^x =  item1^y) #\/
           (item1^index < size('VECTOR1') #/\ item2^index=0 #/\ item1^x >  item1^y) #\/
           (item1^index = size('VECTOR1') #/\ item2^index=0 #/\ item1^x >= item1^y)     ],
          ['PATH_FROM_TO'(index,1,0) = 1],
          []).

ctr_example(lex_greatereq,
            [lex_greatereq([[var-5], [var-2], [var-8], [var-9]],
                           [[var-5], [var-2], [var-6], [var-2]]),
             lex_greatereq([[var-5], [var-2], [var-3], [var-9]],
                           [[var-5], [var-2], [var-3], [var-9]])]).

ctr_draw_example(lex_greatereq,
                 ['COMPONENTS','DESTINATION'],
                 [[[index-1, x-5, y-5],
                   [index-2, x-2, y-2],
                   [index-3, x-8, y-6],
                   [index-4, x-9, y-2]],
                  [[index-0, x-0, y-0]]],
                 ['PRODUCT'('PATH','VOID')],
                 [1-2,2-3,3-5,4-5],
                 ['PATH_FROM_TO'([1],[5],[1-2,2-3,3-5]),
                  'COLLECTIONS'(['COMPONENTS'-[1,2,3,4],'DESTINATION'-[5]])],
                 '','PATH_FROM_TO(index,1,0)=1',
                 [2.3,2.5,2.3,2.5]).

ctr_see_also(lex_greatereq,
 [link('negation',                    lex_less,            '',   []),
  link('implied by',                  sort,                '',   []),
  link('implied by',                  lex_equal,           '',   []),
  link('implied by',                  lex_greater,         '',   []),
  link('implies (if swap arguments)', lex_lesseq,          '',   []),
  link('system of constraints',       lex_chain_greatereq, '',   []),
  link('uses in its reformulation',   lex_chain_greatereq, '',   []),
  link('common keyword',              lex_between,         '%k', ['lexicographic order']),
  link('common keyword',              lex_chain_greater,   '%k', ['lexicographic order']),
  link('common keyword',              lex_chain_less,      '%k', ['lexicographic order']),
  link('common keyword',              lex_chain_lesseq,    '%k', ['lexicographic order']),
  link('common keyword',              cond_lex_greatereq,  '%k', ['lexicographic order']),
  link('common keyword',              lex_different,       '%k', ['vector'])]).

ctr_key_words(lex_greatereq,['order constraint'                       ,
                             'vector'                                 ,
                             'symmetry'                               ,
                             'matrix symmetry'                        ,
                             'lexicographic order'                    ,
                             'multiset ordering'                      ,
                             'duplicated variables'                   ,
                             'Berge-acyclic constraint network'       ,
                             'automaton'                              ,
                             'automaton without counters'             ,
                             'reified automaton constraint'           ,
                             'derived collection'                     ,
                             'arc-consistency'                        ,
                             'heuristics and lexicographical ordering']).

ctr_persons(lex_greatereq,['Frisch A. M.'       ,
                           'Hnich B.'           ,
                           'K{\\i}z{\\i}ltan Z.',
                           'Miguel I.'          ,
                           'Walsh T.'           ,
                           'Beldiceanu N.'      ,
                           'Carlsson M.'        ,
                           'Harvey W.'          ,
                           'Fr{\\"u}hwirth T.'  ]).

ctr_eval(lex_greatereq, [checker(lex_greatereq_c)  ,
			 builtin(lex_greatereq_b)  ,
                         automaton(lex_greatereq_a)]).

lex_greatereq_c(VECTOR1, VECTOR2) :-
    collection(VECTOR1, [int]),
    collection(VECTOR2, [int]),
    length(VECTOR1, L),
    length(VECTOR2, L),
    get_attr1(VECTOR1, VECT1),
    get_attr1(VECTOR2, VECT2),
    lex_greatereq_c1(VECT1, VECT2).

lex_greatereq_c1([], []) :- !.
lex_greatereq_c1([V|R], [V|S]) :- !,
    lex_greatereq_c1(R, S).
lex_greatereq_c1([V1|_], [V2|_]) :-
    V1 > V2.

lex_greatereq_b(VECTOR1, VECTOR2) :-
    collection(VECTOR1, [dvar]),
    collection(VECTOR2, [dvar]),
    length(VECTOR1, L),
    length(VECTOR2, L),
    get_attr1(VECTOR1, VECT1),
    get_attr1(VECTOR2, VECT2),
    lex_chain([VECT2,VECT1], [op(#=<)]).

% 1: VAR1 < VAR2
% 2: VAR1 = VAR2
% 3: VAR1 > VAR2
lex_greatereq_a(FLAG, VECTOR1, VECTOR2) :-
    collection(VECTOR1, [dvar]),
    collection(VECTOR2, [dvar]),
    length(VECTOR1, L),
    length(VECTOR2, L),
    lex_greatereq_signature(VECTOR1, VECTOR2, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE,
                          [source(s),sink(s),sink(t)],
                          [arc(s,2,s),
                           arc(s,3,t),
                           arc(t,1,t),
                           arc(t,2,t),
                           arc(t,3,t)],
                          [], [], []),
    automaton_bool(FLAG, [1,2,3], AUTOMATON).

lex_greatereq_signature([], [], []).
lex_greatereq_signature([[var-VAR1]|Xs], [[var-VAR2]|Ys], [S|Ss]) :-
    S in 1..3,
    VAR1 #< VAR2 #<=> S #= 1,
    VAR1 #= VAR2 #<=> S #= 2,
    VAR1 #> VAR2 #<=> S #= 3,
    lex_greatereq_signature(Xs, Ys, Ss).
