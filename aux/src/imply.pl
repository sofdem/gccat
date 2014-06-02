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

ctr_date(imply,['20051226','20091016']).

ctr_origin(imply, 'Logic', []).

ctr_arguments(imply,
              ['VAR'-dvar                      ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(imply,
                 [vals(['VAR','VARIABLES'^var],int(in(0,1)),<,all,dontcare)]).

ctr_synonyms(imply,[rel,ifthen]).

ctr_restrictions(imply,
                 ['VAR' >= 0               ,
                  'VAR' =< 1               ,
                  size('VARIABLES') = 2    ,
                  required('VARIABLES',var),
                  'VARIABLES'^var >= 0     ,
                  'VARIABLES'^var =< 1     ]).

ctr_pure_functional_dependency(imply, []).
ctr_functional_dependency(imply, 1, [2]).

ctr_example(imply,
            [imply(1,[[var-0],[var-0]]),
             imply(1,[[var-0],[var-1]]),
             imply(0,[[var-1],[var-0]]),
             imply(1,[[var-1],[var-1]])]).

ctr_see_also(imply,
 [link('implies',        atleast_nvalue,        '',   []),
  link('implies',        soft_alldifferent_ctr, '',   []),
  link('common keyword', and,                   '%k', ['Boolean constraint']),
  link('common keyword', equivalent,            '%k', ['Boolean constraint']),
  link('common keyword', nand,                  '%k', ['Boolean constraint']),
  link('common keyword', nor,                   '%k', ['Boolean constraint']),
  link('common keyword', or,                    '%k', ['Boolean constraint']),
  link('common keyword', xor,                   '%k', ['Boolean constraint'])]).

ctr_key_words(imply,['Boolean constraint'              ,
                     'Berge-acyclic constraint network',
                     'automaton'                       ,
                     'automaton without counters'      ,
                     'reified automaton constraint'    ,
                     'functional dependency'           ,
		     'pure functional dependency'      ,
                     'arc-consistency'                 ]).

ctr_eval(imply, [reformulation(imply_r), automaton(imply_a)]).

ctr_sol(imply,2,0,2,4,[0-1,1-3]).
ctr_sol(imply,3,0,3,0,[]).
ctr_sol(imply,4,0,4,0,[]).
ctr_sol(imply,5,0,5,0,[]).
ctr_sol(imply,6,0,6,0,[]).
ctr_sol(imply,7,0,7,0,[]).
ctr_sol(imply,8,0,8,0,[]).

imply_r(VAR, VARIABLES) :-
    check_type(dvar(0,1), VAR),
    collection(VARIABLES, [dvar(0,1)]),
    length(VARIABLES, 2),
    get_attr1(VARIABLES, VARS),
    VARS = [VAR1, VAR2],
    VAR #<=> (VAR1 #=> VAR2).

% 0: VAR=0
% 1: VAR=1
imply_a(FLAG, VAR, VARIABLES) :-
    check_type(dvar(0,1), VAR),
    collection(VARIABLES, [dvar(0,1)]),
    length(VARIABLES, 2),
    get_attr1(VARIABLES, LIST),
    append([VAR], LIST, LIST_VARIABLES),
    AUTOMATON = automaton(LIST_VARIABLES, _,
                          LIST_VARIABLES, 
                          [source(s),sink(t)],
                          [arc(s,0,i),
                           arc(s,1,j),
                           arc(i,1,k),
                           arc(j,0,t),
                           arc(j,1,l),
                           arc(k,0,t),
                           arc(l,1,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [], [], []),
    automaton_bool(FLAG, [0,1], AUTOMATON).
