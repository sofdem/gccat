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

ctr_date(global_cardinality_no_loop,['20051104','20060809']).

ctr_origin(global_cardinality_no_loop, 'Derived from %c and %c.', [global_cardinality,tree]).

ctr_synonyms(global_cardinality_no_loop, [gcc_no_loop]).

ctr_arguments(global_cardinality_no_loop,
              ['NLOOP'-dvar                                  ,
               'VARIABLES'-collection(var-dvar)              ,
               'VALUES'-collection(val-int, noccurrence-dvar)]).

ctr_exchangeable(global_cardinality_no_loop,
                 [items('VALUES',all)]).

ctr_restrictions(global_cardinality_no_loop,
                 ['NLOOP' >= 0                             ,
                  'NLOOP' =< size('VARIABLES')             ,
                  required('VARIABLES',var)                ,
                  size('VALUES') > 0                       ,
                  required('VALUES',[val,noccurrence])     ,
                  distinct('VALUES',val)                   ,
                  'VALUES'^noccurrence >= 0                ,
                  'VALUES'^noccurrence =< size('VARIABLES')]).

ctr_typical(global_cardinality_no_loop,
            [size('VARIABLES')           > 1             ,
             range('VARIABLES'^var)      > 1             ,
             size('VALUES')              > 1             ,
             size('VARIABLES')           > size('VALUES')]).

ctr_pure_functional_dependency(global_cardinality_no_loop, []).
ctr_functional_dependency(global_cardinality_no_loop, 1, [2]).
ctr_functional_dependency(global_cardinality_no_loop, 3-2, [2,3-1]).

ctr_graph(global_cardinality_no_loop,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var  =  'VALUES'^val,
           variables^key =\= 'VALUES'^val],
          ['NVERTEX' = 'VALUES'^noccurrence],
          []).

ctr_graph(global_cardinality_no_loop,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var=variables^key],
          ['NARC' = 'NLOOP'],
	  []).

ctr_example(global_cardinality_no_loop,
            global_cardinality_no_loop(1,[[var-1],[var-1],[var-8],[var-6]],
                                         [[val-1, noccurrence-1],
                                          [val-5, noccurrence-0],
                                          [val-6, noccurrence-1]])).

ctr_draw_example(global_cardinality_no_loop,
                 ['VARIABLES'],
                 [[[var-1],[var-1],[var-8],[var-6]]],
                 ['SELF'],
                 [2-2,4-4],
                 ['NVERTEX',
                  'FOREACH'('VALUES',[1-[2],5-[],6-[4]])],
                 '','1:NVERTEX=1, 5:NVERTEX=0, 6:NVERTEX=1',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(global_cardinality_no_loop,
 [link('root concept',   global_cardinality,                'assignment of a %e to its position is ignored',                 [variable]),
  link('specialisation', global_cardinality_low_up_no_loop, '%e replaced by %e %e',                                          [variable, fixed, interval]),
  link('related',        tree,                              'graph partitioning by a set of trees with degree restrictions', [])]).

ctr_key_words(global_cardinality_no_loop,['value constraint'          ,
                                          'flow'                      ,
                                          'functional dependency'     ,
		                          'pure functional dependency']).

ctr_eval(global_cardinality_no_loop, [reformulation(global_cardinality_no_loop_r)]).

global_cardinality_no_loop_r(NLOOP, VARIABLES, VALUES) :-
    check_type(dvar_gteq(0), NLOOP),
	collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    NLOOP #=< N,
	collection(VALUES, [int,dvar(0,N)]),
	length(VALUES, M),
    M > 0,
	get_attr1(VARIABLES, VARS        ),
	get_attr1(VALUES   , VALS        ),
	get_attr2(VALUES   , NOCCURRENCES),
    all_different(VALS),
    gcc_no_loop1(VARS, 1, SUMLOOP),
    call(SUMLOOP #= NLOOP),
    global_cardinality_no_loop1(1, M, N, VALS, NOCCURRENCES, VARS).

global_cardinality_no_loop1(I, M, _, [], [], _) :-
    I > M, !.
global_cardinality_no_loop1(I, M, N, [VAL|RVAL], [NOCCURRENCE|RNOCCURRENCE], VARS) :-
    I =< M,
    gcc_no_loop2(1, N, I, VARS, VAL, SUMI),
    call(SUMI #= NOCCURRENCE),
    I1 is I+1,
    global_cardinality_no_loop1(I1, M, N, RVAL, RNOCCURRENCE, VARS).
