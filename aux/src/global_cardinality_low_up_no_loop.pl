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

ctr_date(global_cardinality_low_up_no_loop,['20051218','20060809']).

ctr_origin(global_cardinality_low_up_no_loop, 'Derived from %c and %c.', [global_cardinality_low_up,tree]).

ctr_synonyms(global_cardinality_low_up_no_loop, [gcc_low_up_no_loop]).

ctr_arguments(global_cardinality_low_up_no_loop,
              ['MINLOOP'-int                                   ,
               'MAXLOOP'-int                                   ,
               'VARIABLES'-collection(var-dvar)                ,
               'VALUES'-collection(val-int, omin-int, omax-int)]).

ctr_exchangeable(global_cardinality_low_up_no_loop,
                 [items('VALUES',all),
                  vals(['VALUES'^omin],int(>=(0)),>,dontcare,dontcare),
                  vals(['VALUES'^omax],int(=<(size('VARIABLES'))),<,dontcare,dontcare)]).

ctr_restrictions(global_cardinality_low_up_no_loop,
                 ['MINLOOP' >= 0                    ,
                  'MINLOOP' =< 'MAXLOOP'            ,
                  'MAXLOOP' =< size('VARIABLES')    ,
                  required('VARIABLES',var)         ,
                  size('VALUES') > 0                ,
                  required('VALUES',[val,omin,omax]),
                  distinct('VALUES',val)            ,
                  'VALUES'^omin >= 0                ,
                  'VALUES'^omax =< size('VARIABLES'),
                  'VALUES'^omin =< 'VALUES'^omax    ]).

ctr_typical(global_cardinality_low_up_no_loop,
            [size('VARIABLES')       > 1                ,
             range('VARIABLES'^var)  > 1                ,
             size('VALUES')          > 1                ,
             'VALUES'^omin          =< size('VARIABLES'),
             'VALUES'^omax           > 0                ,
             'VALUES'^omax           < size('VARIABLES'),
             size('VARIABLES')       > size('VALUES')   ]).

ctr_graph(global_cardinality_low_up_no_loop,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var  =  'VALUES'^val,
           variables^key =\= 'VALUES'^val],
          ['NVERTEX' >= 'VALUES'^omin,
           'NVERTEX' =< 'VALUES'^omax],
          []).

ctr_graph(global_cardinality_low_up_no_loop,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var=variables^key],
          ['NARC' >= 'MINLOOP',
	   'NARC' =< 'MAXLOOP'],
	  []).

ctr_example(global_cardinality_low_up_no_loop,
            global_cardinality_low_up_no_loop(1,1,[[var-1],[var-1],[var-8],[var-6]],
                                                  [[val-1, omin-1, omax-1],
                                                   [val-5, omin-0, omax-0],
                                                   [val-6, omin-1, omax-2]])).

ctr_draw_example(global_cardinality_low_up_no_loop,
                 ['VARIABLES'],
                 [[[var-1],[var-1],[var-8],[var-6]]],
                 ['SELF'],
                 [2-2,4-4],
                 ['NVERTEX',
                  'FOREACH'('VALUES',[1-[2],5-[],6-[4]])],
                 '','1:NVERTEX=1, 5:NVERTEX=0, 6:NVERTEX=1',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(global_cardinality_low_up_no_loop,
 [link('implied by',       same_and_global_cardinality_low_up,  '',                                                              []),
  link('root concept',     global_cardinality_low_up,           'assignment of a %e to its position is ignored',                 [variable]),
  link('generalisation',   global_cardinality_no_loop,          '%e %e replaced by %e',                                          [fixed, interval, variable]),
  link('related',          tree,                                'graph partitioning by a set of trees with degree restrictions', [])]).

ctr_key_words(global_cardinality_low_up_no_loop,['value constraint',
                                                 'flow'            ]).

ctr_eval(global_cardinality_low_up_no_loop, [reformulation(global_cardinality_low_up_no_loop_r)]).

global_cardinality_low_up_no_loop_r(MINLOOP, MAXLOOP, VARIABLES, VALUES) :-
    check_type(int_gteq(0), MINLOOP),
    check_type(int_gteq(MINLOOP), MAXLOOP),
	collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
	collection(VALUES, [int,int(0,N),int(0,N)]),
	length(VALUES, M),
    M > 0,
	get_attr1(VARIABLES, VARS ),
	get_attr1(VALUES   , VALS ),
	get_attr2(VALUES   , OMINS),
	get_attr3(VALUES   , OMAXS),
    all_different(VALS),
    gcc_no_loop1(VARS, 1, SUMLOOP),
    call(SUMLOOP #>= MINLOOP),
    call(SUMLOOP #=< MAXLOOP),
    global_cardinality_low_up_no_loop1(1, M, N, VALS, OMINS, OMAXS, VARS).

global_cardinality_low_up_no_loop1(I, M, _, [], [], [], _) :-
    I > M, !.
global_cardinality_low_up_no_loop1(I, M, N, [VAL|RVAL], [OMIN|ROMIN], [OMAX|ROMAX], VARS) :-
    I =< M,
    gcc_no_loop2(1, N, I, VARS, VAL, SUMI),
    call(SUMI #>= OMIN),
    call(SUMI #=< OMAX),
    I1 is I+1,
    global_cardinality_low_up_no_loop1(I1, M, N, RVAL, ROMIN, ROMAX, VARS).
