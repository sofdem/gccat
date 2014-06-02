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

ctr_date(global_cardinality_low_up,['20031008','20040530','20060809','20090521']).

ctr_origin(global_cardinality_low_up, 'Used for defining %c.', [sliding_distribution]).

ctr_synonyms(global_cardinality_low_up, [gcc_low_up, gcc]).

ctr_arguments(global_cardinality_low_up,
              ['VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int, omin-int, omax-int)]).

ctr_exchangeable(global_cardinality_low_up,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],all(notin('VALUES'^val)),=,dontcare,dontcare),
                  items('VALUES',all),
                  vals(['VALUES'^omin],int(>=(0)),>,dontcare,dontcare),
                  vals(['VALUES'^omax],int(=<(size('VARIABLES'))),<,dontcare,dontcare),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(global_cardinality_low_up,
                 [required('VARIABLES',var)         ,
                  size('VALUES') > 0                ,
                  required('VALUES',[val,omin,omax]),
                  distinct('VALUES',val)            ,
                  'VALUES'^omin >= 0                ,
                  'VALUES'^omax =< size('VARIABLES'),
                  'VALUES'^omin =< 'VALUES'^omax    ]).

ctr_typical(global_cardinality_low_up,
            [size('VARIABLES')       > 1                ,
             range('VARIABLES'^var)  > 1                ,
             size('VALUES')          > 1                ,
             'VALUES'^omin          =< size('VARIABLES'),
             'VALUES'^omax           > 0                ,
             'VALUES'^omax           < size('VARIABLES'),
             size('VARIABLES')       > size('VALUES')   ,
             in_attr('VARIABLES',var,'VALUES',val)      ]).

ctr_contractible(global_cardinality_low_up, [], 'VALUES', any).

ctr_graph(global_cardinality_low_up,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var = 'VALUES'^val],
          ['NVERTEX' >= 'VALUES'^omin,
           'NVERTEX' =< 'VALUES'^omax],
          []).

ctr_example(global_cardinality_low_up,
            global_cardinality_low_up([[var-3],[var-3],[var-8],[var-6]],
                                      [[val-3, omin-2, omax-3],
                                       [val-5, omin-0, omax-1],
                                       [val-6, omin-1, omax-2]])).

ctr_draw_example(global_cardinality_low_up,
                 ['VARIABLES'],
                 [[[var-3],[var-3],[var-8],[var-6]]],
                 ['SELF'],
                 [1-1,2-2,4-4],
                 ['NVERTEX',
                  'FOREACH'('VALUES',[3-[1,2],5-[],6-[4]])],
                 '','3:NVERTEX=2, 5:NVERTEX=0, 6:NVERTEX=1',
                 [2.145,2.145,2.145,2.145]).

ctr_cond_imply(global_cardinality_low_up, increasing_global_cardinality, [increasing('VARIABLES')], [], id).

ctr_see_also(global_cardinality_low_up,
 [link('specialisation',        alldifferent,                       'each value should occur at most once',                                            []),
  link('generalisation',        global_cardinality,                 '%e %e replaced by %e',                                                            [fixed, interval, variable]),
  link('soft variant',          open_global_cardinality_low_up,     'a %e %e defines the set of variables that are actually considered',               [set, variable]),
  link('system of constraints', sliding_distribution,               'one %c constraint for each sliding sequence of $\\argument{SEQ}$ consecutive %e', [global_cardinality_low_up, variables]),
  link('shift of concept',      global_cardinality_low_up_no_loop,  'assignment of a %e to its position is ignored',                                   [variable]),
  link('implied by',            increasing_global_cardinality,      'a %c constraint where the %e are increasing',                                     [global_cardinality_low_up, variables]),
  link('implied by',            same_and_global_cardinality_low_up, '',                                                                                []),
  link('common keyword',        open_global_cardinality,            '%k,%k',                                                                           ['assignment','counting constraint']),
  link('related',               ordered_global_cardinality,         'restrictions are done on nested sets of values, all starting from first value',   [])]).

ctr_key_words(global_cardinality_low_up,['value constraint'   ,
                                         'counting constraint',
                                         'assignment'         ,
                                         'flow'               ,
                                         'arc-consistency'    ,
                                         'bound-consistency'  ,
                                         'DFS-bottleneck'     ,
                                         'entailment'         ]).

ctr_persons(global_cardinality_low_up,['R\\\'egin J.-C.',
                                       'Bessi\\`ere C.' ,
                                       'Quimper C.-G.'  ,
                                       'Katsirelos G.'  ,
                                       'Narodytska N.'  ,
                                       'Walsh T.'       ]).

ctr_eval(global_cardinality_low_up, [reformulation(global_cardinality_low_up_r)]).

global_cardinality_low_up_r(VARIABLES, VALUES) :-
    length(VARIABLES, N),
	collection(VARIABLES, [dvar]),
	collection(VALUES, [int,int(0,N),int(0,N)]),
	length(VALUES, M),
    M > 0,
	get_attr1(VARIABLES, VARS ),
	get_attr1(VALUES   , VALS ),
	get_attr2(VALUES   , OMINS),
	get_attr3(VALUES   , OMAXS),
    all_different(VALS),
	get_minimum(VARS, MINVARS),
	get_maximum(VARS, MAXVARS),
	get_minimum(VALS, MINVALS),
	get_maximum(VALS, MAXVALS),
	MIN is min(MINVARS,MINVALS),
	MAX is max(MAXVARS,MAXVALS),
    complete_card_low_up(MIN, MAX, N, VALS, OMINS, OMAXS, VN),
    global_cardinality(VARS, VN).
