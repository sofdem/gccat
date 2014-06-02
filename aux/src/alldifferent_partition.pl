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

ctr_date(alldifferent_partition,['20030820','20060803']).

ctr_origin(alldifferent_partition, 'Derived from %c.', [alldifferent]).

ctr_types(alldifferent_partition,
          ['VALUES'-collection(val-int)]).

ctr_synonyms(alldifferent_partition,[alldiff_partition    ,
                                     alldistinct_partition]).

ctr_arguments(alldifferent_partition,
              ['VARIABLES'-collection(var-dvar)  ,
               'PARTITIONS'-collection(p-'VALUES')]).

ctr_exchangeable(alldifferent_partition,
                 [items('VARIABLES',all),
                  items('PARTITIONS',all),
                  items('PARTITIONS'^p,all),
                  vals(['VARIABLES'^var],part('PARTITIONS'),=,all,dontcare),
                  vals(['VARIABLES'^var],part('PARTITIONS'),=\=,all,in)]).

ctr_restrictions(alldifferent_partition,
                 [size('VALUES') >= 1                    ,
                  required('VALUES',val)                 ,
                  distinct('VALUES',val)                 ,
                  size('VARIABLES') =< size('PARTITIONS'),
                  required('VARIABLES',var)              ,
                  size('PARTITIONS') >= 2                ,
                  required('PARTITIONS',p)               ]).

ctr_typical(alldifferent_partition,
            [size('VARIABLES') > 2]).

ctr_contractible(alldifferent_partition, [], 'VARIABLES', any).

ctr_graph(alldifferent_partition,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [in_same_partition(variables1^var,variables2^var,'PARTITIONS')],
          ['MAX_NSCC' =< 1],
	    ['ONE_SUCC']).

ctr_example(alldifferent_partition,
            alldifferent_partition([[var-6],[var-3],[var-4]],
                                   [[p-[[val-1], [val-3]]],
                                    [p-[[val-4]         ]],
                                    [p-[[val-2], [val-6]]]])).

ctr_draw_example(alldifferent_partition,
                 ['VARIABLES'],
                 [[[var-6],[var-3],[var-4]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 []).

ctr_see_also(alldifferent_partition,
 [link(specialisation,              alldifferent,      '%e replaced by %e', [in_list(variable,partition),variable]),
  link('common keyword',            in_same_partition, '%k',                [partition]),
  link('used in graph description', in_same_partition, '',                  [])]).

ctr_key_words(alldifferent_partition,['value constraint'            ,
                                      'partition'                   ,
                                      'incompatible pairs of values',
                                      'all different'               ,
                                      'sort based reformulation'    ,
                                      'one\\_succ'                  ,
                                      'arc-consistency'             ]).

ctr_eval(alldifferent_partition, [reformulation(alldifferent_partition_r)]).

alldifferent_partition_r(VARIABLES, PARTITIONS) :-
	collection(VARIABLES, [dvar]),
      collection(PARTITIONS, [col_len_gteq(1, [int])]),
	get_attr1(VARIABLES, VARS),
	get_col_attr1(PARTITIONS, 1, PVALS),
	flattern(PVALS, VALS),
	all_different(VALS),
	length(VARIABLES, N),
	length(PARTITIONS, M),
	N =< M,
	M > 1,
	length(PVALS, LPVALS),
	get_partition_var(VARS, PVALS, PVARS, LPVALS, 0),
	all_different(PVARS).
