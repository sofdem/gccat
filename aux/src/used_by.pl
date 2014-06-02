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

ctr_date(used_by,['20000128','20030820','20040530','20060820']).

ctr_origin(used_by, 'N.~Beldiceanu', []).

ctr_arguments(used_by,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(used_by,
                 [items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_restrictions(used_by,
                 [size('VARIABLES1') >= size('VARIABLES2'),
                  required('VARIABLES1',var)              ,
                  required('VARIABLES2',var)              ]).

ctr_typical(used_by,
            [size('VARIABLES1')      > 1,
             range('VARIABLES1'^var) > 1,
             size('VARIABLES2')      > 1,
             range('VARIABLES2'^var) > 1]).

ctr_contractible(used_by, [], 'VARIABLES2', any).

ctr_extensible(used_by, [], 'VARIABLES1', any).

% used_by('VARIABLES11', 'VARIABLES12') and
% used_by('VARIABLES21', 'VARIABLES22') =>
% used_by(union('VARIABLES11','VARIABLES21'), union('VARIABLES12','VARIABLES22'))
ctr_aggregate(used_by , [], [union, union]).

ctr_graph(used_by,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          [for_all('CC','NSOURCE' >= 'NSINK'),
           'NSINK' = size('VARIABLES2')],
          []).

ctr_example(used_by,
            used_by([[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                    [[var-1],[var-1],[var-2],[var-5]])).

ctr_draw_example(used_by,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                  [[var-1],[var-1],[var-2],[var-5]]],
                 ['PRODUCT'],
                 [1-[1,2],
                  3-[1,2],
                  4-4,
                  5-3,
                  6-[1,2]],
                 ['NSINK'([7,8,9,10]),
                  'NCC'([[1,3,6,7,8],[4,10],[5,9]])],
                 '','CC#1:NSINK=2,CC#2:NSINK=1,CC#3:NSINK=1\\nNSINK=4',
                 [3,2.145,3.1,2.145]).

ctr_see_also(used_by,
 [link('system of constraints', k_used_by,         '',                  []),
  link('generalisation',        used_by_interval,  '%e replaced by %e', [variable,variable/constant]),
  link('generalisation',        used_by_modulo,    '%e replaced by %e', [variable,variable mod constant]),
  link('generalisation',        used_by_partition, '%e replaced by %e', [variable,in_list(variable,partition)]),
  link('soft variant',          soft_used_by_var,  '%k',                ['variable-based violation measure']),
  link('implied by',            same,              '',                  []),
  link('implies',               uses,              '',                  [])]).

ctr_key_words(used_by,['constraint between two collections of variables',
                       'multiset'                                       ,
                       'inclusion'                                      ,
		       'sort based reformulation'                       ,
                       'flow'                                           ,
		       'bipartite matching'                             ,
                       'arc-consistency'                                ,
                       'bound-consistency'                              ,
                       'automaton'                                      ,
                       'automaton with array of counters'               ,
                       'DFS-bottleneck'                                 ]).

ctr_persons(used_by,['Beldiceanu N.',
                     'Katriel I.'   ,
                     'Thiel S.'     ,
		     'Cymer R.'     ]).

ctr_eval(used_by, [reformulation(used_by_r)]).

used_by_r(VARIABLES1, VARIABLES2) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
    	N1 >= N2,
	get_attr1(VARIABLES1, VARS1),
	get_attr1(VARIABLES2, VARS2),
    	used_by_reified(VARS2, VARS1, VARS2).
