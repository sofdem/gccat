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

ctr_date(nclass,['20000128','20030820','20060812']).

ctr_origin(nclass, 'Derived from %c.', [nvalue]).

ctr_types(nclass,
          ['VALUES'-collection(val-int)]).

ctr_arguments(nclass,
              ['NCLASS'-dvar                      ,
               'VARIABLES'-collection(var-dvar)   ,
               'PARTITIONS'-collection(p-'VALUES')]).

ctr_exchangeable(nclass,
                 [items('VARIABLES',all),
                  items('PARTITIONS',all),
                  items('PARTITIONS'^p,all),
                  vals(['VARIABLES'^var],part('PARTITIONS'),=,dontcare,dontcare),
                  vals(['VARIABLES'^var,'PARTITIONS'^p^val],int,=\=,all,dontcare)]).

ctr_restrictions(nclass,
                 [size('VALUES') >= 1                                  ,
                  required('VALUES',val)                               ,
                  distinct('VALUES',val)                               ,
                  'NCLASS' >= 0                                        ,
                  'NCLASS' =< min(size('VARIABLES'),size('PARTITIONS')),
                  'NCLASS' =< range('VARIABLES'^var)                   ,
                  required('VARIABLES',var)                            ,
                  required('PARTITIONS',p)                             ,
                  size('PARTITIONS') >= 2                              ]).

ctr_typical(nclass,
            ['NCLASS'           > 1                     ,
             'NCLASS'           < size('VARIABLES')     ,
             'NCLASS'           < range('VARIABLES'^var),
              size('VARIABLES') > size('PARTITIONS')    ]).

ctr_pure_functional_dependency(nclass, []).
ctr_functional_dependency(nclass, 1, [2,3]).

ctr_extensible(nclass, ['NCLASS'=size('PARTITIONS')], 'VARIABLES', any).

ctr_graph(nclass,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [in_same_partition(variables1^var,variables2^var,'PARTITIONS')],
          ['NSCC' = 'NCLASS'],
          []).

ctr_example(nclass,
            nclass(2,
                   [[var-3],[var-2],[var-7],[var-2],[var-6]],
                   [[p-[[val-1], [val-3]]],
                    [p-[[val-4]         ]],
                    [p-[[val-2], [val-6]]]])).

ctr_draw_example(nclass,
                 ['VARIABLES'],
                 [[[var-3],[var-2],[var-7],[var-2],[var-6]]],
                 ['CLIQUE'],
                 [1-1,
                  2-[2,4,5],
                  4-[2,4,5],
                  5-[2,4,5]],
                 ['NSCC'([[1],[2,4,5]])],
                 '','NSCC=2',
                 [2.145,2.3,2.145,2.3]).

ctr_see_also(nclass,
 [link('specialisation',            nvalue,            '%e replaced by %e',       [in_list(variable,partition),variable]),
  link('related',                   nequivalence,      '%e replaced by %e',       [in_list(variable,partition),variable mod constant]),
  link('related',                   ninterval,         '%e replaced by %e',       [in_list(variable,partition),variable/constant]),
  link('related',                   npair,             '%e replaced by %e of %e', [in_list(variable,partition),pair,variables]),
  link('used in graph description', in_same_partition, '',                        [])]).

ctr_key_words(nclass,['counting constraint'                   ,
                      'value partitioning constraint'         ,
                      'number of distinct equivalence classes',
                      'partition'                             ,
                      'strongly connected component'          ,
                      'equivalence'                           ,
                      'functional dependency'                 ,
		      'pure functional dependency'            ]).

ctr_persons(nclass,['Beldiceanu N.',
                    'Carlsson M.'  ,
                    'Thiel S.'     ]).
