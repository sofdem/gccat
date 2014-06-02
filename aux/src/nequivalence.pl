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

ctr_date(nequivalence,['20000128','20030820','20060812']).

ctr_origin(nequivalence, 'Derived from %c.', [nvalue]).

ctr_arguments(nequivalence,
              ['NEQUIV'-dvar                   ,
               'M'-int                         ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(nequivalence,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],mod('M'),=,dontcare,dontcare)]).

ctr_restrictions(nequivalence,
                 [required('VARIABLES',var)             ,
                  'NEQUIV' >= min( 1 ,size('VARIABLES')),
                  'NEQUIV' =< min('M',size('VARIABLES')),
                  'NEQUIV' =< range('VARIABLES'^var)    ,
                  'M'      >  0                         ]).

ctr_typical(nequivalence,
            ['NEQUIV' > 1                      ,
             'NEQUIV' < size('VARIABLES')      ,
             'NEQUIV' < range('VARIABLES'^var) ,
             'M'      > 1                      ,
             'M'      < maxval('VARIABLES'^var)]).

ctr_pure_functional_dependency(nequivalence, []).
ctr_functional_dependency(nequivalence, 1, [2,3]).

ctr_contractible(nequivalence, ['NEQUIV'=1,size('VARIABLES')>0], 'VARIABLES', any).
ctr_contractible(nequivalence, ['NEQUIV'=size('VARIABLES')], 'VARIABLES', any).

ctr_extensible(nequivalence, ['NEQUIV'='M'], 'VARIABLES', any).

ctr_graph(nequivalence,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var mod 'M' = variables2^var mod 'M'],
          ['NSCC' = 'NEQUIV'],
          []).

ctr_example(nequivalence,
            nequivalence(2,3,
                         [[var-3],[var-2],[var-5],[var-6],
                          [var-15],[var-3],[var-3]])).

ctr_draw_example(nequivalence,
                 ['VARIABLES'],
                 [[[var-3],[var-2],[var-5],[var-6],[var-15],[var-3],[var-3]]],
                 ['CLIQUE'],
                 [1-[1,4,5,6,7],
                  2-[2,3],
                  3-[2,3],
                  4-[1,4,5,6,7],
                  5-[1,4,5,6,7],
                  6-[1,4,5,6,7],
                  7-[1,4,5,6,7]],
                 ['NSCC'([[1,4,5,6,7],[2,3]])],
                 '','NSCC=2',
                 [2.145,2.3,2.4,2.3]).

ctr_see_also(nequivalence,
 [link('specialisation', nvalue,    '%e replaced by %e',       [variable mod constant,variable]),
  link('related',        nclass,    '%e replaced by %e',       [variable mod constant,in_list(variable,partition)]),
  link('related',        ninterval, '%e replaced by %e',       [variable mod constant,variable/constant]),
  link('related',        npair,     '%e replaced by %e of %e', [variable mod constant,pair,variables])]).

ctr_key_words(nequivalence,['counting constraint'                   ,
                            'value partitioning constraint'         ,
                            'number of distinct equivalence classes',
                            'strongly connected component'          ,
                            'equivalence'                           ,
                            'functional dependency'                 ,
		            'pure functional dependency'            ]).

ctr_persons(nequivalence,['Beldiceanu N.',
                          'Carlsson M.'  ,
                          'Thiel S.'     ]).
