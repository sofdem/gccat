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

ctr_date(npair,['20030820','20060812']).

ctr_origin(npair, 'Derived from %c.', [nvalue]).

ctr_arguments(npair,
              ['NPAIRS'-dvar                       ,
               'PAIRS'-collection(x-dvar, y-dvar)]).

ctr_exchangeable(npair,
                 [items('PAIRS',all),
                  attrs_sync('PAIRS',[[x,y]]),
                  vals(['NPAIRS'],int,=\=,all,dontcare)]).

ctr_restrictions(npair,
                 ['NPAIRS' >= min(1,size('PAIRS')),
                  'NPAIRS' =< size('PAIRS')       ,
                  required('PAIRS',[x,y])         ]).

ctr_typical(npair,
            ['NPAIRS'         > 1            ,
             'NPAIRS'         < size('PAIRS'),
             size('PAIRS')    > 1            ,
             range('PAIRS'^x) > 1            ,
             range('PAIRS'^y) > 1            ]).

ctr_pure_functional_dependency(npair, []).
ctr_functional_dependency(npair, 1, [2]).

ctr_contractible(npair, ['NPAIRS'=1,size('PAIRS')>0], 'PAIRS', any).
ctr_contractible(npair, ['NPAIRS'=size('PAIRS')], 'PAIRS', any).

ctr_graph(npair,
          ['PAIRS'],
          2,
          ['CLIQUE'>>collection(pairs1,pairs2)],
          [pairs1^x = pairs2^x,
           pairs1^y = pairs2^y],
          ['NSCC' = 'NPAIRS'],
          []).

ctr_example(npair,
            npair(2,
                  [[x-3, y-1],
                   [x-1, y-5],
                   [x-3, y-1],
                   [x-3, y-1],
                   [x-1, y-5]])).

ctr_draw_example(npair,
                 ['PAIRS'],
                 [[[x-3, y-1],
                   [x-1, y-5],
                   [x-3, y-1],
                   [x-3, y-1],
                   [x-1, y-5]]],
                 ['CLIQUE'],
                 [1-[1,3,4],
                  2-[2,5],
                  3-[1,3,4],
                  4-[1,3,4],
                  5-[2,5]],
                 ['NSCC'([[1,3,4],[2,5]])],
                 '','NSCC=2',
                 [2.5,2.2,2.145,2.21]).

ctr_see_also(npair,
 [link('specialisation', nvalue,       '%e of %e replaced by %e', [pair,variables,variable]),
  link('related',        nequivalence, '%e of %e replaced by %e', [pair,variables,variable mod constant]),
  link('related',        nclass,       '%e of %e replaced by %e', [pair,variables,in_list(variable,partition)]),
  link('related',        ninterval,    '%e of %e replaced by %e', [pair,variables,variable/constant])]).

ctr_key_words(npair,['counting constraint'                   ,
                     'value partitioning constraint'         ,
                     'number of distinct equivalence classes',
                     'pair'                                  ,
                     'strongly connected component'          ,
                     'equivalence'                           ,
                     'functional dependency'                 ,
		     'pure functional dependency'            ]).
