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

ctr_date(minimum_modulo,['20000128','20030820','20041230','20060812']).

ctr_origin(minimum_modulo, 'Derived from %c.', [minimum]).

ctr_arguments(minimum_modulo,
              ['MIN'-dvar                      ,
               'VARIABLES'-collection(var-dvar),
               'M'-int                         ]).

ctr_exchangeable(minimum_modulo,
                 [items('VARIABLES',all)]).

ctr_restrictions(minimum_modulo,
                 [size('VARIABLES') > 0    ,
                  'M' > 0                  ,
                  required('VARIABLES',var)]).

ctr_typical(minimum_modulo,
            [size('VARIABLES')      > 1                      ,
             range('VARIABLES'^var) > 1                      ,
             'M'                    > 1                      ,
             'M'                    < maxval('VARIABLES'^var)]).

ctr_pure_functional_dependency(minimum_modulo, []).
ctr_functional_dependency(minimum_modulo, 1, [2,3]).

ctr_graph(minimum_modulo, 
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^key = variables2^key #\/
           variables1^var mod 'M' < variables2^var mod 'M'],
          ['ORDER'(0,'MAXINT',var) = 'MIN'],
          []).

ctr_example(minimum_modulo,
            [minimum_modulo(6, [[var-9],[var-1],[var-7],[var-6],[var-5]], 3),
             minimum_modulo(9, [[var-9],[var-1],[var-7],[var-6],[var-5]], 3)]).

ctr_draw_example(minimum_modulo,
                 ['VARIABLES'],
                 [[[var-9],[var-1],[var-7],[var-6],[var-5]]],
                 ['CLIQUE'],
                 [1-[1,2,3,5],
                  2-[2,5],
                  3-[3,5],
                  4-[2,3,4,5],
                  5-5],
                 ['ORDER'([1])],
                 '','ORDER(0,MAXINT,var)=9',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(minimum_modulo,
 [link('specialisation',     minimum,        '%e replaced by %e', [variable mod constant, variable]),
  link('comparison swapped', maximum_modulo, '',                  [])]).

ctr_key_words(minimum_modulo,['order constraint'          ,
                              'modulo'                    ,
                              'maxint'                    ,
                              'minimum'                   ,
                              'functional dependency'     ,
		              'pure functional dependency']).
