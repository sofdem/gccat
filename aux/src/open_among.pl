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

ctr_date(open_among,['20060824']).

ctr_origin(open_among, 'Derived from %c and %c.', [among,open_global_cardinality]).

ctr_arguments(open_among,
              ['S'-svar                        ,
               'NVAR'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(open_among,
                 [items('VALUES',all),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_restrictions(open_among,
                 ['S' >= 1                   ,
                  'S' =< size('VARIABLES')   ,
                  'NVAR' >= 0                ,
                  'NVAR' =< size('VARIABLES'),
                  required('VARIABLES',var)  ,
                  required('VALUES',val)     ,
                  distinct('VALUES',val)     ]).

ctr_typical(open_among,
            ['NVAR' > 0                         ,
             'NVAR' < size('VARIABLES')         ,
              size('VARIABLES') > 1             ,
              size('VALUES')    > 1             ,
              size('VARIABLES') > size('VALUES')]).

ctr_functional_dependency(open_among, 2, [1,3,4]).

ctr_contractible(open_among, ['NVAR'=0], 'VARIABLES', suffix).

ctr_graph(open_among,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [in(variables^var,'VALUES'),
	   in_set(variables^key,'S') ],
          ['NARC' = 'NVAR'],
	  []).

ctr_example(open_among,
            open_among({2,3,4,5},
                       3,
                       [[var-8],[var-5],[var-5],[var-4],[var-1]],
                       [[val-1],[val-5],[val-8]])).

ctr_draw_example(open_among,
                 ['VARIABLES'],
                 [[[var-8],[var-5],[var-5],[var-4],[var-1]]],
                 ['SELF'],
                 [2-2,3-3,5-5],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,0.53625]).

ctr_see_also(open_among,
 [link('hard version',              among,                   '',      []),
  link('common keyword',            open_atleast,            '%k,%k', ['open constraint', 'value constraint']),
  link('common keyword',            open_atmost,             '%k,%k', ['open constraint', 'value constraint']),
  link('common keyword',            open_global_cardinality, '%k,%k', ['open constraint', 'counting constraint']),
  link('used in graph description', in_set,                  '',      [])]).

ctr_key_words(open_among,['open constraint'                   ,
                          'value constraint'                  ,
                          'counting constraint'               ,
                          'constraint involving set variables',
                          'functional dependency'             ]).
