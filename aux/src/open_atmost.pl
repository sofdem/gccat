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

ctr_date(open_atmost,['20060824']).

ctr_origin(open_atmost, 'Derived from %c and %c.', [atmost,open_global_cardinality]).

ctr_arguments(open_atmost,
              ['S'-svar                        ,
               'N'-int                         ,
               'VARIABLES'-collection(var-dvar),
               'VALUE'-int                     ]).

ctr_exchangeable(open_atmost,
                 [vals(['N'],int,<,dontcare,dontcare),
                  vals(['VARIABLES'^var],comp('VALUE'),=<,dontcare,dontcare)]).

ctr_restrictions(open_atmost,
                 ['S' >= 1                 ,
                  'S' =< size('VARIABLES') ,
                  'N' >= 0                 ,
                  required('VARIABLES',var)]).

ctr_typical(open_atmost,
            ['N' > 0                ,
             'N' < size('VARIABLES'),
             size('VARIABLES') > 1  ]).

ctr_contractible(open_atmost, [], 'VARIABLES', suffix).

ctr_graph(open_atmost,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var = 'VALUE'  ,
           in_set(variables^key,'S')],
          ['NARC' =< 'N'],
          []).

ctr_example(open_atmost,
            open_atmost({2,3,4},1,[[var-2],[var-2],[var-4],[var-5]],2)).

ctr_draw_example(open_atmost,
                 ['VARIABLES'],
                 [[[var-2],[var-2],[var-4],[var-5]]],
                 ['SELF'],
                 [2-2],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,2.145,0.6,0.6]).

ctr_see_also(open_atmost,
 [link('hard version',              atmost,                  '',      []),
  link('comparison swapped',        open_atleast,            '',      []),
  link('common keyword',            open_among,              '%k,%k', ['open constraint', 'value constraint']),
  link('common keyword',            open_global_cardinality, '%k,%k', ['open constraint', 'value constraint']),
  link('used in graph description', in_set,                  '',      [])]).

ctr_key_words(open_atmost,['open constraint'                   ,
                           'value constraint'                  ,
                           'at most'                           ,
                           'constraint involving set variables']).
