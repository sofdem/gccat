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

ctr_date(roots,['20070620']).

ctr_origin(roots, '\\cite{BessiereHebrardHnichKiziltanWalsh05IJCAI}', []).

ctr_arguments(roots,
              ['S'-svar                        ,
			   'T'-svar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_restrictions(roots,
                 ['S' =< size('VARIABLES') ,
                  required('VARIABLES',var)]).

ctr_typical(roots,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_derived_collections(roots,
                        [col('SETS'-collection(s-svar,t-svar),
                             [item(s-'S',t-'T')])]).

ctr_graph(roots,
          ['SETS','VARIABLES'],
          2,
          ['PRODUCT'>>collection(sets,variables)],
          [in_set(variables^key,sets^s) #<=> in_set(variables^var,sets^t)],
          ['NARC' = size('VARIABLES')],
          []).

ctr_example(roots,
            roots({2,4,5},{2,3,8},[[var-1],[var-3],[var-1],[var-2],[var-3]])).

ctr_draw_example(roots,
                 ['SETS','VARIABLES'],
                 [[[s-{2,4,5},t-{2,3,8}]],
                  [[var-1],[var-3],[var-1],[var-2],[var-3]]],
                 ['PRODUCT'],
                 [1-[1,2,3,4,5]],
                 ['NARC'],
                 '','NARC=5',
                 []).

ctr_see_also(roots,
 [link('related',        among,                  'can be expressed with %c',                           [roots]),
  link('related',        assign_and_nvalues,     'can be expressed with %c and $\\constraint{range}$', [roots]),
  link('related',        atleast,                'can be expressed with %c',                           [roots]),
  link('related',        atmost,                 'can be expressed with %c',                           [roots]),
  link('related',        common,                 'can be expressed with %c and $\\constraint{range}$', [roots]),
  link('related',        count,                  'can be expressed with %c',                           [roots]),
  link('related',        domain_constraint,      '',                                                   []),
  link('related',        global_cardinality,     'can be expressed with %c',                           [roots]),
  link('related',        global_contiguity,      'can be expressed with %c',                           [roots]),
  link('related',        symmetric_alldifferent, 'can be expressed with %c and $\\constraint{range}$', [roots]),
  link('related',        uses,                   'can be expressed with %c and $\\constraint{range}$', [roots]),
  link('common keyword', link_set_to_booleans,   '%k',                                                 ['constraint involving set variables'])]).

ctr_key_words(roots,['counting constraint'               ,
					 'value constraint'                  ,
					 'decomposition'                     ,
					 'hybrid-consistency'                ,
					 'disequality'                       ,
					 'constraint involving set variables']).

ctr_persons(roots,['Bessi\\`ere C.'     ,
                   'Hebrard E.'         ,
                   'Hnich B.'           ,
                   'K{\\i}z{\\i}ltan Z.',
                   'Walsh T.'           ]).
