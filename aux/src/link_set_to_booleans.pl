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

ctr_date(link_set_to_booleans,['20030820','20060811']).

ctr_origin(link_set_to_booleans, 'Inspired by %c.', [domain_constraint]).

ctr_arguments(link_set_to_booleans,
              ['SVAR'-svar                                ,
               'BOOLEANS'-collection(bool-dvar, val-int)]).

ctr_exchangeable(link_set_to_booleans,
                 [items('BOOLEANS',all)]).

ctr_restrictions(link_set_to_booleans,
                 [required('BOOLEANS',[bool,val]),
                  'BOOLEANS'^bool >= 0           ,
                  'BOOLEANS'^bool =< 1           ,
                  distinct('BOOLEANS',val)       ]).

ctr_typical(link_set_to_booleans,
            [size('BOOLEANS')       > 1,
             range('BOOLEANS'^bool) > 1]).

ctr_derived_collections(link_set_to_booleans,
                        [col('SET'-collection(one-int, setvar-svar),
                             [item(one-1, setvar-'SVAR')])]).

ctr_graph(link_set_to_booleans,
          ['SET','BOOLEANS'],
          2,
          ['PRODUCT'>>collection(set,booleans)],
          [booleans^bool = set^one #<=> in_set(booleans^val,set^setvar)],
          ['NARC' = size('BOOLEANS')],
          []).

ctr_example(link_set_to_booleans,
            link_set_to_booleans({1,3,4},
                                 [[bool-0, val-0],
                                  [bool-1, val-1],
                                  [bool-0, val-2],
                                  [bool-1, val-3],
                                  [bool-1, val-4],
                                  [bool-0, val-5]])).

ctr_draw_example(link_set_to_booleans,
                 ['SET','BOOLEANS'],
                 [[[one-1, setvar-{1,3,4}]],
                  [[bool-0, val-0],
                   [bool-1, val-1],
                   [bool-0, val-2],
                   [bool-1, val-3],
                   [bool-1, val-4],
                   [bool-0, val-5]]],
                 ['PRODUCT'],
                 [1-[1,2,3,4,5,6]],
                 ['NARC'],
                 '','NARC=6',
                 [3,2.145,3.5,2]).

ctr_see_also(link_set_to_booleans,
 [link('common keyword', domain_constraint,         '%k', ['channelling constraint']),
  link('common keyword', alldifferent_between_sets, '%k', ['constraint involving set variables']),
  link('common keyword', clique,                    '%k', ['constraint involving set variables']),
  link('common keyword', k_cut,                     '%k', ['constraint involving set variables']),
  link('common keyword', symmetric_gcc,             '%k', ['constraint involving set variables']),
  link('common keyword', tour,                      '%k', ['constraint involving set variables']),
  link('common keyword', roots,                     '%k', ['constraint involving set variables']),
  link('common keyword', strongly_connected,        '%k', ['constraint involving set variables']),
  link('common keyword', symmetric_cardinality,     '%k', ['constraint involving set variables']),
  link('common keyword', path_from_to,              '%k', ['constraint involving set variables'])]).

ctr_key_words(link_set_to_booleans,['decomposition'                     ,
                                    'value constraint'                  ,
                                    'channelling constraint'            ,
                                    'set channel'                       ,
                                    'linear programming'                ,
                                    'constraint involving set variables',
                                    'derived collection'                ]).
