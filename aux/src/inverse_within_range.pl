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

ctr_date(inverse_within_range,['20060517','20060810']).

ctr_origin(inverse_within_range, 'Derived from %c.', [inverse]).

ctr_arguments(inverse_within_range,
              ['X'-collection(var-dvar),
               'Y'-collection(var-dvar)]).

ctr_exchangeable(inverse_within_range,
                 [args([['X','Y']])]).

ctr_synonyms(inverse_within_range,[inverse_in_range, inverse_range]).

ctr_restrictions(inverse_within_range,
                 [required('X',var),
                  required('Y',var)]).

ctr_typical(inverse_within_range,
            [size('X')      > 1,
             range('X'^var) > 1,
             size('Y')      > 1,
             range('Y'^var) > 1]).

ctr_graph(inverse_within_range,
          ['X','Y'],
          2,
          ['SYMMETRIC_PRODUCT'>>collection(s1,s2)],
          [s1^var = s2^key],
          [],
          ['BIPARTITE','NO_LOOP','SYMMETRIC']).

ctr_example(inverse_within_range,
            inverse_within_range([[var-9],[var-4],[var-2]],
                                 [[var-9],[var-3],[var-9],[var-2]])).

% ctr_draw_example(inverse_within_range,
%                  ['X','Y'],
%                  [[[var-4],[var-2]],
%                   [[var-3],[var-2]]],
%                  ['SYMMETRIC_PRODUCT'],
%                  [1-2,2-1],
%                  [],
%                  '','',
%                  [1.4,1.4,1.1,1.1]).

ctr_see_also(inverse_within_range,
 [link('specialisation', inverse,     'the %e collections have not necessarly the same number of items', [2]),
  link('common keyword', inverse_set, '%k',                                                              ['channelling constraint'])]).

ctr_key_words(inverse_within_range,['graph constraint'      ,
                                    'channelling constraint',
                                    'dual model'            ,
                                    'heuristics'            ,
                                    'bipartite'             ,
                                    'no loop'               ,
                                    'symmetric'             ]).
