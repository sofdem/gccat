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

ctr_date(increasing_nvalue_chain,['20091118']).

ctr_origin(increasing_nvalue_chain, 'Derived from %c.', [increasing_nvalue]).

ctr_arguments(increasing_nvalue_chain,
              ['NVAL'-dvar                            ,
               'VARIABLES'-collection(b-dvar,var-dvar)]).

ctr_restrictions(increasing_nvalue_chain,
                 ['NVAL' >= min(1,size('VARIABLES')),
                  'NVAL' =< size('VARIABLES')       ,
                  required('VARIABLES',[b,var])     ,
                  'VARIABLES'^b >= 0                ,
                  'VARIABLES'^b =< 1                ]).

ctr_typical(increasing_nvalue_chain,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^b)   > 1,
             range('VARIABLES'^var) > 1]).

ctr_graph(increasing_nvalue_chain,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables2^b = 0 #\/ variables1^var =< variables2^var],
          ['NARC' = size('VARIABLES')-1],
          []).

ctr_graph(increasing_nvalue_chain,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          [variables2^b = 0 #\/ variables1^var < variables2^var],
          ['NARC' = 'NVAL'-1],
          []).

ctr_example(increasing_nvalue_chain,
            increasing_nvalue_chain(6, [[b-0,var-2],[b-1,var-4],[b-1,var-4],[b-1,var-4],
                                        [b-0,var-4],[b-1,var-8],
                                        [b-0,var-1],
                                        [b-0,var-7],[b-1,var-7]])).

ctr_draw_example(increasing_nvalue_chain,
                 ['VARIABLES'],
                 [[[b-0,var-2],[b-1,var-4],[b-1,var-4],[b-1,var-4],[b-0,var-4],[b-1,var-8],[b-0,var-1],[b-0,var-7],[b-1,var-7]]],
                 ['PATH'],
                 [1-2,4-5,5-6,6-7,7-8],
                 ['NARC'],
                 '','NARC=5',
                 [1.5,3,3,3]).

ctr_see_also(increasing_nvalue_chain,
 [link('related', increasing_nvalue, '', []),
  link('related', nvalue,            '', []),
  link('related', ordered_nvector,   '', [])]).

ctr_key_words(increasing_nvalue_chain,['counting constraint'      ,
                                       'number of distinct values',
                                       'order constraint'         ]).

ctr_eval(increasing_nvalue_chain, [reformulation(increasing_nvalue_chain_r)]).

increasing_nvalue_chain_r(_, _).
