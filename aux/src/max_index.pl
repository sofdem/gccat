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

ctr_date(max_index,['20030820','20040530','20041230','20060811']).

ctr_origin(max_index, 'N.~Beldiceanu', []).

ctr_arguments(max_index,
              ['MAX_INDEX'-dvar,
               'VARIABLES'-collection(index-int, var-dvar)]).

ctr_exchangeable(max_index,
                 [items('VARIABLES',all),
                  translate(['VARIABLES'^var])]).

ctr_restrictions(max_index,
                 [size('VARIABLES') >  0                ,
                  'MAX_INDEX'       >= 0                ,
                  'MAX_INDEX'       =< size('VARIABLES'),
                  required('VARIABLES',[index,var])     ,
                  'VARIABLES'^index >= 1                ,
                  'VARIABLES'^index =< size('VARIABLES'),
                  distinct('VARIABLES',index)           ]).

ctr_typical(max_index,
            [size('VARIABLES')      > 0,
             range('VARIABLES'^var) > 1]).

ctr_graph(max_index,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [(variables1^key = variables2^key #\/ variables1^var > variables2^var)],
          ['ORDER'(0,0,index) = 'MAX_INDEX'],
          []).

ctr_example(max_index,
            max_index(3,[[index-1, var-3],
                         [index-2, var-2],
                         [index-3, var-7],
                         [index-4, var-2],
                         [index-5, var-7]])).

ctr_draw_example(max_index,
                 ['VARIABLES'],
                 [[[index-1, var-3],
                   [index-2, var-2],
                   [index-3, var-7],
                   [index-4, var-2],
                   [index-5, var-7]]],
                 ['CLIQUE'],
                 [1-[1,2,4],
                  2-2,
                  3-[1,2,3,4],
                  4-4,
                  5-[1,2,4,5]],
                 ['ORDER'([3])],
                 '','ORDER(0,0,index)=3',
                 [2.3,2.2,2.145,2.145]).

ctr_see_also(max_index,
 [link('comparison swapped', min_index, '', [])]).

ctr_key_words(max_index,['order constraint'     ,
                         'maximum'              ,
                         'functional dependency']).

ctr_persons(max_index,['Beldiceanu N.']).
