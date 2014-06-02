/*
    The contents of this file are subject to the Mozilla Public License
    Version  1.1  (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at:

    http://www.mozilla.org/MPL/

    Software  distributed  under  the License is distributed on an "AS
    IS"  basis,  WITHOUT  WARRANTY  OF  ANY  KIND,  either  express or
    implied.  See  the  License  for  the  speciafic language governing
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

ctr_date(divisible_or,['20120212']).

ctr_origin(divisible_or, 'Arithmetic.', []).

ctr_arguments(divisible_or,
              ['C'-dvar,
               'D'-dvar]).

ctr_synonyms(divisible_or,[div_or]).

ctr_restrictions(divisible_or,
                 ['C' > 0,
                  'D' > 0]).

ctr_predefined(divisible_or).

ctr_example(divisible_or,
            divisible_or(4,12)).

ctr_see_also(divisible_or,
 [link('implies',    same_sign,        '', []),
  link('implies',    zero_or_not_zero, '', []),
  link('implied by', divisible,        '', [])]).

ctr_key_words(divisible_or,['predefined constraint',
                            'arithmetic constraint',
                            'binary constraint'    ]).

ctr_eval(divisible_or, [builtin(divisible_or_b)]).

divisible_or_b(C, D) :-
    check_type(dvar, C),
    check_type(dvar, D),
    C #> 0, 
    D #> 0,
    C mod D #= 0 #\/ D mod C #= 0.
