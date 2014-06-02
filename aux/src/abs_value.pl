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

ctr_date(abs_value,['20100821']).

ctr_origin(abs_value, 'Arithmetic.', []).

ctr_usual_name(abs_value, abs).

ctr_arguments(abs_value,
              ['Y'-dvar,
               'X'-dvar]).

ctr_synonyms(abs_value,[absolute_value]).

ctr_restrictions(abs_value, ['Y' >= 0]).

ctr_pure_functional_dependency(abs_value, []).
ctr_functional_dependency(abs_value, 1, [2]).

ctr_predefined(abs_value).

ctr_example(abs_value,
            abs_value(8,-8)).

ctr_see_also(abs_value,
 [link('implies',                     geq,              '', []),
  link('implies',                     zero_or_not_zero, '', []),
  link('implies (if swap arguments)', opposite_sign,    '', []),
  link('implies (if swap arguments)', zero_or_not_zero, '', []),
  link('implied by',                  eq,               '', [])]).

ctr_key_words(abs_value,['predefined constraint'     ,
                         'arithmetic constraint'     ,
                         'binary constraint'         ,
                         'functional dependency'     ,
		         'pure functional dependency',
                         'arc-consistency'           ]).

ctr_eval(abs_value, [checker(abs_value_c),
		     builtin(abs_value_b)]).

abs_value_c(Y, X) :-
    check_type(int, Y),
    check_type(int, X),
    Y is abs(X).

abs_value_b(Y, X) :-
    check_type(dvar, Y),
    check_type(dvar, X),
    (X #>= 0 #/\ Y #= X) #\/ (X #< 0 #/\ X+Y #= 0).
