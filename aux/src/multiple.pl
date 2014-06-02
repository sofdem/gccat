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

ctr_date(multiple,['20120501']).

ctr_origin(multiple, 'Arithmetic.', []).

ctr_arguments(multiple,
              ['X'-dvar,
               'Y'-dvar,
               'C'-int]).

ctr_restrictions(multiple, ['X' =\= 0,
			    'Y' =\= 0,
			    'C' >   0]).

ctr_typical(multiple, ['C' > 1]).

ctr_functional_dependency(multiple, 3, [1,2]).

ctr_predefined(multiple).

ctr_example(multiple,
            multiple(8,-2,4)).

ctr_key_words(multiple,['predefined constraint',
                        'arithmetic constraint',
                        'binary constraint'    ,
                        'functional dependency']).

ctr_eval(multiple, [checker(multiple_c),
		    builtin(multiple_b)]).

multiple_c(X, Y, C) :-
    check_type(int, X),
    X =\= 0,
    check_type(int, Y),
    Y =\= 0,
    check_type(dvar, C),
    AX is abs(X),
    AY is abs(Y),
    MAX is max(AX,AY),
    MIN is min(AX,AY),
    DIV is  MAX // MIN,
    C #= DIV,
    MAX =:= C*MIN.

multiple_b(X, Y, C) :-
    check_type(dvar, X),
    X #\= 0,
    check_type(dvar, Y),
    Y #\= 0,
    check_type(int, C),
    max(abs(X),abs(Y)) #= C*min(abs(X),abs(Y)).
