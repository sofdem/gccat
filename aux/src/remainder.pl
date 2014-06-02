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

ctr_date(remainder,['20110612']).

ctr_origin(remainder, 'Arithmetic.', []).

ctr_arguments(remainder,
              ['Q'-dvar,
               'D'-dvar,
               'R'-dvar]).

ctr_synonyms(remainder,[modulo, mod]).

ctr_restrictions(remainder, ['Q' >= 0  ,
                             'D' >  0  ,
                             'R' >= 0  ,
                             'R' <  'D']).

ctr_pure_functional_dependency(remainder, []).
ctr_functional_dependency(remainder, 3, [1,2]).

ctr_predefined(remainder).

ctr_example(remainder,
            remainder(15,2,1)).

ctr_key_words(remainder,['predefined constraint'     ,
                         'arithmetic constraint'     ,
                         'ternary constraint'        ,
                         'functional dependency'     ,
		         'pure functional dependency']).

ctr_eval(remainder, [checker(remainder_c),
		     builtin(remainder_b)]).

remainder_c(Q, D, R) :-
    check_type(int, Q),
    check_type(int, D),
    check_type(dvar, R),
    Q >= 0,
    D >  0,
    R #>= 0,
    R #<  D,
    REM is Q mod D,
    R #= REM.

remainder_b(Q, D, R) :-
    check_type(dvar, Q),
    check_type(dvar, D),
    check_type(dvar, R),
    Q #>= 0,
    D #>  0,
    R #>= 0,
    R #<  D,
    Q mod D #= R.
