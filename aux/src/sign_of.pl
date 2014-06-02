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

ctr_date(sign_of,['20110612']).

ctr_origin(sign_of, 'Arithmetic.', []).

ctr_usual_name(sign_of, sign).

ctr_arguments(sign_of,
              ['S'-dvar,
               'X'-dvar]).

ctr_restrictions(sign_of,
                 ['S' >= -1,
                  'S' =<  1]).

ctr_typical(sign_of,
            ['S' =\= 0,
             'X' =\= 0]).

ctr_pure_functional_dependency(sign_of, []).
ctr_functional_dependency(sign_of, 1, [2]).

ctr_predefined(sign_of).

ctr_example(sign_of,
            [sign_of(-1,-8),
             sign_of(0,0),
             sign_of(1,8)]).

ctr_see_also(sign_of,
 [link('implies', same_sign,        '', []),
  link('implies', zero_or_not_zero, '', [])]).

ctr_key_words(sign_of,['predefined constraint'     ,
                       'arithmetic constraint'     ,
                       'binary constraint'         ,
                       'functional dependency'     ,
		       'pure functional dependency',
                       'arc-consistency'           ]).

ctr_eval(sign_of, [checker(sign_of_c),
		   builtin(sign_of_b)]).

sign_of_c(S, X) :-
    check_type(int, S),
    check_type(int, X),
    (S = -1 -> X < 0 ;
     S =  0 -> X = 0 ;
     S =  1 -> X > 0 ).

sign_of_b(S, X) :-
    check_type(dvar, S),
    check_type(dvar, X),
    S #>= -1,
    S #=<  1,
    (X #< 0 #/\ S #= -1) #\/ (X #= 0 #/\ S #= 0) #\/ (X #> 0 #/\ S #= 1).
