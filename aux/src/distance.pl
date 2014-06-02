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

ctr_date(distance,['20090416']).

ctr_origin(distance, 'Arithmetic constraint.', []).

ctr_arguments(distance, ['X'-dvar,'Y'-dvar,'Z'-dvar]).

ctr_exchangeable(distance,
                 [args([['X','Y'],['Z']])]).

ctr_restrictions(distance,
                 ['Z' >= 0]).

ctr_typical(distance,
            ['Z' > 0]).

ctr_pure_functional_dependency(distance, []).
ctr_functional_dependency(distance, 3, [1,2]).

ctr_predefined(distance).

ctr_example(distance,
            distance(5, 7, 2)).

ctr_see_also(distance,
 [link('implies', leq_cst,      '',                                                                                   []),
  link('related', all_min_dist, 'fixed minimum distance between all pairs of variables of a collection of variables', []),
  link('related', smooth,       '',                                                                                   [])]).

ctr_key_words(distance,['arithmetic constraint'     ,
                        'predefined constraint'     ,
                        'ternary constraint'        ,
                        'functional dependency'     ,
		        'pure functional dependency']).

ctr_eval(distance, [checker(distance_c),
		    builtin(distance_b)]).

distance_c(X, Y, Z) :-
    check_type(int, X),
    check_type(int, Y),
    check_type(dvar_gteq(0), Z),
    Z #= abs(X-Y).

distance_b(X, Y, Z) :-
    check_type(dvar, X),
    check_type(dvar, Y),
    check_type(dvar_gteq(0), Z),
    Z #= abs(X-Y).

% distance_smt(F, X, Y, Z) :-
%     check_type(dvar, X),
%     check_type(dvar, Y),
%     check_type(dvar_gteq(0), Z),
%     smt(F #<=> (Z#>=0 #/\ (Z#=X-Y #\/ Z #=Y-X))).

%call(F) or smt(F) or fix(F,X,Y,Z) +: Formula(F,X,Y,Z).
