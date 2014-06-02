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

ctr_date(eq,['20070821']).

ctr_origin(eq, 'Arithmetic.', []).

ctr_arguments(eq,
              ['VAR1'-dvar,
               'VAR2'-dvar]).

ctr_exchangeable(eq,
                 [args([['VAR1','VAR2']]),
                  vals(['VAR1','VAR2'],int,=\=,all,dontcare)]).

ctr_synonyms(eq,[xeqy]).

ctr_restrictions(eq, []).

ctr_pure_functional_dependency(eq, []).
ctr_functional_dependency(eq, 2, [1]).
ctr_functional_dependency(eq, 1, [2]).

ctr_predefined(eq).

ctr_example(eq,
            eq(8,8)).

ctr_see_also(eq,
 [link('negation',       neq,              '',                                         []),
  link('generalisation', eq_cst,           '%e added',                                 [constant]),
  link('generalisation', eq_set,           '%e replaced by %e',                        ['variable','set~variable']),
  link('generalisation', all_equal,        'equality between more than two variables', []),
  link('implies',        geq,              '',                                         []),
  link('implies',        leq,              '',                                         []),
  link('implies',        abs_value,        '',                                         []),
  link('implies',        same_sign,        '',                                         []),
  link('implies',        zero_or_not_zero, '',                                         []),
  link('common keyword', gt,               '%k,%k',                                    ['binary constraint', 'arithmetic constraint']),
  link('common keyword', lt,               '%k,%k',                                    ['binary constraint', 'arithmetic constraint'])]).

ctr_key_words(eq,['predefined constraint'     ,
                  'arithmetic constraint'     ,
                  'binary constraint'         ,
                  'functional dependency'     ,
		  'pure functional dependency',
                  'arc-consistency'           ]).

ctr_eval(eq, [checker(eq_c),
	      builtin(eq_b)]).

eq_c(VAR1, VAR2) :-
    check_type(int, VAR1),
    check_type(int, VAR2),
    VAR1 = VAR2.

eq_b(VAR1, VAR2) :-
    check_type(dvar, VAR1),
    check_type(dvar, VAR2),
    VAR1 #= VAR2.
