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

ctr_date(leq_cst,['20090912']).

ctr_origin(leq_cst, 'Arithmetic.', []).

ctr_arguments(leq_cst,
              ['VAR1'-dvar,
               'VAR2'-dvar,
               'CST2'-int ]).

ctr_exchangeable(leq_cst,
                 [args([['VAR1'],['VAR2','CST2']]),
                  vals(['VAR1'],int(=<('VAR2'+'CST2')),=\=,all,dontcare),
                  vals(['VAR2'],int(>=('VAR1'-'CST2')),=\=,all,dontcare),
                  vals(['CST2'],int(>=('VAR1'-'VAR2')),=\=,all,dontcare)]).

ctr_typical(leq_cst,
            ['CST2' =\= 0           ,
             'VAR1'  < 'VAR2'+'CST2']).

ctr_predefined(leq_cst).

ctr_example(leq_cst,
            leq_cst(5,2,4)).

ctr_see_also(leq_cst,
 [link('implied by',     eq_cst,   '',             []),
  link('implied by',     distance, '',             []),
  link('specialisation', leq,      '%e set to %e', [constant, 0]),
  link('common keyword', geq_cst,  '%k,%k',        ['binary constraint', 'arithmetic constraint'])]).

ctr_key_words(leq_cst,['predefined constraint',
                       'arithmetic constraint',
                       'binary constraint'    ,
                       'arc-consistency'      ,
                       'metro'                ]).

ctr_persons(leq_cst,['Simonis H.']).

ctr_eval(leq_cst, [builtin(leq_cst_b)]).

leq_cst_b(VAR1, VAR2, CST2) :-
    check_type(dvar, VAR1),
    check_type(dvar, VAR2),
    check_type(int,  CST2),
    VAR1 #=< VAR2+CST2.
