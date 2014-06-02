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

ctr_date(set_value_precede,['20041003']).

ctr_origin(set_value_precede, '\\cite{YatChiuLawJimmyLee04}', []).

ctr_arguments(set_value_precede,
              ['S'-int                         ,
               'T'-int                         ,
               'VARIABLES'-collection(var-svar)]).

ctr_restrictions(set_value_precede,
                 ['S' =\= 'T'              ,
                  required('VARIABLES',var)]).

ctr_typical(set_value_precede,
            ['S' < 'T'            ,
             size('VARIABLES') > 1]).

ctr_contractible(set_value_precede, [], 'VARIABLES', suffix).

ctr_predefined(set_value_precede).

ctr_example(set_value_precede,
            [set_value_precede(2, 1, [[var-{0,2}], [var-{0,1}], [var-{}], [var-{1}]]),
             set_value_precede(0, 1, [[var-{0,2}], [var-{0,1}], [var-{}], [var-{1}]]),
             set_value_precede(0, 2, [[var-{0,2}], [var-{0,1}], [var-{}], [var-{1}]]),
             set_value_precede(0, 4, [[var-{0,2}], [var-{0,1}], [var-{}], [var-{1}]])]).

ctr_see_also(set_value_precede,
 [link('specialisation', int_value_precede, '%e of %e replaced by %e of %e', [sequence,'set~variables',sequence,'domain~variables'])]).

ctr_key_words(set_value_precede,['order constraint'                  ,
                                 'symmetry'                          ,
                                 'indistinguishable values'          ,
                                 'value precedence'                  ,
                                 'constraint involving set variables']).

ctr_persons(set_value_precede,['Law Y. C.'   ,
                               'Lee J. H. M.']).
