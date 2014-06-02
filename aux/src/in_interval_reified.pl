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

ctr_date(in_interval_reified,['20100916']).

ctr_origin(in_interval_reified, 'Reified version of %c.', [in_interval]).

ctr_arguments(in_interval_reified,
              ['VAR'-dvar,
               'LOW'-int ,
               'UP'-int  ,
               'B'-dvar  ]).

ctr_exchangeable(in_interval_reified,
                 [vals(['VAR'],comp(in('LOW','UP')),=,dontcare,dontcare),
                  translate(['VAR','LOW','UP'])]).

ctr_synonyms(in_interval_reified,[dom_reified,in_reified]).

ctr_restrictions(in_interval_reified,
                 ['LOW' =< 'UP',
                  'B'   >= 0   ,
                  'B'   =< 1   ]).

ctr_typical(in_interval_reified,
            ['VAR' =\= 'LOW',
             'VAR' =\= 'UP' ,
             'LOW' <   'UP' ]).

ctr_predefined(in_interval_reified).

ctr_example(in_interval_reified,
            in_interval_reified(3, 2, 5, 1)).

ctr_see_also(in_interval_reified,
 [link('specialisation', in_interval, '', []),
  link('uses in its reformulation', alldifferent, 'bound consistency preserving reformulation', [])]).

ctr_key_words(in_interval_reified,['predefined constraint',
				   'value constraint'     ,
                                   'binary constraint'    ,
                                   'reified constraint'   ,
                                   'arc-consistency'      ]).

ctr_eval(in_interval_reified, [reformulation(in_interval_reified_r)]).

in_interval_reified_r(VAR, LOW, UP, B) :-
    check_type(dvar, VAR),
    check_type(int, LOW),
    check_type(int, UP),
    check_type(dvar(0,1), B),
    LOW =< UP,
    VAR in LOW..UP #<=> B.
