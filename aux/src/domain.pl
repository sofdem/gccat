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

ctr_date(domain,['20070821']).

ctr_origin(domain, 'Domain definition.', []).

ctr_arguments(domain,
              ['VARIABLES'-collection(var-dvar),
               'LOW'-int                       ,
               'UP'-int                        ]).

ctr_exchangeable(domain,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int(in('LOW','UP')),=\=,dontcare,dontcare),
                  vals(['LOW'],int,>,dontcare,dontcare),
                  vals(['UP'],int,<,dontcare,dontcare),
                  translate(['VARIABLES'^var,'LOW','UP'])]).

ctr_synonyms(domain,[dom]).

ctr_restrictions(domain, [required('VARIABLES',var),
                          'LOW' =< 'UP'            ]).

ctr_typical(domain,
            [size('VARIABLES') > 1   ,
             'LOW'             < 'UP']).

ctr_contractible(domain, [], 'VARIABLES', any).

ctr_predefined(domain).

ctr_example(domain,
            domain([[var-2],[var-8],[var-2]],1,9)).

ctr_see_also(domain,
 [link('common keyword',            in_interval, '%k', ['domain definition']),
  link('common keyword',            in,          '%k', ['domain definition']),
  link('uses in its reformulation', tree_range,  '',   [])]).

ctr_key_words(domain,['predefined constraint',
                      'value constraint'     ,
                      'interval'             ,
                      'domain definition'    ]).

ctr_eval(domain, [builtin(domain_b)]).

domain_b(VARIABLES, LOW, UP) :-
	check_type(int, LOW),
	check_type(int, UP),
	LOW =< UP,
	collection(VARIABLES, [fdvar(LOW,UP)]),
    get_attr1(VARIABLES, VARS),
	domain(VARS, LOW, UP).
