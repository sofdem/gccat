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

ctr_date(in_intervals,['20080610']).

ctr_origin(in_intervals, 'Domain definition.', []).

ctr_arguments(in_intervals,
              ['VAR'-dvar                             ,
               'INTERVALS'-collection(low-int, up-int)]).

ctr_exchangeable(in_intervals,
                 [items('INTERVALS',all),
                  vals(['INTERVALS'^low],int,>,dontcare,dontcare),
                  vals(['INTERVALS'^up],int,<,dontcare,dontcare),
                  translate(['VAR','INTERVALS'^low,'INTERVALS'^up])]).

ctr_synonyms(in_intervals,[in]).

ctr_restrictions(in_intervals,
                 [required('INTERVALS',[low,up])   ,
                  'INTERVALS'^low =< 'INTERVALS'^up,
                  size('INTERVALS') > 0            ]).

ctr_typical(in_intervals,
            [size('INTERVALS') > 1]).

ctr_extensible(in_intervals, [], 'INTERVALS', any).

ctr_predefined(in_intervals).

ctr_example(in_intervals,
            in_intervals(5,
                        [[low-1, up-1],
                         [low-3, up-5],
                         [low-8, up-8]])).

ctr_see_also(in_intervals,
 [link('specialisation', in_interval, 'set of intervals replaced by single interval', [])]).

ctr_key_words(in_intervals,['value constraint'     ,
                            'predefined constraint',
                            'unary constraint'     ,
                            'interval'             ,
                            'domain definition'    ,
                            'arc-consistency'      ]).

ctr_eval(in_intervals, [reformulation(in_intervals_r)]).

in_intervals_r(VAR, INTERVALS) :-
    check_type(dvar, VAR),
    collection(INTERVALS, [int,int]),
    length(INTERVALS, L),
    L > 0,
    get_attr1(INTERVALS, LOWS),
    get_attr2(INTERVALS, UPS),
    check_lesseq(LOWS, UPS),
    in_intervals1(LOWS, UPS, VAR, TERM),
    call(TERM).

in_intervals1([], [], _, 0).
in_intervals1([LOW|RLOW], [UP|RUP], VAR, (VAR#>=LOW #/\ VAR#=<UP) #\/ R) :-
    in_intervals1(RLOW, RUP, VAR, R).
