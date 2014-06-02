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

ctr_date(atmost1,['20061003']).

ctr_origin(atmost1, '\\cite{SadlerGervet01}', []).

ctr_arguments(atmost1,
              ['SETS'-collection(s-svar, c-int)]).

ctr_exchangeable(atmost1,
                 [items('SETS',all),
                  vals(['SETS'^s],int,=\=,all,dontcare)]).

ctr_synonyms(atmost1,[pair_atmost1]).

ctr_restrictions(atmost1,
                 [required('SETS',[s,c]),
                  'SETS'^c >= 1         ]).

ctr_typical(atmost1,
            [size('SETS') > 1]).

ctr_contractible(atmost1, [], 'SETS', any).

ctr_predefined(atmost1).

ctr_example(atmost1,
            atmost1([[s-{5,8}  ,c-2],
                     [s-{5}    ,c-1],
                     [s-{5,6,7},c-3],
                     [s-{1,4}  ,c-2]])).

ctr_key_words(atmost1,['predefined constraint'             ,
                       'constraint involving set variables',
                       'bound-consistency'                 ]).

ctr_persons(atmost1,['Sadler A. J.'   ,
                     'Gervet C.'      ,
                     'van Hoeve W.-J.',
                     'Sabharwal A.'   ]).
