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

ctr_date(disj,['20070527']).

ctr_origin(disj, '\\cite{MonetteDevilleDupont07}', []).

ctr_arguments(disj,
              ['TASKS'-collection(start-dvar, duration-dvar, before-svar, position-dvar)]).

ctr_exchangeable(disj,
                 [translate(['TASKS'^start]),
                  vals(['TASKS'^duration],int(>=(1)),>,dontcare,dontcare)]).

ctr_restrictions(disj,
                 [required('TASKS',[start,duration,before,position]),
                  'TASKS'^duration >= 1                             ,
                  'TASKS'^position >= 0                             ,
                  'TASKS'^position < size('TASKS')                  ]).

ctr_typical(disj,
            [size('TASKS') > 1]).

ctr_graph(disj,
          ['TASKS'],
          2,
          ['CLIQUE'(=\=)>>collection(tasks1,tasks2)],
          [tasks1^start + tasks1^duration =< tasks2^start #\/ tasks2^start + tasks2^duration =< tasks1^start,
           tasks1^start + tasks1^duration =< tasks2^start #<=> in_set(tasks1^key,tasks2^before)             ,
           tasks1^start + tasks1^duration =< tasks2^start #<=> tasks1^position < tasks2^position            ],
          ['NARC' = size('TASKS')*size('TASKS')-size('TASKS')],
          []).

ctr_example(disj,
            disj([[start-1, duration-3, before-{}     , position-0],
                  [start-9, duration-1, before-{1,3,4}, position-3],
                  [start-7, duration-2, before-{1,4}  , position-2],
                  [start-4, duration-1, before-{1}    , position-1]])).

ctr_draw_example(disj,
                 ['TASKS'],
                 [[[start-1, duration-3, before-{}     , position-0],
                   [start-9, duration-1, before-{1,3,4}, position-3],
                   [start-7, duration-2, before-{1,4}  , position-2],
                   [start-4, duration-1, before-{1}    , position-1]]],
                 ['CLIQUE'(<)],
                 [1-[2,3,4],2-[1,3,4],3-[1,2,4],4-[1,2,3]],
                 ['NARC'],
                 '','NARC=12',
                 []).

ctr_see_also(disj,
 [link('common keyword',            disjunctive,  '%k', ['scheduling constraint']),
  link('used in graph description', in_set,       '',   [])]).

ctr_key_words(disj,['scheduling constraint'                      ,
                    'resource constraint'                        ,
                    'decomposition'                              ,
                    'sequencing with release times and deadlines',
		    'constraint involving set variables'         ]).

ctr_persons(disj,['Monette J.-N.',
                  'Deville Y.'   ,
                  'Dupont P.'    ]).

ctr_application(disj, [1]).
