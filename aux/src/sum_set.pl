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

ctr_date(sum_set,['20031001','20060818']).

ctr_origin(sum_set, 'H.~Cambazard', []).

ctr_arguments(sum_set,
              ['SV'-svar                             ,
               'VALUES'-collection(val-int, coef-int),
               'CTR'-atom                            ,
               'VAR'-dvar                            ]).

ctr_exchangeable(sum_set,
                 [items('VALUES',all)]).

ctr_restrictions(sum_set,
                 [required('VALUES',[val,coef])   ,
                  distinct('VALUES',val)          ,
                  'VALUES'^coef >= 0              ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])]).

ctr_typical(sum_set,
            [size('VALUES') > 1          ,
             'VALUES'^coef  > 0          ,
             in_list('CTR',[=,<,>=,>,=<])]).

ctr_graph(sum_set,
          ['VALUES'],
          1,
          ['SELF'>>collection(values)],
          [in_set(values^val,'SV')],
          ['CTR'('SUM'('VALUES',coef),'VAR')],
          []).

ctr_example(sum_set,
            sum_set({2,3,6},
                    [[val-2,coef-7],
                     [val-9,coef-1],
                     [val-5,coef-7],
                     [val-6,coef-2]],
                    =,
                    9)).

ctr_draw_example(sum_set,
                 ['VALUES'],
                 [[[val-2,coef-7],[val-9,coef-1],[val-5,coef-7],[val-6,coef-2]]],
                 ['SELF'],
                 [1-1,4-4],
                 ['SUM'([1,4])],
                 '','SUM=7+2=9',
                 [2.4,2.145,1.5,0.8]).

ctr_see_also(sum_set,
 [link('common keyword', sum,     '%k', ['sum']),
  link('common keyword', sum_ctr, '%k', ['sum'])]).

ctr_key_words(sum_set,['arithmetic constraint'             ,
                       'binary constraint'                 ,
                       'sum'                               ,
                       'constraint involving set variables']).

ctr_persons(sum_set,['Cambazard H.']).
