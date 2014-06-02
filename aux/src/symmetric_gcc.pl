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

ctr_date(symmetric_gcc,['20030820','20040530','20060818']).

ctr_origin(symmetric_gcc, 'Derived from %c by W.~Kocjan.', [global_cardinality]).

ctr_arguments(symmetric_gcc,
              ['VARS'-collection(idvar-int, var-svar, nocc-dvar),
               'VALS'-collection(idval-int, val-svar, nocc-dvar)]).

ctr_exchangeable(symmetric_gcc,
                 [items('VARS',all),
                  items('VALS',all)]).

ctr_synonyms(symmetric_gcc,[sgcc]).

ctr_restrictions(symmetric_gcc,
                 [required('VARS',[idvar,var,nocc]),
                  size('VARS') >= 1                ,
                  'VARS'^idvar >= 1                ,
                  'VARS'^idvar =< size('VARS')     ,
                  distinct('VARS',idvar)           ,
                  'VARS'^nocc >= 0                 ,
                  'VARS'^nocc =< size('VALS')      ,
                  required('VALS',[idval,val,nocc]),
                  size('VALS') >= 1                ,
                  'VALS'^idval >= 1                ,
                  'VALS'^idval =< size('VALS')     ,
                  distinct('VALS',idval)           ,
                  'VALS'^nocc >= 0                 ,
                  'VALS'^nocc =< size('VARS')      ]).

ctr_typical(symmetric_gcc,
            [size('VARS') > 1,
             size('VALS') > 1]).

ctr_graph(symmetric_gcc,
          ['VARS','VALS'],
          2,
          ['PRODUCT'>>collection(vars,vals)],
          [in_set(vars^idvar,vals^val) #<=> in_set(vals^idval,vars^var),
           vars^nocc = card_set(vars^var)                              ,
           vals^nocc = card_set(vals^val)                              ],
          ['NARC' = size('VARS')*size('VALS')],
          []).

ctr_example(symmetric_gcc,
            symmetric_gcc([[idvar-1, var-{3}    , nocc-1],
                           [idvar-2, var-{1}    , nocc-1],
                           [idvar-3, var-{1,2}  , nocc-2],
                           [idvar-4, var-{1,3}  , nocc-2]],
                          [[idval-1, val-{2,3,4}, nocc-3],
                           [idval-2, val-{3}    , nocc-1],
                           [idval-3, val-{1,4}  , nocc-2],
                           [idval-4, val-{}     , nocc-0]])).

ctr_draw_example(symmetric_gcc,
                 ['VARS','VALS'],
                 [[[idvar-1, var-{3}    , nocc-1],
                   [idvar-2, var-{1}    , nocc-1],
                   [idvar-3, var-{1,2}  , nocc-2],
                   [idvar-4, var-{1,3}  , nocc-2]],
                  [[idval-1, val-{2,3,4}, nocc-3],
                   [idval-2, val-{3}    , nocc-1],
                   [idval-3, val-{1,4}  , nocc-2],
                   [idval-4, val-{}     , nocc-0]]],
                 ['PRODUCT'],
                 [1-[1,2,3,4],
                  2-[1,2,3,4],
                  3-[1,2,3,4],
                  4-[1,2,3,4]],
                 ['NARC'],
                 '','NARC=16',
                 [1.7,2.145,3.3,2.145]).

ctr_see_also(symmetric_gcc,
 [link('specialisation',            symmetric_cardinality, '%e replaced by %e %e', [variable,fixed,interval]),
  link('root concept',              global_cardinality,    '',                     []),
  link('common keyword',            link_set_to_booleans,  '%k',                   ['constraint involving set variables']),
  link('used in graph description', in_set,                '',                     [])]).

ctr_key_words(symmetric_gcc,['decomposition'                     ,
                             'timetabling constraint'            ,
                             'assignment'                        ,
                             'relation'                          ,
                             'flow'                              ,
                             'constraint involving set variables']).

ctr_persons(symmetric_gcc,['Kocjan W.' ,
                           'Kreuger P.']).
