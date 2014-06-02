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

ctr_date(open_global_cardinality,['20060824']).

ctr_origin(open_global_cardinality, '\\cite{HoeveRegin06}', []).

ctr_arguments(open_global_cardinality,
              ['S'-svar                                      ,
               'VARIABLES'-collection(var-dvar)              ,
               'VALUES'-collection(val-int, noccurrence-dvar)]).

ctr_exchangeable(open_global_cardinality,
                 [items('VALUES',all),
                  vals(['VARIABLES'^var],all(notin('VALUES'^val)),=,dontcare,dontcare)]).

ctr_synonyms(open_global_cardinality,[open_gcc, ogcc]).

ctr_restrictions(open_global_cardinality,
                 ['S' >= 1                                 ,
                  'S' =< size('VARIABLES')                 ,
                  required('VARIABLES',var)                ,
                  required('VALUES',[val,noccurrence])     ,
                  distinct('VALUES',val)                   ,
                  'VALUES'^noccurrence >= 0                ,
                  'VALUES'^noccurrence =< size('VARIABLES')]).

ctr_typical(open_global_cardinality,
            [size('VARIABLES')           > 1             ,
             range('VARIABLES'^var)      > 1             ,
             size('VALUES')              > 1             ,
             range('VALUES'^noccurrence) > 1             ,
             size('VARIABLES')           > size('VALUES')]).

ctr_graph(open_global_cardinality,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var = 'VALUES'^val,
           in_set(variables^key,'S')   ],
          ['NVERTEX' = 'VALUES'^noccurrence],
          []).

ctr_example(open_global_cardinality,
            open_global_cardinality({2,3,4},
                                    [[var-3],[var-3],[var-8],[var-6]],
                                    [[val-3, noccurrence-1],
                                     [val-5, noccurrence-0],
                                     [val-6, noccurrence-1]])).

ctr_draw_example(open_global_cardinality,
                 ['VARIABLES'],
                 [[[var-3],[var-3],[var-8],[var-6]]],
                 ['SELF'],
                 [2-2,4-4],
                 ['NVERTEX',
                  'FOREACH'('VALUES',[3-[2],5-[],6-[4]])],
                 '','3:NVERTEX=1, 5:NVERTEX=0, 6:NVERTEX=1',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(open_global_cardinality,
 [link('hard version',              global_cardinality,             '',                     []),
  link('common keyword',            global_cardinality_low_up,      '%k,%k',                ['assignment','counting constraint']),
  link('common keyword',            open_among,                     '%k,%k',                ['open constraint','counting constraint'], '\\\\ '),
  link('common keyword',            open_atleast,                   '%k,%k',                ['open constraint','value constraint']),
  link('common keyword',            open_atmost,                    '%k,%k',                ['open constraint','value constraint']),
  link('specialisation',            open_global_cardinality_low_up, '%e replaced by %e %e', [variable,fixed,interval]),
  link('specialisation',            open_alldifferent,              'each active value\\footnote{An \\emph{active value} corresponds to a value occuring at a position mentionned in the set $\\argument{S}$.} should occur at most once', []),
  link('used in graph description', in_set,                         '',                     [])]).

ctr_key_words(open_global_cardinality,['open constraint'                   ,
                                       'value constraint'                  ,
                                       'counting constraint'               ,
                                       'assignment'                        ,
                                       'constraint involving set variables',
                                       'flow'                              ]).

ctr_persons(open_global_cardinality,['R\\\'egin J.-C.',
                                     'van Hoeve W.-J.']).
