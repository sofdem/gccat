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

ctr_date(open_alldifferent,['20060824','20090524']).

ctr_origin(open_alldifferent, '\\cite{HoeveRegin06}', []).

ctr_arguments(open_alldifferent,
              ['S'-svar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(open_alldifferent,
                 [vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(open_alldifferent,[open_alldiff    ,
                                open_alldistinct,
                                open_distinct   ]).

ctr_restrictions(open_alldifferent,
                 ['S' >= 1                 ,
                  'S' =< size('VARIABLES') ,
                  required('VARIABLES',var)]).

ctr_typical(open_alldifferent,
            [size('VARIABLES') > 2]).

ctr_contractible(open_alldifferent, [], 'VARIABLES', suffix).

ctr_graph(open_alldifferent,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var,
           in_set(variables1^key,'S')     ,
           in_set(variables2^key,'S')     ],
          ['MAX_NSCC' =< 1],
	      ['ONE_SUCC']).

ctr_example(open_alldifferent,
            open_alldifferent({2,3,4},[[var-9],[var-1],[var-9],[var-3]])).

ctr_draw_example(open_alldifferent,
                 ['VARIABLES'],
                 [[[var-9],[var-1],[var-9],[var-3]]],
                 ['CLIQUE'],
                 [2-2,3-3,4-4],
                 ['MAX_NSCC'([2])],
                 '','MAX_NSCC=1',
                 []).

ctr_see_also(open_alldifferent,
 [link('hard version',   alldifferent,                       '',      []),
  link('common keyword', size_max_starting_seq_alldifferent, '%k,%k', ['all different', 'disequality']),
  link('common keyword', size_max_seq_alldifferent,          '%k,%k', ['all different', 'disequality']),
  link('generalisation', open_global_cardinality,            'control the number of occurrence of each active value\\footnote{An \\emph{active value} corresponds to a value occuring at a position mentionned in the set $\\argument{S}$.} with a counter variable', []),
  link('generalisation', open_global_cardinality_low_up,     'control the number of occurrence of each active value with an interval',        []),
  link('used in graph description', in_set, '', [])]).

ctr_key_words(open_alldifferent,['open constraint'                   ,
                                 'soft constraint'                   ,
                                 'value constraint'                  ,
                                 'all different'                     ,
                                 'disequality'                       ,
                                 'constraint involving set variables',
                                 'flow'                              ]).

ctr_persons(open_alldifferent,['R\\\'egin J.-C.',
                               'van Hoeve W.-J.']).
