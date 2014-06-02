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

ctr_date(inverse_set,['20041211','20060810']).

ctr_origin(inverse_set, 'Derived from %c.', [inverse]).

ctr_arguments(inverse_set,
              ['X'-collection(index-int, set-svar),
	         'Y'-collection(index-int, set-svar)]).

ctr_exchangeable(inverse_set,
                 [args([['X','Y']]),
                  items('X',all),
                  items('Y',all)]).

ctr_restrictions(inverse_set,
                 [required('X',[index,set]),
                  required('Y',[index,set]),
                  increasing_seq('X',index),
                  increasing_seq('Y',index),
                  'X'^index >= 1           ,
                  'X'^index =< size('X')   ,
                  'Y'^index >= 1           ,
                  'Y'^index =< size('Y')   ,
                  'X'^set   >= 1           ,
                  'X'^set   =< size('Y')   ,
                  'Y'^set   >= 1           ,
                  'Y'^set   =< size('X')   ]).

ctr_typical(inverse_set,
            [size('X') > 1,
             size('Y') > 1]).

ctr_graph(inverse_set,
          ['X','Y'],
          2,
          ['PRODUCT'>>collection(x,y)],
          [in_set(y^index,x^set) #<=> in_set(x^index,y^set)],
          ['NARC' = size('X') * size('Y')],
          []).

ctr_example(inverse_set,
            inverse_set([[index-1, set-{2,4}  ],
                         [index-2, set-{4}    ],
                         [index-3, set-{1}    ],
                         [index-4, set-{4}    ]],
                        [[index-1, set-{3}    ],
                         [index-2, set-{1}    ],
                         [index-3, set-{}     ],
                         [index-4, set-{1,2,4}],
                         [index-5, set-{}     ]])).

ctr_draw_example(inverse_set,
                 ['X','Y'],
                 [[[index-1, set-{2,4}  ],
                   [index-2, set-{4}    ],
                   [index-3, set-{1}    ],
                   [index-4, set-{4}    ]],
                  [[index-1, set-{3}    ],
                   [index-2, set-{1}    ],
                   [index-3, set-{}     ],
                   [index-4, set-{1,2,4}],
                   [index-5, set-{}     ]]],
                 ['PRODUCT'],
                 [1-[1,2,3,4,5],
                  2-[1,2,3,4,5],
                  3-[1,2,3,4,5],
                  4-[1,2,3,4,5]],
                 ['NARC'],
                 '','NARC=20',
                 [2,2.145,2.6,2.145]).

ctr_see_also(inverse_set,
 [link('specialisation',            inverse,              '%e %e replaced by %e', [set, variable, 'domain~variable']),
  link('common keyword',            inverse_within_range, '%k',                   ['channelling constraint']),
  link('used in graph description', in_set,               '',                     [])]).

ctr_key_words(inverse_set,['channelling constraint'            ,
                           'set channel'                       ,
                           'dual model'                        ,
                           'constraint involving set variables']).
