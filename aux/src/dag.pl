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

ctr_date(dag,['20061001']).

ctr_origin(dag, '\\cite{Dooms06}', []).

ctr_arguments(dag,
              ['NODES'-collection(index-int, succ-svar)]).

ctr_exchangeable(dag,
                 [items('NODES',all)]).

ctr_restrictions(dag,
                 [required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(dag,
            [size('NODES') > 2]).

ctr_graph(dag,
          ['NODES'],
          1,
          ['SELF'>>collection(nodes)],
          [in_set(nodes^key,nodes^succ)],
          ['NARC' = 0],
          []).

ctr_graph(dag,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [in_set(nodes2^index,nodes1^succ)],
          ['MAX_NSCC' =< 1],
          []).

ctr_example(dag,
            dag([[index-1, succ-{2,4}],
                 [index-2, succ-{3,4}],
                 [index-3, succ-{}   ],
                 [index-4, succ-{}   ],
                 [index-5, succ-{6}  ],
                 [index-6, succ-{}   ]])).

ctr_see_also(dag,
 [link('used in graph description', in_set, '', [])]).

ctr_draw_example(dag,
                 ['NODES'],
                 [[[[index-1, succ-{2,4}],
                    [index-2, succ-{3,4}],
                    [index-3, succ-{2,4}],
                    [index-4, succ-{1}  ],
                    [index-5, succ-{6}  ],
                    [index-6, succ-{5}  ]]],
                  [[[index-1, succ-{2,4}],
                    [index-2, succ-{3,4}],
                    [index-3, succ-{}   ],
                    [index-4, succ-{}   ],
                    [index-5, succ-{6}  ],
                    [index-6, succ-{}   ]]]],
                 ['CLIQUE'],
                 [[1-[2,4],2-[3,4],3-[2,4],4-[1],5-[6],6-[5]],
                  [1-[2,4],2-[3,4],              5-[6]     ]],
                 [['COLLECTIONS'(['NODES'-[1,2,3,4,5,6]])],[]],
                 '','',
                 [1.8,2.2,1.6,2]).

ctr_key_words(dag,['graph constraint'                  ,
                   'constraint involving set variables']).

ctr_persons(dag,['Dooms G.']).

ctr_application(dag, [1]).
