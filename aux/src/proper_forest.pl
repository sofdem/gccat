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

ctr_date(proper_forest,['20050604','20060813']).

ctr_origin(proper_forest, 'Derived from %c, \\cite{BeldiceanuKatrielLorca06}.', [tree]).

ctr_arguments(proper_forest,
              ['NTREES'-dvar                                ,
               'NODES'-collection(index-int, neighbour-svar)]).

ctr_exchangeable(proper_forest,
                 [items('NODES',all)]).

ctr_restrictions(proper_forest,
                 ['NTREES'           >= 0            ,
                  required('NODES',[index,neighbour]),
                  size('NODES') mod 2 = 0            ,
                  'NODES'^index      >= 1            ,
                  'NODES'^index      =< size('NODES'),
                  distinct('NODES',index)            ,
                  'NODES'^neighbour  >= 1            ,
                  'NODES'^neighbour  =< size('NODES'),
                  'NODES'^neighbour =\= 'NODES'^index]).

ctr_typical(proper_forest,
            ['NTREES'      > 0,
             size('NODES') > 1]).

ctr_functional_dependency(proper_forest, 1, [2]).

ctr_graph(proper_forest,
          ['NODES'],
          2,
          ['CLIQUE'(=\=)>>collection(nodes1,nodes2)],
          [in_set(nodes2^index,nodes1^neighbour)],
          ['NVERTEX' = ('NARC'+ 2*'NTREES')/2,
           'NCC'     = 'NTREES'              ,
           'NVERTEX' = size('NODES')         ],
          ['SYMMETRIC']).

ctr_example(proper_forest,
            proper_forest(3,
                          [[index-1 , neighbour-{3,6}  ],
                           [index-2 , neighbour-{9}    ],
                           [index-3 , neighbour-{1,5,7}],
                           [index-4 , neighbour-{9}    ],
                           [index-5 , neighbour-{3}    ],
                           [index-6 , neighbour-{1}    ],
                           [index-7 , neighbour-{3}    ],
                           [index-8 , neighbour-{10}   ],
                           [index-9 , neighbour-{2,4}  ],
                           [index-10, neighbour-{8}    ]])).

ctr_draw_example(proper_forest,
                 ['NODES'],
                 [[[index-1 , neighbour-{3,6}  ],
                   [index-2 , neighbour-{9}    ],
                   [index-3 , neighbour-{1,5,7}],
                   [index-4 , neighbour-{9}    ],
                   [index-5 , neighbour-{3}    ],
                   [index-6 , neighbour-{1}    ],
                   [index-7 , neighbour-{3}    ],
                   [index-8 , neighbour-{10}   ],
                   [index-9 , neighbour-{2,4}  ],
                   [index-10, neighbour-{8}    ]]],
                 ['CLIQUE'(=\=)],
                 [ 1-[3,6],
                   2-[9],
                   3-[1,5,7],
                   4-[9],
                   5-[3],
                   6-[1],
                   7-[3],
                   8-[10],
                   9-[2,4],
                  10-[8]],
                 ['NARC','NVERTEX','NCC'([[1,3,5,6,7],[2,4,9],[8,10]])],
                 '',
		 'CC#1:NARC=8,NVERTEX=5\\nCC#2:NARC=4,NVERTEX=3\\nCC#3:NARC=2,NVERTEX=2\\nNCC=3',
                 [2.5,2.145,3.8,2.2]).

ctr_see_also(proper_forest,
 [link('common keyword', tree, '%k,%k', ['connected component','tree'])]).

ctr_key_words(proper_forest,['graph constraint'                  ,
                             'undirected graph'                  ,
                             'connected component'               ,
                             'tree'                              ,
                             'no cycle'                          ,
                             'symmetric'                         ,
                             'constraint involving set variables',
                             'hybrid-consistency'                ,
                             'functional dependency'             ]).

ctr_persons(proper_forest,['Beldiceanu N.',
                           'Katriel I.'   ,
                           'Lorca X.'     ,
                           'Cayley A.'    ]).

ctr_application(proper_forest, [2]).
