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

ctr_date(tour,['20030820','20060819']).

ctr_origin(tour, '\\cite{AlthausBockmayrElfKasperJungerMehlhorn02}', []).

ctr_arguments(tour,
              ['NODES'-collection(index-int, succ-svar)]).

ctr_exchangeable(tour,
                 [items('NODES',all)]).

ctr_synonyms(tour,[atour,cycle]).

ctr_restrictions(tour,
                 [size('NODES') >= 3            ,
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ]).

ctr_graph(tour,
          ['NODES'],
          2,
          ['CLIQUE'(=\=)>>collection(nodes1,nodes2)],
          [in_set(nodes2^index,nodes1^succ) #<=> in_set(nodes1^index,nodes2^succ)],
          ['NARC' = size('NODES')*size('NODES')-size('NODES')],
          []).

ctr_graph(tour,
          ['NODES'],
          2,
          ['CLIQUE'(=\=)>>collection(nodes1,nodes2)],
          [in_set(nodes2^index,nodes1^succ)],
          ['MIN_NSCC' = size('NODES'),
           'MIN_ID'   = 2            ,
           'MAX_ID'   = 2            ,
           'MIN_OD'   = 2            ,
           'MAX_OD'   = 2            ],
          []).

ctr_example(tour,
            tour([[index-1, succ-{2,4}],
                  [index-2, succ-{1,3}],
                  [index-3, succ-{2,4}],
                  [index-4, succ-{1,3}]])).

ctr_draw_example(tour,
                 ['NODES'],
                 [[[[index-1, succ-{2,3,4}],
                    [index-2, succ-{1,3,4}],
                    [index-3, succ-{1,2,4}],
                    [index-4, succ-{1,2,3}]]],
                  [[[index-1, succ-{2,4}],
                    [index-2, succ-{1,3}],
                    [index-3, succ-{2,4}],
                    [index-4, succ-{1,3}]]]],
                 ['CLIQUE'(=\=)],
                 [[1-[2,3,4],2-[1,3,4],3-[1,2,4],4-[1,2,3]],
                  [1-[2,4],2-[1,3],3-[2,4],4-[1,3]]],
                 [['COLLECTIONS'(['NODES'-[1,2,3,4]])],
                  ['MIN_NSCC'([1,2,3,4])]],
                 '','MIN_NSCC=4\\nMIN_ID=2\\nMAX_ID =2\\nMIN_OD=2\\nMAX_OD=2',
                 [2.145,2.145,2.9,2.5]).

ctr_see_also(tour,
 [link('common keyword',            circuit,              '%k,%k', ['graph partitioning constraint', 'Hamiltonian']),
  link('common keyword',            link_set_to_booleans, '%k',    ['constraint involving set variables']),
  link('common keyword',            cycle,                '%k',    ['graph constraint']),
  link('used in graph description', in_set,               '',      [])]).

ctr_key_words(tour,['graph constraint'                  ,
                    'undirected graph'                  ,
                    'Hamiltonian'                       ,
		    'DFS-bottleneck'                    ,
                    'linear programming'                ,
		    'matching'                          ,
                    'constraint involving set variables']).

ctr_persons(tour,['Althaus E.'  ,
                  'Bockmayr A.' ,
                  'Elf M.'      ,
                  'Kasper T.'   ,
                  'J\\"unger M.',
                  'Mehlhorn K.' ,
		  'Cymer R.'    ]).

ctr_application(tour, [1]).
