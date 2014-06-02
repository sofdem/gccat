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

ctr_date(k_cut,['20030820','20041230','20060811']).

ctr_origin(k_cut, 'E.~Althaus', []).

ctr_arguments(k_cut,
              ['K'-int,
               'NODES'-collection(index-int, succ-svar)]).

ctr_exchangeable(k_cut,
                 [vals(['K'],int(>=(1)),>,dontcare,dontcare),
                  items('NODES',all)]).

ctr_restrictions(k_cut,
                 ['K' >= 1                      ,
                  'K' =< size('NODES')          ,
                  required('NODES',[index,succ]),
                  'NODES'^index >= 1            ,
                  'NODES'^index =< size('NODES'),
                  distinct('NODES',index)       ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(k_cut,
            [size('NODES') > 1]).

ctr_graph(k_cut,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^index=nodes2^index #\/ in_set(nodes2^index,nodes1^succ)],
          ['NCC' >= 'K'],
          []).

ctr_example(k_cut,
            k_cut(3,
                  [[index-1, succ-{   }],
                   [index-2, succ-{3,5}],
                   [index-3, succ-{5  }],
                   [index-4, succ-{   }],
                   [index-5, succ-{2,3}]])).

ctr_draw_example(k_cut,
                 ['NODES'],
                 [[[[index-1, succ-{2,4  }],
                    [index-2, succ-{1,3,5}],
                    [index-3, succ-{2,5  }],
                    [index-4, succ-{1,5  }],
                    [index-5, succ-{2,3,4}]]],
                  [[[index-1, succ-{   }],
                    [index-2, succ-{3,5}],
                    [index-3, succ-{5  }],
                    [index-4, succ-{   }],
                    [index-5, succ-{2,3}]]]],
                 ['CLIQUE'],
                 [[1-[1,2,4],
                   2-[1,2,3,5],
                   3-[2,3,5],
                   4-[1,4,5],
                   5-[2,3,4,5]],
                  [1-1,
                   2-[2,3,5],
                   3-[3,5],
                   4-[4],
                   5-[2,3,5]]],
                 [['COLLECTIONS'(['NODES'-[1,2,3,4,5]])],
                  ['NCC'([[1],[2,3,5],[4]])]],
                 '','NCC=3',
                 [2.145,2.3,2.5,2.32]).

ctr_see_also(k_cut,
 [link('common keyword',            link_set_to_booleans, '%k', ['constraint involving set variables']),
  link('used in graph description', in_set,               '',   [])]).

ctr_key_words(k_cut,['graph constraint'                  ,
                     'linear programming'                ,
                     'connected component'               ,
                     'constraint involving set variables']).

ctr_persons(k_cut,['Althaus E.']).
