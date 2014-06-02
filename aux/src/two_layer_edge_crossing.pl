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

ctr_date(two_layer_edge_crossing,['20030820','20060819']).

ctr_origin(two_layer_edge_crossing, 'Inspired by \\cite{HararySchwenk72}.', []).

ctr_arguments(two_layer_edge_crossing,
              ['NCROSS'-dvar                                       ,
               'VERTICES_LAYER1'-collection(id-int, pos-dvar)      ,
               'VERTICES_LAYER2'-collection(id-int, pos-dvar)      ,
               'EDGES'-collection(id-int, vertex1-int, vertex2-int)]).

ctr_exchangeable(two_layer_edge_crossing,
                 [args([['NCROSS'],['VERTICES_LAYER1','VERTICES_LAYER2'],['EDGES']]),
                  items('VERTICES_LAYER1',all),
                  items('VERTICES_LAYER2',all)]).

ctr_restrictions(two_layer_edge_crossing,
                 ['NCROSS' >= 0                                  ,
                  required('VERTICES_LAYER1',[id,pos])           ,
                  'VERTICES_LAYER1'^id >= 1                      ,
                  'VERTICES_LAYER1'^id =< size('VERTICES_LAYER1'),
                  distinct('VERTICES_LAYER1',id)                 ,
                  distinct('VERTICES_LAYER1',pos)                ,
                  required('VERTICES_LAYER2',[id,pos])           ,
                  'VERTICES_LAYER2'^id >= 1                      ,
                  'VERTICES_LAYER2'^id =< size('VERTICES_LAYER2'),
                  distinct('VERTICES_LAYER2',id)                 ,
                  distinct('VERTICES_LAYER2',pos)                ,
                  required('EDGES',[id,vertex1,vertex2])         ,
                  'EDGES'^id >= 1                                ,
                  'EDGES'^id =< size('EDGES')                    ,
                  distinct('EDGES',id)                           ,
                  'EDGES'^vertex1 >= 1                           ,
                  'EDGES'^vertex1 =< size('VERTICES_LAYER1')     ,
                  'EDGES'^vertex2 >= 1                           ,
                  'EDGES'^vertex2 =< size('VERTICES_LAYER2')     ]).

ctr_typical(two_layer_edge_crossing,
            [size('VERTICES_LAYER1') >  1,
             size('VERTICES_LAYER2') >  1,
             size('EDGES')           >= size('VERTICES_LAYER1'),
             size('EDGES')           >= size('VERTICES_LAYER2')]).

ctr_pure_functional_dependency(two_layer_edge_crossing, []).
ctr_functional_dependency(two_layer_edge_crossing, 1, [2,3,4]).

ctr_derived_collections(two_layer_edge_crossing,
    [col('EDGES_EXTREMITIES'-collection(layer1-dvar, layer2-dvar),
         [item(layer1-'EDGES'^vertex1('VERTICES_LAYER1',pos,id),
               layer2-'EDGES'^vertex2('VERTICES_LAYER2',pos,id))])]).

ctr_graph(two_layer_edge_crossing,
          ['EDGES_EXTREMITIES'],
          2,
          ['CLIQUE'(<)>>collection(edges_extremities1,edges_extremities2)],
          [(edges_extremities1^layer1 < edges_extremities2^layer1 #/\
            edges_extremities1^layer2 > edges_extremities2^layer2)    #\/
           (edges_extremities1^layer1 > edges_extremities2^layer1 #/\
            edges_extremities1^layer2 < edges_extremities2^layer2)],
          ['NARC' = 'NCROSS'],
          []).

ctr_example(two_layer_edge_crossing,
            two_layer_edge_crossing(2,
                                    [[id-1, pos-1],
                                     [id-2, pos-2]],
                                    [[id-1, pos-3],
                                     [id-2, pos-1],
                                     [id-3, pos-2]],
                                    [[id-1, vertex1-2, vertex2-2],
                                     [id-2, vertex1-2, vertex2-3],
                                     [id-3, vertex1-1, vertex2-1]])).

ctr_draw_example(two_layer_edge_crossing,
                 ['EDGES_EXTREMITIES'],
                 [[[layer1-2, layer2-1],
                   [layer1-2, layer2-2],
                   [layer1-1, layer2-3]]],
                 ['CLIQUE'(<)],
                 [ 3-[1,2]],
                 ['NARC'],
                 '','NARC=2',
                 [1.4,1.4,1.2,1.2]).

ctr_see_also(two_layer_edge_crossing,
 [link('common keyword', crossing,       '%k', ['line segments intersection']),
  link('common keyword', graph_crossing, '%k', ['line segments intersection'])]).

ctr_key_words(two_layer_edge_crossing,['geometrical constraint'    ,
                                       'line segments intersection',
                                       'derived collection'        ,
                                       'functional dependency'     ,
		                       'pure functional dependency',
                                       'obscure'                   ]).

ctr_persons(two_layer_edge_crossing,['Harary F.'     ,
                                     'Schwenk A. J.' ,
                                     'Di Battista G.',
                                     'Eades P.'      ,
                                     'Tamassia R.'   ,
                                     'Tollis I. G.'  ,
                                     'Garey M. R.'   ,
                                     'Johnson D. S.' ]).
