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

ctr_date(subgraph_isomorphism,['20090821']).

ctr_origin(subgraph_isomorphism, '\\cite{Gregor79}', []).

ctr_arguments(subgraph_isomorphism,
              ['NODES_PATTERN'-collection(index-int, succ-sint),
               'NODES_TARGET'-collection(index-int, succ-svar) ,
               'FUNCTION'-collection(image-dvar)               ]).

ctr_exchangeable(subgraph_isomorphism,
                 [items('NODES_PATTERN',all),
                  items('NODES_TARGET',all)]).

ctr_restrictions(subgraph_isomorphism,
                 [required('NODES_PATTERN',[index,succ])        ,
                  'NODES_PATTERN'^index >= 1                    ,
                  'NODES_PATTERN'^index =< size('NODES_PATTERN'),
                  distinct('NODES_PATTERN',index)               ,
                  'NODES_PATTERN'^succ  >= 1                    ,
                  'NODES_PATTERN'^succ  =< size('NODES_PATTERN'),
                  required('NODES_TARGET',[index,succ])         ,
                  'NODES_TARGET'^index >= 1                     ,
                  'NODES_TARGET'^index =< size('NODES_TARGET')  ,
                  distinct('NODES_TARGET',index)                ,
                  'NODES_TARGET'^succ  >= 1                     ,
                  'NODES_TARGET'^succ  =< size('NODES_TARGET')  ,
                  required('FUNCTION',[image])                  ,
                  'FUNCTION'^image >= 1                         ,
                  'FUNCTION'^image =< size('NODES_TARGET')      ,
                  distinct('FUNCTION',image)                    ,
                  size('FUNCTION') = size('NODES_PATTERN')      ]).

ctr_typical(subgraph_isomorphism,
            [size('NODES_PATTERN') > 1,
             size('NODES_TARGET')  > 1]).

ctr_predefined(subgraph_isomorphism).

ctr_example(subgraph_isomorphism,
            subgraph_isomorphism([[index-1, succ-{2,4}  ],
                                  [index-2, succ-{1,3,4}],
                                  [index-3, succ-{}     ],
                                  [index-4, succ-{}     ]],
                                 [[index-1, succ-{}     ],
                                  [index-2, succ-{3,4,5}],
                                  [index-3, succ-{}     ],
                                  [index-4, succ-{2,5}  ],
                                  [index-5, succ-{}     ]],
                                 [[image-4              ],
                                  [image-2              ],
                                  [image-3              ],
                                  [image-5              ]])).

ctr_see_also(subgraph_isomorphism,
 [link('related', graph_isomorphism, '', [])]).

ctr_key_words(subgraph_isomorphism,['predefined constraint'             ,
                                    'graph constraint'                  ,
                                    'constraint involving set variables',
                                    'symmetry'                          ]).

ctr_persons(subgraph_isomorphism,['McGregor J. J.' ,
                                  'Ullmann J. R.'  ,
                                  'R\\\'egin J.-C.',
                                  'Larrosa J.'     ,
                                  'Valiente G.'    ,
                                  'Zampelli S.'    ,
                                  'Deville Y.'     ,
                                  'Solnon C.'      ,
                                  'Sorlin S.'      ,
                                  'Dupont P.'      ,
                                  'Puget J.-F.'    ]).
