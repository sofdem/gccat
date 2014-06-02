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

ctr_date(dom_reachability,['20061011']).

ctr_origin(dom_reachability, '\\cite{QuesadaVanRoyDevilleCollet06}', []).

ctr_arguments(dom_reachability,
              ['SOURCE'-int                                               ,
               'FLOW_GRAPH'-collection(index-int, succ-svar)              ,
               'DOMINATOR_GRAPH'-collection(index-int, succ-sint)         ,
               'TRANSITIVE_CLOSURE_GRAPH'-collection(index-int, succ-svar)]).

ctr_exchangeable(dom_reachability,
                 [items('FLOW_GRAPH',all),
                  items('DOMINATOR_GRAPH',all),
                  items('TRANSITIVE_CLOSURE_GRAPH',all)]).

ctr_restrictions(dom_reachability,
                 ['SOURCE' >= 1                                                       ,
                  'SOURCE' =< size('FLOW_GRAPH')                                      ,
                  required('FLOW_GRAPH',[index,succ])                                 ,
                  'FLOW_GRAPH'^index >= 1                                             ,
                  'FLOW_GRAPH'^index =< size('FLOW_GRAPH')                            ,
                  'FLOW_GRAPH'^succ  >= 1                                             ,
                  'FLOW_GRAPH'^succ  =< size('FLOW_GRAPH')                            ,
                  distinct('FLOW_GRAPH',index)                                        ,
                  required('DOMINATOR_GRAPH',[index,succ])                            ,
                  size('DOMINATOR_GRAPH')= size('FLOW_GRAPH')                         ,
                  'DOMINATOR_GRAPH'^index >= 1                                        ,
                  'DOMINATOR_GRAPH'^index =< size('DOMINATOR_GRAPH')                  ,
                  'DOMINATOR_GRAPH'^succ  >= 1                                        ,
                  'DOMINATOR_GRAPH'^succ  =< size('DOMINATOR_GRAPH')                  ,
                  distinct('DOMINATOR_GRAPH',index)                                   ,
                  required('TRANSITIVE_CLOSURE_GRAPH',[index,succ])                   ,
                  size('TRANSITIVE_CLOSURE_GRAPH')=size('FLOW_GRAPH')                 ,
                  'TRANSITIVE_CLOSURE_GRAPH'^index >= 1                               ,
                  'TRANSITIVE_CLOSURE_GRAPH'^index =< size('TRANSITIVE_CLOSURE_GRAPH'),
                  'TRANSITIVE_CLOSURE_GRAPH'^succ  >= 1                               ,
                  'TRANSITIVE_CLOSURE_GRAPH'^succ  =< size('TRANSITIVE_CLOSURE_GRAPH'),
                  distinct('TRANSITIVE_CLOSURE_GRAPH',index)                          ]).

ctr_typical(dom_reachability,
            [size('FLOW_GRAPH') > 2]).

ctr_predefined(dom_reachability).

ctr_example(dom_reachability,
            dom_reachability(1,
                             [[index-1, succ-{2}      ],
                              [index-2, succ-{3,4}    ],
                              [index-3, succ-{}       ],
                              [index-4, succ-{}       ]],
                             [[index-1, succ-{2,3,4}  ],
                              [index-2, succ-{3,4}    ],
                              [index-3, succ-{}       ],
                              [index-4, succ-{}       ]],
                             [[index-1, succ-{1,2,3,4}],
                              [index-2, succ-{2,3,4}  ],
                              [index-3, succ-{3}      ],
                              [index-4, succ-{4}      ]])).

ctr_see_also(dom_reachability,
 [link('common keyword', path,         '%k', [path]),
  link('common keyword', path_from_to, '%k', [path])]).

ctr_key_words(dom_reachability,['predefined constraint'             ,
                                'graph constraint'                  ,
                                'path'                              ,
                                'constraint involving set variables']).

ctr_persons(dom_reachability,['Quesada L. O.'   ,
                              'Van Roy P.'      ,
                              'Deville Y.'      ,
                              'Collet R.'       ,
                              'Schulte C.'      ,
                              'Lagerkvist M. Z.',
                              'Tack G.'         ,
                              'Georgiadis L.'   ,
                              'Roditty L.'      ,
                              'Garey M. R.'     ,
                              'Johnson D. S.'   ]).
