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

ctr_date(stable_compatibility,['20070601']).

ctr_origin(stable_compatibility, 'P.~Flener, \\cite{BeldiceanuFlenerLorca06}', []).

ctr_arguments(stable_compatibility,
              ['NODES'-collection(index-int,father-dvar,prec-sint,inc-sint)]).

ctr_exchangeable(stable_compatibility,
                 [items('NODES',all)]).

ctr_restrictions(stable_compatibility,
                 [required('NODES',[index,father,prec,inc]),
                  'NODES'^index           >= 1             ,
                  'NODES'^index           =< size('NODES') ,
                  distinct('NODES',index)                  ,
                  'NODES'^father          >= 1             ,
                  'NODES'^father          =< size('NODES') ,
                  'NODES'^prec            >= 1             ,
                  'NODES'^prec            =< size('NODES') ,
                  'NODES'^inc             >= 1             ,
                  'NODES'^inc             =< size('NODES') ,
                  'NODES'^inc             >  'NODES'^index ]).

ctr_typical(stable_compatibility,
            [size('NODES')         > 2,
             range('NODES'^father) > 1]).

ctr_graph(stable_compatibility,
          ['NODES'],
          2,
          ['CLIQUE'>>collection(nodes1,nodes2)],
          [nodes1^father = nodes2^index],
          ['MAX_NSCC'                        =< 1,
           'NCC'                             =  1,
           'MAX_ID'                          =< 2,
           'PATH_FROM_TO'(index,index,prec ) =  1,
           'PATH_FROM_TO'(index,index,inc  ) =  0,
           'PATH_FROM_TO'(index,inc  ,index) =  0],
          []).

ctr_example(stable_compatibility,
            stable_compatibility([[index-1 , father-4, prec-{11,12}, inc-{}                ],
                                  [index-2 , father-3, prec-{ 8, 9}, inc-{}                ],
                                  [index-3 , father-4, prec-{ 2,10}, inc-{}                ],
                                  [index-4 , father-5, prec-{ 1, 3}, inc-{}                ],
                                  [index-5 , father-7, prec-{ 4,13}, inc-{}                ],
                                  [index-6 , father-2, prec-{ 8,14}, inc-{}                ],
                                  [index-7 , father-7, prec-{ 6,13}, inc-{}                ],
                                  [index-8 , father-6, prec-{}     , inc-{9,10,11,12,13,14}],
                                  [index-9 , father-2, prec-{}     , inc-{10,11,12,13}     ],
                                  [index-10, father-3, prec-{}     , inc-{11,12,13}        ],
                                  [index-11, father-1, prec-{}     , inc-{12,13}           ],
                                  [index-12, father-1, prec-{}     , inc-{13}              ],
                                  [index-13, father-5, prec-{}     , inc-{14}              ],
                                  [index-14, father-6, prec-{}     , inc-{}                ]])).

ctr_draw_example(stable_compatibility,
                 ['NODES'],
                 [[[index-1 , father-4, prec-{11,12}, inc-{}                ],
                   [index-2 , father-3, prec-{ 8, 9}, inc-{}                ],
                   [index-3 , father-4, prec-{ 2,10}, inc-{}                ],
                   [index-4 , father-5, prec-{ 1, 3}, inc-{}                ],
                   [index-5 , father-7, prec-{ 4,13}, inc-{}                ],
                   [index-6 , father-2, prec-{ 8,14}, inc-{}                ],
                   [index-7 , father-7, prec-{ 6,13}, inc-{}                ],
                   [index-8 , father-6, prec-{}     , inc-{9,10,11,12,13,14}],
                   [index-9 , father-2, prec-{}     , inc-{10,11,12,13}     ],
                   [index-10, father-3, prec-{}     , inc-{11,12,13}        ],
                   [index-11, father-1, prec-{}     , inc-{12,13}           ],
                   [index-12, father-1, prec-{}     , inc-{13}              ],
                   [index-13, father-5, prec-{}     , inc-{14}              ],
                   [index-14, father-6, prec-{}     , inc-{}                ]]],
                 ['CLIQUE'],
                 [1-6,2-2,3-6,4-5,5-1,6-7,7-2,8-1,9-4,10-7,11-4,12-5,13-3,14-3],
                 [],
                 '','MAX_NSCC=1,NCC=1,NVERTEX=14',
                 [4.5,3.5,5,3.5]).

ctr_see_also(stable_compatibility,
 [link('root concept', tree, '', [])]).

ctr_key_words(stable_compatibility,['graph constraint',
                                    'tree'            ,
                                    'bioinformatics'  ,
                                    'phylogeny'       ]).

ctr_persons(stable_compatibility,['Beldiceanu N.'       ,
                                  'Flener P.'           ,
                                  'Lorca X.'            ,
                                  'Bininda-Emonds O. R.',
                                  'Gittleman J. L.'     ,
                                  'Steel M. A.'         ,
                                  'Ng M. P.'            ,
                                  'Wormald N. C.'       ,
                                  'Steel M.'            ,
                                  'Aho A. V.'           ,
                                  'Sagiv Y.'            ,
                                  'Szymanski T.'        ,
                                  'Ullman J. D.'        ,
                                  'Gent I. P.'          ,
                                  'Prosser P.'          ,
                                  'Smith B. M.'         ,
                                  'Wei W.'              ]).

ctr_application(stable_compatibility, [1]).
