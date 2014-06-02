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

ctr_date(graph_crossing,['20000128','20030820','20040530','20060809']).

ctr_origin(graph_crossing, 'N.~Beldiceanu', []).

ctr_arguments(graph_crossing,
              ['NCROSS'-dvar                              ,
               'NODES'-collection(succ-dvar, x-int, y-int)]).

ctr_exchangeable(graph_crossing,
                 [attrs_sync('NODES',[[succ],[x,y]]),
                  translate(['NODES'^x]),
                  translate(['NODES'^y])]).

ctr_synonyms(graph_crossing,[crossing,
                             ncross  ]).

ctr_restrictions(graph_crossing,
                 ['NCROSS'      >= 0            ,
                  required('NODES',[succ,x,y])  ,
                  'NODES'^succ  >= 1            ,
                  'NODES'^succ  =< size('NODES')]).

ctr_typical(graph_crossing,
            [size('NODES')       > 1,
             range('NODES'^succ) > 1,
             range('NODES'^x)    > 1,
             range('NODES'^y)    > 1]).

ctr_pure_functional_dependency(graph_crossing, []).
ctr_functional_dependency(graph_crossing, 1, [2]).

ctr_graph(graph_crossing,
          ['NODES'],
          2,
          ['CLIQUE'(<)>>collection(n1,n2)],
          [max(n1^x,'NODES'@(n1^succ)^x) >= min(n2^x,'NODES'@(n2^succ)^x),
           max(n2^x,'NODES'@(n2^succ)^x) >= min(n1^x,'NODES'@(n1^succ)^x),
           max(n1^y,'NODES'@(n1^succ)^y) >= min(n2^y,'NODES'@(n2^succ)^y),
           max(n2^y,'NODES'@(n2^succ)^y) >= min(n1^y,'NODES'@(n1^succ)^y),
           (n2^x-'NODES'@(n1^succ)^x)*('NODES'@(n1^succ)^y-n1^y)-
           ('NODES'@(n1^succ)^x-n1^x)*(n2^y-'NODES'@(n1^succ)^y) =\= 0   ,
           ('NODES'@(n2^succ)^x-'NODES'@(n1^succ)^x)*(n2^y-n1^y)-
           (n2^x-n1^x)*('NODES'@(n2^succ)^y-'NODES'@(n1^succ)^y) =\= 0   ,
           sign((n2^x-'NODES'@(n1^succ)^x)*('NODES'@(n1^succ)^y-n1^y)-
                ('NODES'@(n1^succ)^x-n1^x)*(n2^y-'NODES'@(n1^succ)^y)) =\=
           sign(('NODES'@(n2^succ)^x-'NODES'@(n1^succ)^x)*(n2^y-n1^y)-
                (n2^x-n1^x)*('NODES'@(n2^succ)^y-'NODES'@(n1^succ)^y))   ],
          ['NARC' = 'NCROSS'],
          []).

ctr_example(graph_crossing,
            graph_crossing(2,[[succ-1 , x-4 , y-7],
                              [succ-1 , x-2 , y-5],
                              [succ-1 , x-7 , y-6],
                              [succ-2 , x-1 , y-2],
                              [succ-3 , x-2 , y-2],
                              [succ-2 , x-5 , y-3],
                              [succ-3 , x-8 , y-2],
                              [succ-9 , x-6 , y-2],
                              [succ-10, x-10, y-6],
                              [succ-8 , x-10, y-1]])).

ctr_draw_example(graph_crossing,
                 ['NODES'],
                 [[[succ-1 , x-4 , y-7],
                   [succ-1 , x-2 , y-5],
                   [succ-1 , x-7 , y-6],
                   [succ-2 , x-1 , y-2],
                   [succ-3 , x-2 , y-2],
                   [succ-2 , x-5 , y-3],
                   [succ-3 , x-8 , y-2],
                   [succ-9 , x-6 , y-2],
                   [succ-10, x-10, y-6],
                   [succ-8 , x-10, y-1]]],
                 ['CLIQUE'],
                 [5-6,7-8],
                 ['NARC'],
                 '','NARC=2',
                 [4,2.145,1.4,1]).

ctr_see_also(graph_crossing,
 [link('common keyword', map,                     '%k,%k', ['graph constraint', 'graph partitioning constraint']),
  link('common keyword', cycle,                   '%k,%k', ['graph constraint', 'graph partitioning constraint']),
  link('common keyword', tree,                    '%k,%k', ['graph constraint', 'graph partitioning constraint']),
  link('common keyword', crossing,                '%k',    ['line segments intersection'],'\\\\ '),
  link('common keyword', two_layer_edge_crossing, '%k',    ['line segments intersection'])]).

ctr_key_words(graph_crossing,['graph constraint'             ,
                              'graph partitioning constraint',
                              'geometrical constraint'       ,
                              'line segments intersection'   ,
                              'functional dependency'        ,
		              'pure functional dependency'   ]).

ctr_persons(graph_crossing,['Beldiceanu N.']).

ctr_application(graph_crossing, [2]).
