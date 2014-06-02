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

ctr_date(crossing,['20000128','20030820','20060806']).

ctr_origin(crossing, 'Inspired by \\cite{CormenLeisersonRivest90}.', []).

ctr_arguments(crossing,
              ['NCROSS'-dvar                                            ,
               'SEGMENTS'-collection(ox-dvar, oy-dvar, ex-dvar, ey-dvar)]).

ctr_exchangeable(crossing,
                 [items('SEGMENTS',all),
                  attrs_sync('SEGMENTS',[[ox,oy],[ex,ey]]),
                  translate(['SEGMENTS'^ox,'SEGMENTS'^ex]),
                  translate(['SEGMENTS'^oy,'SEGMENTS'^ey])]).

ctr_restrictions(crossing,
                 ['NCROSS' >= 0                                                     ,
                  'NCROSS' =< (size('SEGMENTS')*size('SEGMENTS')-size('SEGMENTS'))/2,
                  required('SEGMENTS',[ox,oy,ex,ey])                                ]).

ctr_typical(crossing,
            [size('SEGMENTS') > 1]).

ctr_pure_functional_dependency(crossing, []).
ctr_functional_dependency(crossing, 1, [2]).

ctr_graph(crossing,
          ['SEGMENTS'],
          2,
          ['CLIQUE'(<)>>collection(s1,s2)],
          [max(s1^ox,s1^ex) >= min(s2^ox,s2^ex)                             ,
           max(s2^ox,s2^ex) >= min(s1^ox,s1^ex)                             ,
           max(s1^oy,s1^ey) >= min(s2^oy,s2^ey)                             ,
           max(s2^oy,s2^ey) >= min(s1^oy,s1^ey)                             ,
           (s2^ox-s1^ex)*(s1^ey-s1^oy)-(s1^ex-s1^ox)*(s2^oy-s1^ey) = 0   #\/
           (s2^ex-s1^ex)*(s2^oy-s1^oy)-(s2^ox-s1^ox)*(s2^ey-s1^ey) = 0   #\/
           sign((s2^ox-s1^ex)*(s1^ey-s1^oy)-(s1^ex-s1^ox)*(s2^oy-s1^ey)) =\=
           sign((s2^ex-s1^ex)*(s2^oy-s1^oy)-(s2^ox-s1^ox)*(s2^ey-s1^ey))    ],
          ['NARC' = 'NCROSS'],
          ['ACYCLIC', 'NO_LOOP']).

ctr_example(crossing,
            crossing(3,
                     [[ox-1, oy-4, ex-9, ey-2],
                      [ox-1, oy-1, ex-3, ey-5],
                      [ox-3, oy-2, ex-7, ey-4],
                      [ox-9, oy-1, ex-9, ey-4]])).

ctr_draw_example(crossing,
                 ['SEGMENTS'],
                 [[[ox-1, oy-4, ex-9, ey-2],
                   [ox-1, oy-1, ex-3, ey-5],
                   [ox-3, oy-2, ex-7, ey-4],
                   [ox-9, oy-1, ex-9, ey-4]]],
                 ['CLIQUE'(<)],
                 [1-[2,3,4]],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,2.145,2.145,1.7]).

ctr_see_also(crossing,
 [link('common keyword', graph_crossing,          '%k', ['line segments intersection'],'\\\\ '),
  link('common keyword', two_layer_edge_crossing, '%k', ['line segments intersection'])]).

ctr_key_words(crossing,['geometrical constraint'    ,
                        'line segments intersection',
                        'acyclic'                   ,
                        'no loop'                   ,
                        'functional dependency'     ,
		        'pure functional dependency']).

ctr_persons(crossing,['Cormen T. H.'   ,
                      'Leiserson C. E.',
                      'Rivest R. L.'   ]).

ctr_application(crossing, [2]).
