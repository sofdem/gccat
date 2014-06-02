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

ctr_date(colored_matrix,['20031017','20040530']).

ctr_origin(colored_matrix, 'KOALOG', []).

ctr_arguments(colored_matrix,
              ['C'-int                                             ,
               'L'-int                                             ,
               'K'-int                                             ,
               'MATRIX'-collection(column-int, line-int, var-dvar ),
               'CPROJ'-collection(column-int , val-int , nocc-dvar),
               'LPROJ'-collection(line-int   , val-int , nocc-dvar)]).

ctr_synonyms(colored_matrix,[coloured_matrix   ,
                             cardinality_matrix,
                             card_matrix       ]).

ctr_restrictions(colored_matrix,
                 ['C' >= 0                                  ,
                  'L' >= 0                                  ,
                  'K' >= 0                                  ,
                  required('MATRIX',[column,line,var])      ,
                  increasing_seq('MATRIX',[column,line])    ,
                  size('MATRIX')   = 'C'*'L' + 'C' + 'L' + 1,
                  'MATRIX'^column >=  0                     ,
                  'MATRIX'^column =< 'C'                    ,
                  'MATRIX'^line   >=  0                     ,
                  'MATRIX'^line   =< 'L'                    ,
                  'MATRIX'^var    >=  0                     ,
                  'MATRIX'^var    =< 'K'                    ,
                  required('CPROJ',[column,val,nocc])       ,
                  increasing_seq('CPROJ',[column,val])      ,
                  size('CPROJ')   = 'C'*'K' + 'C' + 'K' + 1 ,
                  'CPROJ'^column >=  0                      ,
                  'CPROJ'^column =< 'C'                     ,
                  'CPROJ'^val    >=  0                      ,
                  'CPROJ'^val    =< 'K'                     ,
                  required('LPROJ',[line  ,val,nocc])       ,
                  increasing_seq('LPROJ',[line,val])        ,
                  size('LPROJ')   = 'L'*'K' + 'L' + 'K' + 1 ,
                  'LPROJ'^line   >=  0                      ,
                  'LPROJ'^line   =< 'L'                     ,
                  'LPROJ'^val    >=  0                      ,
                  'LPROJ'^val    =< 'K'                     ]).

ctr_typical(colored_matrix,
            ['C'                 >= 1,
             'L'                 >= 1,
             'K'                 >= 1,
             range('MATRIX'^var) >  1]).

ctr_pure_functional_dependency(colored_matrix, []).
ctr_functional_dependency(colored_matrix, 5-3, [1,2,3]).
ctr_functional_dependency(colored_matrix, 6-3, [1,2,3]).

ctr_predefined(colored_matrix).

ctr_example(colored_matrix,
            colored_matrix(1,
                           2,
                           4,
                           [[column-0, line-0, var-3 ],
                            [column-0, line-1, var-1 ],
                            [column-0, line-2, var-3 ],
                            [column-1, line-0, var-4 ],
                            [column-1, line-1, var-4 ],
                            [column-1, line-2, var-3 ]],
                           [[column-0, val-0 , nocc-0],
                            [column-0, val-1 , nocc-1],
                            [column-0, val-2 , nocc-0],
                            [column-0, val-3 , nocc-2],
                            [column-0, val-4 , nocc-0],
                            [column-1, val-0 , nocc-0],
                            [column-1, val-1 , nocc-0],
                            [column-1, val-2 , nocc-0],
                            [column-1, val-3 , nocc-1],
                            [column-1, val-4 , nocc-2]],
                           [[line-0  , val-0 , nocc-0],
                            [line-0  , val-1 , nocc-0],
                            [line-0  , val-2 , nocc-0],
                            [line-0  , val-3 , nocc-1],
                            [line-0  , val-4 , nocc-1],
                            [line-1  , val-0 , nocc-0],
                            [line-1  , val-1 , nocc-1],
                            [line-1  , val-2 , nocc-0],
                            [line-1  , val-3 , nocc-0],
                            [line-1  , val-4 , nocc-1],
                            [line-2  , val-0 , nocc-0],
                            [line-2  , val-1 , nocc-0],
                            [line-2  , val-2 , nocc-0],
                            [line-2  , val-3 , nocc-2],
                            [line-2  , val-4 , nocc-0]])).

ctr_see_also(colored_matrix,
 [link('part of system of constraints', global_cardinality, '',                              []),
  link('common keyword',                k_alldifferent,     '%k',                            ['system of constraints']),
  link('related to a common problem',   same,               'matrix reconstruction problem', [])]).

ctr_key_words(colored_matrix,['system of constraints'     ,
                              'predefined constraint'     ,
                              'timetabling constraint'    ,
                              'functional dependency'     ,
		              'pure functional dependency',
                              'matrix'                    ,
                              'matrix model'              ]).

ctr_persons(colored_matrix,['R\\\'egin J.-C.',
                            'Gomes C.'       ,
                            'Fulkerson D. R.',
                            'Ford Jr. L. R.' ]).
