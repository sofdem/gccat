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

ctr_date(discrepancy,['20050506','20060808']).

ctr_origin(discrepancy, '\\cite{Focacci01} and \\cite{vanHoeve05}', []).

ctr_arguments(discrepancy,
              ['VARIABLES'-collection(var-dvar, bad-sint),
               'K'-int                                   ]).

ctr_exchangeable(discrepancy,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var,'VARIABLES'^bad],int,=\=,all,dontcare)]).

ctr_restrictions(discrepancy,
                 [required('VARIABLES',var),
                  required('VARIABLES',bad),
                  'K' >= 0                 ,
                  'K' =< size('VARIABLES') ]).

ctr_typical(discrepancy,
            [size('VARIABLES') > 1  ,
             'K' < size('VARIABLES')]).

ctr_pure_functional_dependency(discrepancy, []).
ctr_functional_dependency(discrepancy, 2, [1]).

% discrepancy('VARIABLES1', 'K1') and
% discrepancy('VARIABLES2', 'K2') =>
% discrepancy(union('VARIABLES1','VARIABLES2'), 'K1'+'K2')
ctr_aggregate(discrepancy, [], [union, +]).

ctr_graph(discrepancy,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [in_set(variables^var,variables^bad)],
          ['NARC' = 'K'],
          []).

ctr_example(discrepancy,
            discrepancy([[var-4,bad-{1,4,6}],
                         [var-5,bad-{0,1}  ],
                         [var-5,bad-{1,6,9}],
                         [var-4,bad-{1,4}  ],
                         [var-1,bad-{}     ]],2)).

ctr_draw_example(discrepancy,
                 ['VARIABLES'],
                 [[[var-4,bad-{1,4,6}],
                   [var-5,bad-{0,1}  ],
                   [var-5,bad-{1,6,9}],
                   [var-4,bad-{1,4}  ],
                   [var-1,bad-{}     ]]],
                 ['SELF'],
                 [1-1,4-4],
                 ['NARC'],
                 '','NARC=2',
                 [2.145,2.145,2.145,0.53625]).

ctr_see_also(discrepancy,
 [link('common keyword',            among,  '%k', ['counting constraint']),
  link('used in graph description', in_set, '',   [])]).

ctr_key_words(discrepancy,['value constraint'          ,
                           'counting constraint'       ,
                           'heuristics'                ,
                           'limited discrepancy search',
                           'arc-consistency'           ,
                           'functional dependency'     ,
		           'pure functional dependency']).

ctr_persons(discrepancy,['Focacci F.'     ,
                         'van Hoeve W.-J.',
                         'Ginsberg M. L.' ,
                         'Harvey W. D.'   ]).
