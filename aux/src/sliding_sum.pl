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

ctr_date(sliding_sum,['20000128','20030820','20060815']).

ctr_origin(sliding_sum, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(sliding_sum,
              ['LOW'-int,
               'UP'-int,
               'SEQ'-int,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(sliding_sum,
                 [items('VARIABLES',reverse)]).

ctr_synonyms(sliding_sum,[sequence]).

ctr_restrictions(sliding_sum,
                 ['UP'  >= 'LOW'            ,
                  'SEQ' >  0                ,
                  'SEQ' =< size('VARIABLES'),
                  required('VARIABLES',var) ]).

ctr_typical(sliding_sum,
            ['LOW'           >= 0                   ,
             'UP'            >  0                   ,
             'SEQ'           >  1                   ,
             'SEQ'           <  size('VARIABLES')   ,
             'VARIABLES'^var >= 0                   ,
             'UP'            <  sum('VARIABLES'^var)]).

ctr_contractible(sliding_sum, ['SEQ'=1], 'VARIABLES', any).
ctr_contractible(sliding_sum, [], 'VARIABLES', prefix).
ctr_contractible(sliding_sum, [], 'VARIABLES', suffix).

ctr_graph(sliding_sum,
          ['VARIABLES'],
          'SEQ',
          ['PATH'>>collection],
          [sum_ctr(collection,>=,'LOW'),
           sum_ctr(collection,=<,'UP' )],
          ['NARC' = size('VARIABLES')-'SEQ'+1],
          []).

ctr_example(sliding_sum,
            sliding_sum(3,7,4,
                        [[var-1],[var-4],[var-2],[var-0],[var-0],[var-3],[var-4]])).

ctr_see_also(sliding_sum,
 [link('soft variant',                  relaxed_sliding_sum,  '',   []),
  link('common keyword',                sliding_distribution, '%k', ['sliding sequence constraint']),
  link('part of system of constraints', sum_ctr,              '',   []),
  link('used in graph description',     sum_ctr,              '',   [])]).

ctr_key_words(sliding_sum,['decomposition'              ,
                           'sliding sequence constraint',
                           'sequence'                   ,
                           'system of constraints'      ,
                           'hypergraph'                 ,
                           'sum'                        ,
                           'linear programming'         ,
                           'flow'                       ,
                           'bound-consistency'          ]).

ctr_persons(sliding_sum,['Beldiceanu N.',
                         'Carlsson M.'  ,
                         'Maher M. J.'  ,
                         'Narodytska N.',
                         'Quimper C.-G.',
                         'Walsh T.'     ]).

ctr_eval(sliding_sum, [reformulation(sliding_sum_r)]).

sliding_sum_r(LOW, UP, SEQ, VARIABLES) :-
    integer(LOW),
    integer(UP),
    integer(SEQ),
    collection(VARIABLES,[dvar]),
    length(VARIABLES, L),
    UP  >= LOW,
    SEQ > 0,
    SEQ =< L,
    sliding_sum1(VARIABLES, [], LOW, UP, SEQ).

sliding_sum1([], _,  _, _, _).
sliding_sum1([Last|R], Seq, LOW, UP, SEQ) :-
    append(Seq, [Last], Sequence),
    length(Sequence, L),
    (L > SEQ -> (Sequence = [_|SeqCur],
                 eval(sum_ctr(SeqCur, >=, LOW)),
                 eval(sum_ctr(SeqCur, =<, UP )),
                 sliding_sum1(R, SeqCur, LOW, UP, SEQ))
              ;
     L = SEQ -> (eval(sum_ctr(Sequence, >=, LOW)),
                 eval(sum_ctr(Sequence, =<, UP )),
                 sliding_sum1(R, Sequence, LOW, UP, SEQ))
              ;
                (sliding_sum1(R, Sequence, LOW, UP, SEQ))
    ).
