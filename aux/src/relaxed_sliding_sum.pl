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

ctr_date(relaxed_sliding_sum,['20000128','20030820','20060813']).

ctr_origin(relaxed_sliding_sum, '\\index{CHIP|indexuse}CHIP', []).

ctr_arguments(relaxed_sliding_sum,
              ['ATLEAST'-int                   ,
               'ATMOST'-int                    ,
               'LOW'-int                       ,
               'UP'-int                        ,
               'SEQ'-int                       ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(relaxed_sliding_sum,
                 [vals(['ATLEAST'],int(>=(0)),>,dontcare,dontcare),
                  vals(['ATMOST'],int(=<(size('VARIABLES')-'SEQ'+1)),<,dontcare,dontcare),
                  items('VARIABLES',reverse)]).

ctr_restrictions(relaxed_sliding_sum,
                 ['ATLEAST' >= 0                        ,
                  'ATMOST'  >= 'ATLEAST'                ,
                  'ATMOST'  =< size('VARIABLES')-'SEQ'+1,
                  'UP'      >= 'LOW'                    ,
                  'SEQ'     >  0                        ,
                  'SEQ'     =< size('VARIABLES')        ,
                  required('VARIABLES',var)             ]).

ctr_typical(relaxed_sliding_sum,
            ['SEQ'                  > 1                            ,
             'SEQ'                  < size('VARIABLES')            ,
             range('VARIABLES'^var) > 1                            ,
             'ATLEAST' > 0 #\/ 'ATMOST' < size('VARIABLES')-'SEQ'+1]).

ctr_graph(relaxed_sliding_sum,
          ['VARIABLES'],
          'SEQ',
          ['PATH'>>collection],
          [sum_ctr(collection,>=,'LOW'),
           sum_ctr(collection,=<,'UP' )],
          ['NARC' >= 'ATLEAST',
           'NARC' =< 'ATMOST' ],
          []).

ctr_example(relaxed_sliding_sum,
            relaxed_sliding_sum(3,4,3,7,4,
                                [[var-2],[var-4],[var-2],[var-0],
                                 [var-0],[var-3],[var-4]])).

ctr_see_also(relaxed_sliding_sum,
 [link('hard version',              sliding_sum, '',                       []),
  link('used in graph description', sum_ctr,     'the sliding constraint', [])]).

ctr_key_words(relaxed_sliding_sum,['sliding sequence constraint',
                                   'soft constraint'            ,
                                   'relaxation'                 ,
                                   'sequence'                   ,
                                   'hypergraph'                 ]).

ctr_persons(relaxed_sliding_sum,['Beldiceanu N.',
                                 'Carlsson M.'  ]).

ctr_eval(relaxed_sliding_sum, [reformulation(relaxed_sliding_sum_r)]).

relaxed_sliding_sum_r(ATLEAST, ATMOST, LOW, UP, SEQ, VARIABLES) :-
    integer(ATLEAST),
    integer(ATMOST),
    integer(LOW),
    integer(UP),
    integer(SEQ),
    collection(VARIABLES, [dvar]),
    length(VARIABLES, N),
    LIMIT is N-SEQ+1,
    ATLEAST >= 0,
    ATMOST  >= ATLEAST,
    ATMOST  =< LIMIT,
    UP      >= LOW,
    SEQ     >  0,
    SEQ     =< N,
    get_attr1(VARIABLES, VARS),
    relaxed_sliding_sum1(VARS, [], LOW, UP, SEQ, SUMB),
    call(SUMB #>= ATLEAST),
    call(SUMB #=< ATMOST).

relaxed_sliding_sum1([], _,  _, _, _, 0).
relaxed_sliding_sum1([Last|R], Seq, LOW, UP, SEQ, B+RB) :-
    append(Seq, [Last], Sequence),
    length(Sequence, L),
    (L > SEQ -> (Sequence = [_|SeqCur],
                 build_sum_var(SeqCur, SumVar),
                 B in 0..1,
                 call((SumVar #>= LOW #/\ SumVar #=< UP) #<=> B),
                 relaxed_sliding_sum1(R, SeqCur, LOW, UP, SEQ, RB))
              ;
     L = SEQ -> (build_sum_var(Sequence, SumVar),
                 B in 0..1,
                 call((SumVar #>= LOW #/\ SumVar #=< UP) #<=> B),
                 relaxed_sliding_sum1(R, Sequence, LOW, UP, SEQ, RB))
              ;
                (relaxed_sliding_sum1(R, Sequence, LOW, UP, SEQ, RB))
    ).
