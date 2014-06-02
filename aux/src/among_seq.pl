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

ctr_date(among_seq,['20000128','20030820']).

ctr_origin(among_seq, '\\cite{BeldiceanuContejean94}', []).

ctr_arguments(among_seq,
              ['LOW'-int                       ,
               'UP'-int                        ,
               'SEQ'-int                       ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(among_seq,
                 [items('VARIABLES',reverse),
                  items('VALUES',all),
                  vals(['LOW'],int(>=(0)),>,dontcare,dontcare),
                  vals(['UP'],int(=<('SEQ')),<,dontcare,dontcare),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_synonyms(among_seq,[sequence]).

ctr_restrictions(among_seq,
                 ['LOW' >= 0                ,
                  'LOW' =< size('VARIABLES'),
                  'UP'  >= 'LOW'            ,
                  'SEQ' >  0                ,
                  'SEQ' >= 'LOW'            ,
                  'SEQ' =< size('VARIABLES'),
                  required('VARIABLES',var) ,
                  required('VALUES',val)    ,
                  distinct('VALUES',val)    ]).

ctr_typical(among_seq,
            ['LOW' < 'SEQ'                     ,
             'UP'  > 0                         ,
             'SEQ' > 1                         ,
             'SEQ' < size('VARIABLES')         ,
             size('VARIABLES') > 1             ,
             size('VALUES')    > 0             ,
             size('VARIABLES') > size('VALUES'),
             'LOW' > 0 #\/ 'UP' < 'SEQ'        ]).

ctr_contractible(among_seq, ['UP'=0], 'VARIABLES', any).
ctr_contractible(among_seq, ['SEQ'=1], 'VARIABLES', any).
ctr_contractible(among_seq, [], 'VARIABLES', prefix).
ctr_contractible(among_seq, [], 'VARIABLES', suffix).

ctr_graph(among_seq, 
          ['VARIABLES'],
          'SEQ',
          ['PATH'>>collection],
          [among_low_up('LOW','UP',collection,'VALUES')],
          ['NARC' = size('VARIABLES')-'SEQ'+1],
	  []).

ctr_example(among_seq,
            among_seq(1,2,4,
		      [[var-9],[var-2],[var-4],[var-5],[var-5],[var-7],[var-2]],
		      [[val-0],[val-2],[val-4],[val-6],[val-8]])).

ctr_see_also(among_seq,
 [link('root concept',                  among,                '',                                                   []),
  link('generalisation',                sliding_distribution, 'single set of values replaced by individual values', []), 
  link('part of system of constraints', among_low_up,         '',                                                   []),
  link('used in graph description',     among_low_up,         '',                                                   [])]).

ctr_key_words(among_seq,['system of constraints'      ,
                         'decomposition'              ,
                         'sliding sequence constraint',
                         'sequence'                   ,
                         'hypergraph'                 ,
                         'arc-consistency'            ,
                         'linear programming'         ,
                         'flow'                       ]).

ctr_persons(among_seq,['Beldiceanu N.'  ,
                       'Contejean E.'   ,
                       'Carlsson M.'    ,
                       'van Hoeve W.-J.',
                       'Pesant G.'      ,
                       'Rousseau L.-M.' ,
                       'Sabharwal A.'   ,
                       'Brand S.'       ,
                       'Narodytska N.'  ,
                       'Quimper C.-G.'  ,
                       'Stuckey P. J.'  ,
                       'Walsh T.'       ,
                       'Maher M. J.'    ]).

ctr_eval(among_seq, [checker(among_seq_c),
		     reformulation(among_seq_r)]).

among_seq_r(LOW, UP, SEQ, VARIABLES, VALUES) :-
    integer(LOW),
    integer(UP),
    integer(SEQ),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int]),
    get_attr1(VALUES, VALS),
    length(VARIABLES, N),
    LOW >= 0,
    LOW =< N,
    SEQ >  0,
    SEQ >= LOW,
    SEQ =< N,
    all_different(VALS),
    among_seq1(LOW, UP, SEQ, VARIABLES, VALUES).

among_seq1(_LOW, _UP, SEQ, VARIABLES, _VALUES) :-
    length(VARIABLES, N),
    N < SEQ, !.
among_seq1(LOW, UP, SEQ, VARIABLES, VALUES) :-
    length(VARIABLES, N),
    N >= SEQ,
    among_seq2(VARIABLES, SEQ, SEQVARIABLES),
    eval(among_low_up(LOW, UP, SEQVARIABLES, VALUES)),
    VARIABLES = [_|RVARIABLES],
    among_seq1(LOW, UP, SEQ, RVARIABLES, VALUES).

among_seq2(_, 0, []):- !.
among_seq2([VAR|VARS], SEQ, [VAR|RVARS]) :-
    SEQ > 0,
    SEQ1 is SEQ-1,
    among_seq2(VARS, SEQ1, RVARS).

among_seq_c(LOW, UP, SEQ, VARIABLES, VALUES) :-
    integer(LOW),
    integer(UP),
    integer(SEQ),
    collection(VARIABLES, [int]),
    collection(VALUES, [int]),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),
    length(VARIABLES, N),
    LOW >= 0,
    LOW =< N,
    SEQ >  0,
    SEQ >= LOW,
    SEQ =< N,
    sort(VALS, SVALS),
    length(VALS, M),
    length(SVALS, M),
    among_seq_check(VARS, SEQ, CONTINUATION, CONTINUATION, 0, LOW, UP, VALS).

among_seq_check([], _, _, _, _, _, _, _) :- !.
among_seq_check([V|R], I, BUFFER, CONT, SUM, LOW, UP, VALS) :-
    (memberchk(V, VALS) -> IN = 1, SUM1 is SUM+1 ; IN = 0, SUM1 is SUM),
    CONT = [IN|CONT1],
    I1 is I-1,
    (I1 < 0 ->
      BUFFER = [OUT|RBUFFER],
      SUM2 is SUM1-OUT,
      SUM2 >= LOW, SUM2 =< UP,
      among_seq_check(R, I1, RBUFFER, CONT1, SUM2, LOW, UP, VALS)
    ;
      (I1 = 0 -> SUM1 >= LOW, SUM1 =< UP ; true),
      among_seq_check(R, I1, BUFFER, CONT1, SUM1, LOW, UP, VALS)
    ).
