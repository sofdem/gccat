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

ctr_date(sliding_distribution,['20031008','20060815','20090524']).

ctr_origin(sliding_distribution, '\\cite{ReginPuget97}', []).

ctr_arguments(sliding_distribution,
              ['SEQ'-int                                       ,
               'VARIABLES'-collection(var-dvar)                ,
               'VALUES'-collection(val-int, omin-int, omax-int)]).

ctr_exchangeable(sliding_distribution,
                 [items('VARIABLES',reverse),
                  vals(['VARIABLES'^var],all(notin('VALUES'^val)),=,dontcare,dontcare),
                  items('VALUES',all),
                  vals(['VALUES'^omin],int(>=(0)),>,dontcare,dontcare),
                  vals(['VALUES'^omax],int(=<('SEQ')),<,dontcare,dontcare),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(sliding_distribution,
                 ['SEQ' >  0                        ,
                  'SEQ' =< size('VARIABLES')        ,
                  required('VARIABLES',var)         ,
                  size('VALUES') > 0                ,
                  required('VALUES',[val,omin,omax]),
                  distinct('VALUES',val)            ,
                  'VALUES'^omin >= 0                ,
                  'VALUES'^omax =< 'SEQ'            ,
                  'VALUES'^omin =< 'VALUES'^omax    ]).

ctr_typical(sliding_distribution,
            ['SEQ'             > 1                ,
             'SEQ'             < size('VARIABLES'),
             size('VARIABLES') > size('VALUES')   ]).

ctr_contractible(sliding_distribution, ['SEQ'=1], 'VARIABLES', any).
ctr_contractible(sliding_distribution, [], 'VARIABLES', prefix).
ctr_contractible(sliding_distribution, [], 'VARIABLES', suffix).
ctr_contractible(sliding_distribution, [], 'VALUES', any).

ctr_graph(sliding_distribution,
          ['VARIABLES'],
          'SEQ',
          ['PATH'>>collection],
          [global_cardinality_low_up(collection,'VALUES')],
          ['NARC' = size('VARIABLES')-'SEQ'+1],
          []).

ctr_example(sliding_distribution,
            sliding_distribution(4,
                                 [[var-0],[var-5],[var-0],[var-6],[var-5],[var-0],[var-0]],
                                 [[val-0,omin-1,omax-2],
                                  [val-1,omin-0,omax-4],
                                  [val-4,omin-0,omax-4],
                                  [val-5,omin-1,omax-2],
                                  [val-6,omin-0,omax-2]])).

ctr_see_also(sliding_distribution,
 [link('specialisation',                among_seq,                 'individual values replaced by single set of values', []),
  link('common keyword',                sliding_sum,               '%k',                                                 ['sliding sequence constraint']),
  link('common keyword',                stretch_circuit,           '%k',                                                 ['sliding sequence constraint']),
  link('common keyword',                stretch_path,              '%k',                                                 ['sliding sequence constraint']),
  link('common keyword',                pattern,                   '%k',                                                 ['sliding sequence constraint']),
  link('part of system of constraints', global_cardinality_low_up, '',                                                   []),
  link('used in graph description',     global_cardinality_low_up, '',                                                   [])]).

ctr_key_words(sliding_distribution,['decomposition'              ,
                                    'sliding sequence constraint',
                                    'sequence'                   ,
                                    'system of constraints'      ,
                                    'hypergraph'                 ]).

ctr_persons(sliding_distribution,['R\\\'egin J.-C.',
                                  'Puget J.-F.'   ]).

ctr_eval(sliding_distribution, [reformulation(sliding_distribution_r)]).

sliding_distribution_r(SEQ, VARIABLES, VALUES) :-
    integer(SEQ),
    collection(VARIABLES,[dvar]),
    length(VARIABLES, L),
    SEQ > 0,
    SEQ =< L,
	collection(VALUES, [int,int(0,L),int(0,L)]),
	length(VALUES, M),
    M > 0,
    sliding_distribution1(VARIABLES, [], VALUES, SEQ).

sliding_distribution1([], _, _, _).
sliding_distribution1([Last|R], Seq, VALUES, SEQ) :-
    append(Seq, [Last], Sequence),
    length(Sequence, L),
    (L > SEQ -> (Sequence = [_|SeqCur],
                 eval(global_cardinality_low_up(SeqCur, VALUES)),
                 sliding_distribution1(R, SeqCur, VALUES, SEQ))
              ;
     L = SEQ -> (eval(global_cardinality_low_up(Sequence, VALUES)),
                 sliding_distribution1(R, Sequence, VALUES, SEQ))
              ;
                (sliding_distribution1(R, Sequence, VALUES, SEQ))
    ).
