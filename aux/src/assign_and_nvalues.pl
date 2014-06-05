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

ctr_date(assign_and_nvalues,['20000128','20030820','20040530','20050321','20060804']).

ctr_origin(assign_and_nvalues, 'Derived from %c and %c.', [assign_and_counts,nvalues]).

ctr_arguments(assign_and_nvalues,
              ['ITEMS'-collection(bin-dvar, value-dvar),
               'RELOP'-atom                            ,
               'LIMIT'-dvar                            ]).

ctr_exchangeable(assign_and_nvalues,
                 [items('ITEMS',all),
                  vals(['ITEMS'^bin],int,=\=,all,dontcare)]).

ctr_restrictions(assign_and_nvalues,
                 [required('ITEMS',[bin,value])     ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(assign_and_nvalues,
            [size('ITEMS')        > 1,
             range('ITEMS'^bin)   > 1,
             range('ITEMS'^value) > 1,
             in_list('RELOP',[<,=<]) ,
             'LIMIT' > 1             ,
             'LIMIT' < size('ITEMS') ]).

ctr_contractible(assign_and_nvalues, [in_list('RELOP',[<,=<])], 'ITEMS', any).
ctr_extensible(assign_and_nvalues, [in_list('RELOP',[>=,>])], 'ITEMS', any).

ctr_graph(assign_and_nvalues,
          ['ITEMS','ITEMS'],
          2,
          ['PRODUCT'>>collection(items1,items2)],
          [items1^bin = items2^bin],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'ITEMS'^value)])]],
          [nvalues(variables,'RELOP','LIMIT')]).

ctr_example(assign_and_nvalues,
            assign_and_nvalues([[bin-2, value-3],
                                [bin-1, value-5],
                                [bin-2, value-3],
                                [bin-2, value-3],
                                [bin-2, value-4]],
                               =<,
                               2)).

ctr_draw_example(assign_and_nvalues,
                 ['ITEMS','ITEMS'],
                 [[[bin-2, value-3],
                   [bin-1, value-5],
                   [bin-2, value-3],
                   [bin-2, value-3],
                   [bin-2, value-4]]],
                 ['PRODUCT'],
                 [1-[1,3,4,5], 2-2, 3-[1,3,4,5], 4-[1,3,4,5], 5-[1,3,4,5]],
                 ['COLLECTIONS'(['ITEMS'-[1,2,3,4,5],'ITEMS'-[6,7,8,9,10]])],
                 '','',
                 []).

ctr_see_also(assign_and_nvalues,
 [link('assignment dimension removed', nvalue,           '',   []),
  link('assignment dimension removed', nvalues,          '',   []),
  link('used in graph description',    nvalues,          '',   []),
  link('common keyword',               nvalues_except_0, '%k', ['number of distinct values']),
  link('related',                      roots,            '',   [])]).

ctr_key_words(assign_and_nvalues,['assignment'               ,
                                  'assignment dimension'     ,
                                  'number of distinct values',
                                  'acyclic'                  ,
                                  'bipartite'                ,
                                  'no loop'                  ]).

ctr_persons(assign_and_nvalues,['Beldiceanu N.']).

ctr_application(assign_and_nvalues, [1]).

ctr_eval(assign_and_nvalues, [reformulation(assign_and_nvalues_r)]).

assign_and_nvalues_r(ITEMS, RELOP, LIMIT) :-
    collection(ITEMS, [dvar,dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    get_attr1(ITEMS, BINS),
    get_attr2(ITEMS, VALUES),
	get_minimum(BINS, MINBINS),
	get_maximum(BINS, MAXBINS),
    gen_matrix_bool(MINBINS, MAXBINS, BINS, BMATRIX),
    get_minimum(VALUES, MINVALUES),
    JOKER is MINVALUES-1,
    LIM is LIMIT + 1,
    assign_and_nvalues1(BMATRIX, VALUES, JOKER, RELOP, LIM).

assign_and_nvalues1([], _, _, _, _).
assign_and_nvalues1([BLINE|RBMATRIX], VALUES, JOKER, RELOP, LIM) :-
    assign_and_nvalues2(BLINE, VALUES, JOKER, VALS),
    length(VALS, M),
    N in 0..M,
    nvalue(N, VALS),
    call_term_relop_value(N, RELOP, LIM),
    assign_and_nvalues1(RBMATRIX, VALUES, JOKER, RELOP, LIM).

assign_and_nvalues2([], [], JOKER, [JOKER]).
assign_and_nvalues2([VAR|RVAR], [VAL|RVAL], JOKER, [V|R]) :-
    (VAR #= 0 #/\ V #= JOKER) #\/ (VAR #= 1 #/\ V #= VAL),
    assign_and_nvalues2(RVAR, RVAL, JOKER, R).
