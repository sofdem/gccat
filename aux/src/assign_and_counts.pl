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

ctr_date(assign_and_counts,['20000128','20030820','20060804']).

ctr_origin(assign_and_counts, 'N.~Beldiceanu', []).

ctr_arguments(assign_and_counts,
              ['COLOURS'-collection(val-int)            ,
               'ITEMS'-collection(bin-dvar, colour-dvar),
               'RELOP'-atom                             ,
               'LIMIT'-dvar                             ]).

ctr_exchangeable(assign_and_counts,
                 [items('COLOURS',all),
                  items('ITEMS',all),
                  vals(['ITEMS'^bin],int,=\=,all,dontcare)]).

ctr_restrictions(assign_and_counts,
                 [required('COLOURS',val)           ,
                  distinct('COLOURS',val)           ,
                  required('ITEMS',[bin,colour])    ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(assign_and_counts,
            [size('COLOURS')    > 0 ,
             size('ITEMS')      > 1 ,
             range('ITEMS'^bin) > 1 ,
             in_list('RELOP',[<,=<]),
             'LIMIT' > 0            ,
             'LIMIT' < size('ITEMS')]).

ctr_contractible(assign_and_counts, [in_list('RELOP',[<,=<])], 'ITEMS', any).
ctr_extensible(assign_and_counts, [in_list('RELOP',[>=,>])], 'ITEMS', any).

ctr_derived_collections(assign_and_counts,
                        [col('VALUES'-collection(val-int),
                             [item(val-'COLOURS'^val)])]).

ctr_graph(assign_and_counts,
          ['ITEMS','ITEMS'],
          2,
          ['PRODUCT'>>collection(items1,items2)],
          [items1^bin = items2^bin],
          [],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP'],
          ['SUCC'>>[source,
                    variables-col('VARIABLES'-collection(var-dvar),
                                  [item(var-'ITEMS'^colour)])]],
          [counts('VALUES',variables,'RELOP','LIMIT')]).

ctr_example(assign_and_counts,
            assign_and_counts([[val-4]],
                              [[bin-1, colour-4],
                               [bin-3, colour-4],
                               [bin-1, colour-4],
                               [bin-1, colour-5]],
                              =<,
                              2)).

ctr_draw_example(assign_and_counts,
                 ['ITEMS','ITEMS'],
                 [[[bin-1, colour-4],
                   [bin-3, colour-4],
                   [bin-1, colour-4],
                   [bin-1, colour-5]]],
                 ['PRODUCT'],
                 [1-[1,3,4], 2-2, 3-[1,3,4], 4-[1,3,4]],
                 ['COLLECTIONS'(['ITEMS'-[1,2,3,4],'ITEMS'-[5,6,7,8]])],
                 '','',
                 []).

ctr_see_also(assign_and_counts,
 [link('assignment dimension removed', count,  '', []),
  link('assignment dimension removed', counts, '', []),
  link('used in graph description',    counts, '', [])]).

ctr_key_words(assign_and_counts,['assignment'                      ,
                                 'assignment dimension'            ,
                                 'coloured'                        ,
                                 'automaton'                       ,
                                 'automaton with array of counters',
                                 'derived collection'              ,
                                 'acyclic'                         ,
                                 'bipartite'                       ,
                                 'no loop'                         ]).

ctr_persons(assign_and_counts,['Beldiceanu N.']).

ctr_application(assign_and_counts, [2]).

ctr_eval(assign_and_counts, [reformulation(assign_and_counts_r)]).

assign_and_counts_r(COLOURS, ITEMS, RELOP, LIMIT) :-
    collection(COLOURS, [int]),
    collection(ITEMS, [dvar,dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    get_attr1(COLOURS, COLS),
    all_different(COLS),
    get_attr1(ITEMS, BINS),
    get_attr2(ITEMS, ITEMSCOLOURS),
    get_minimum(BINS, MINBINS),
    get_maximum(BINS, MAXBINS),
    gen_matrix_bool(MINBINS, MAXBINS, BINS, BMATRIX),
    assign_and_counts1(ITEMSCOLOURS, COLS, CLINE),
    assign_and_counts2(BMATRIX, CLINE, RELOP, LIMIT).

% CLINE: pour chaque tâche construit une variable disant si un item peut ou pas prendre la couleur (indépendament du bin)
% dans la limit il faut qu'au moins un item soit affecté à un bin donné fait le et sur les variables bin
assign_and_counts1([], _, []).
assign_and_counts1([ITEMCOLOUR|RITEMCOLOURS], COLS, [B|R]) :-
    build_or_var_in_values(COLS, ITEMCOLOUR, OR),
    call(OR #<=> B),
    assign_and_counts1(RITEMCOLOURS, COLS, R).

assign_and_counts2([], _, _, _).
assign_and_counts2([BLINE|RBMATRIX], CLINE, RELOP, LIMIT) :-
    assign_and_counts3(BLINE, CLINE, TERM, OR_B),
    call(A #= OR_B),
    call_term_relop_value(TERM, RELOP, A*LIMIT),
    assign_and_counts2(RBMATRIX, CLINE, RELOP, LIMIT).

assign_and_counts3([], [], 0, 0).
assign_and_counts3([B|RBLINE], [C|RCLINE], B*C+R, BC #\/ S) :-
    BC #<=> B #/\ C,
    assign_and_counts3(RBLINE, RCLINE, R, S).
