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

ctr_date(nvalues_except_0,['20030820','20060812']).

ctr_origin(nvalues_except_0, 'Derived from %c.', [nvalues]).

ctr_arguments(nvalues_except_0,
              ['VARIABLES'-collection(var-dvar),
               'RELOP'-atom                    ,
               'LIMIT'-dvar                    ]).

ctr_exchangeable(nvalues_except_0,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int(=\=(0)),=\=,all,dontcare)]).

ctr_restrictions(nvalues_except_0,
                 [required('VARIABLES',var)         ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(nvalues_except_0,
            [size('VARIABLES') > 1                ,
             'LIMIT'           > 1                ,
             'LIMIT'           < size('VARIABLES'),
             atleast(1,'VARIABLES',0)             ,
             in_list('RELOP',[=,<,>=,>,=<])       ]).

ctr_contractible(nvalues_except_0, [in_list('RELOP',[<,=<])], 'VARIABLES', any).

ctr_extensible(nvalues_except_0, [in_list('RELOP',[>=,>])], 'VARIABLES', any).

ctr_graph(nvalues_except_0,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var =\= 0             ,
           variables1^var =   variables2^var],
          ['RELOP'('NSCC','LIMIT')],
          []).

ctr_example(nvalues_except_0,
            nvalues_except_0([[var-4],[var-5],[var-5],[var-4],[var-0],[var-1]],
                             =,
                             3)).

ctr_draw_example(nvalues_except_0,
                 ['VARIABLES'],
                 [[[var-4],[var-5],[var-5],[var-4],[var-0],[var-1]]],
                 ['CLIQUE'],
                 [1-[1,4],
                  2-[2,3],
                  3-[2,3],
                  4-[1,4],
                  6-[6]],
                 ['NSCC'([[1,4],[2,3],[6]])],
                 '','NSCC=3',
                 [2.4,2.145,2.145,2.2]).

ctr_see_also(nvalues_except_0,
 [link('common keyword', nvalue,             '%k,%k', ['counting constraint', 'number of distinct values']),
  link('common keyword', nvalues,            '%k,%k', ['counting constraint', 'number of distinct values']),
  link('common keyword', assign_and_nvalues, '%k',    ['number of distinct values'],'\\\\ ')]).

ctr_key_words(nvalues_except_0,['counting constraint'          ,
                                'value partitioning constraint',
                                'number of distinct values'    ,
                                'strongly connected component' ,
                                'joker value'                  ]).

ctr_eval(nvalues_except_0, [reformulation(nvalues_except_0_r)]).

nvalues_except_0_r(VARIABLES, RELOP, LIMIT) :-
	collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    length(VARIABLES, N),
    N1 is N+1,
    NVAL1 in 1..N1,
    get_attr1(VARIABLES, VARS),
    append([0], VARS, VARS0),
    nvalue(NVAL1, VARS0),
    NVAL1 #= NVAL+1,
    call_term_relop_value(NVAL, RELOP, LIMIT).
