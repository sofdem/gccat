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

ctr_date(nvalues,['20030820','20060812']).

ctr_origin(nvalues, 'Inspired by %c and %c.', [nvalue,count]).

ctr_arguments(nvalues,
              ['VARIABLES'-collection(var-dvar),
               'RELOP'-atom                    ,
               'LIMIT'-dvar                    ]).

ctr_exchangeable(nvalues,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(nvalues,
                 [required('VARIABLES',var)         ,
                  in_list('RELOP',[=,=\=,<,>=,>,=<])]).

ctr_typical(nvalues,
            [size('VARIABLES') > 1                ,
             'LIMIT'           > 1                ,
             'LIMIT'           < size('VARIABLES'),
             in_list('RELOP',[=,<,>=,>,=<])       ]).

ctr_pure_functional_dependency(nvalues, [in_list('RELOP',[=])]).

ctr_contractible(nvalues, [in_list('RELOP',[<,=<])], 'VARIABLES', any).
ctr_contractible(nvalues, [in_list('RELOP',[=]),'LIMIT'=1,size('VARIABLES')>0], 'VARIABLES', any).
ctr_contractible(nvalues, [in_list('RELOP',[=]),'LIMIT'=size('VARIABLES')], 'VARIABLES', any).

ctr_extensible(nvalues, [in_list('RELOP',[>=,>])], 'VARIABLES', any).

ctr_graph(nvalues,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['RELOP'('NSCC','LIMIT')],
          ['EQUIVALENCE']).

ctr_example(nvalues,
            nvalues([[var-4],[var-5],[var-5],[var-4],[var-1],[var-5]],
                    =,
                    3)).

ctr_draw_example(nvalues,
                 ['VARIABLES'],
                 [[[var-4],[var-5],[var-5],[var-4],[var-1],[var-5]]],
                 ['CLIQUE'],
                 [1-[1,4],
                  2-[2,3,6],
                  3-[2,3,6],
                  4-[1,4],
                  5-5,
                  6-[2,3,6]],
                 ['NSCC'([[1,4],[2,3,6],[5]])],
                 '','NSCC=3',
                 [2.4,2.3,2.145,2.3]).

ctr_cond_imply(nvalues, nvalues_except_0, [minval('VARIABLES'^var) > 0], [], id).

ctr_see_also(nvalues,
 [link('specialisation', nvalue, 'replace a comparison with the number of distinct values by an equality with the number of distinct values', []),
  link('common keyword', nvalues_except_0,  '%k,%k', ['counting constraint', 'number of distinct values']),
  link('assignment dimension added', assign_and_nvalues, '', [])]).

ctr_key_words(nvalues,['counting constraint'                   ,
                       'value partitioning constraint'         ,
                       'number of distinct equivalence classes',
                       'number of distinct values'             ,
                       'strongly connected component'          ,
                       'domination'                            ,
                       'equivalence'                           ]).

ctr_eval(nvalues, [reformulation(nvalues_r)]).

nvalues_r(VARIABLES, RELOP, LIMIT) :-
    collection(VARIABLES, [dvar]),
    memberchk(RELOP, [=, =\=, <, >=, >, =<]),
    check_type(dvar, LIMIT),
    length(VARIABLES, N),
    NVAL in 0..N,
    get_attr1(VARIABLES, VARS),
    nvalue(NVAL, VARS),
    call_term_relop_value(NVAL, RELOP, LIMIT).
