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

ctr_date(ninterval,['20030820','20040530','20060812']).

ctr_origin(ninterval, 'Derived from %c.', [nvalue]).

ctr_arguments(ninterval,
              ['NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'SIZE_INTERVAL'-int             ]).

ctr_exchangeable(ninterval,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],intervals('SIZE_INTERVAL'),=,dontcare,dontcare)]).

ctr_restrictions(ninterval,
                 ['NVAL' >= min(1,size('VARIABLES')),
                  'NVAL' =< size('VARIABLES')       ,
                  required('VARIABLES',var)         ,
                  'SIZE_INTERVAL' > 0               ]).

ctr_typical(ninterval,
            ['NVAL'                                                      > 1                     ,
             'NVAL'                                                      < size('VARIABLES')     ,
             'SIZE_INTERVAL'                                             > 1                     ,
             'SIZE_INTERVAL'                                             < range('VARIABLES'^var),
	     (nval('VARIABLES'^var)+'SIZE_INTERVAL'-1) / 'SIZE_INTERVAL' < 'NVAL'                ]).

ctr_pure_functional_dependency(ninterval, []).
ctr_functional_dependency(ninterval, 1, [2,3]).

ctr_contractible(ninterval, ['NVAL'=1,size('VARIABLES')>0], 'VARIABLES', any).
ctr_contractible(ninterval, ['NVAL'=size('VARIABLES')], 'VARIABLES', any).

ctr_graph(ninterval,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var / 'SIZE_INTERVAL' = variables2^var / 'SIZE_INTERVAL'],
          ['NSCC' = 'NVAL'],
          []).

ctr_example(ninterval,
            ninterval(2,[[var-3],[var-1],[var-9],[var-1],[var-9]],4)).

ctr_draw_example(ninterval,
                 ['VARIABLES'],
                 [[[var-3],[var-1],[var-9],[var-1],[var-9]]],
                 ['CLIQUE'],
                 [1-[1,2,4],
                  2-[1,2,4],
                  3-[3,5],
                  4-[1,2,4],
                  5-[3,5]],
                 ['NSCC'([[1,2,4],[3,5]])],
                 '','NSCC=2',
                 [2.4,2.16,2.145,2.2]).

ctr_see_also(ninterval,
 [link('specialisation', nvalue,       '%e replaced by %e',       [variable/constant,variable]),
  link('related',        nequivalence, '%e replaced by %e',       [variable/constant,variable mod constant]),
  link('related',        nclass,       '%e replaced by %e',       [variable/constant,in_list(variable,partition)]),
  link('related',        npair,        '%e replaced by %e of %e', [variable/constant,pair,variables])]).

ctr_key_words(ninterval,['counting constraint'                   ,
                         'value partitioning constraint'         ,
                         'number of distinct equivalence classes',
                         'interval'                              ,
                         'strongly connected component'          ,
                         'equivalence'                           ,
                         'functional dependency'                 ,
		         'pure functional dependency'            ]).

ctr_persons(ninterval,['Beldiceanu N.',
                       'Carlsson M.'  ,
                       'Thiel S.'     ]).

ctr_eval(ninterval, [checker(ninterval_c)]).

ninterval_c(NVAL, VARIABLES, SIZE_INTERVAL) :-
	collection(VARIABLES, [int]),
	integer(SIZE_INTERVAL),
	SIZE_INTERVAL > 0,
	get_attr1(VARIABLES, VARS),
	length(VARS, L),
	MIN_NVAL is min(1,L),
        check_type(dvar(MIN_NVAL,L), NVAL),
        (L = 0 ->
	    NVAL #= 0
	;
	    gen_quotient_fix(VARS, SIZE_INTERVAL, QUOTIENT),
	    sort(QUOTIENT, SORTED),
	    length(SORTED, N),
	    NVAL #= N
	).
