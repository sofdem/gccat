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

ctr_date(among_var,['20090418']).

ctr_origin(among_var, 'Generalisation of %c', [among]).

ctr_arguments(among_var,
              ['NVAR'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-dvar)   ]).

ctr_exchangeable(among_var,
                 [items('VARIABLES',all),
                  items('VALUES',all),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_restrictions(among_var,
                 ['NVAR' >= 0                ,
                  'NVAR' =< size('VARIABLES'),
                  required('VARIABLES',var)  ,
                  required('VALUES',val)     ]).

ctr_typical(among_var,
            [size('VARIABLES') > 1             ,
             size('VALUES')    > 1             ,
             size('VARIABLES') > size('VALUES')]).

ctr_pure_functional_dependency(among_var, []).
ctr_functional_dependency(among_var, 1, [2,3]).

ctr_contractible(among_var, ['NVAR'=0], 'VARIABLES', any).
ctr_contractible(among_var, ['NVAR'=size('VARIABLES')], 'VARIABLES', any).

% among_var('NVAR1', 'VARIABLES1', 'VALUES1') and
% among_var('NVAR2', 'VARIABLES2', 'VALUES2') =>
% among_var('NVAR1'+'NVAR2', union('VARIABLES1','VARIABLES2'), union('VALUES1','VALUES2'))
ctr_aggregate(among_var, [], [+, union, union]).

ctr_graph(among_var,
          ['VARIABLES','VALUES'],
          2,
          ['PRODUCT'>>collection(variables,values)],
          [variables^var = values^val],
          ['NSOURCE' = 'NVAR'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(among_var,
            among_var(3,
                      [[var-4],[var-5],[var-5],[var-4],[var-1]],
                      [[val-1],[val-5],[val-8],[val-1]])).

ctr_draw_example(among_var,
                 ['VARIABLES','VALUES'],
                 [[[var-4],[var-5],[var-5],[var-4],[var-1]],
                  [[val-1],[val-5],[val-8],[val-1]]],
                 ['PRODUCT'],
                 [2-2,
                  3-2,
                  5-[1,4]],
                 ['NSOURCE'([2,3,5])],
                 '','NSOURCE=3',
                 [2.145,2.145,2.145,2.145]).

ctr_see_also(among_var,
 [link('implied by',                among,  '',                                       []),
  link('specialisation',            among,  '%e replaced by %e within list of %e %e', [variable,constant,values,'VALUES']),
  link('related',                   common, '',                                       []),
  link('uses in its reformulation', min_n,  '',                                       [])]).

ctr_key_words(among_var,['counting constraint'       ,
                         'acyclic'                   ,
                         'bipartite'                 ,
                         'no loop'                   ,
                         'functional dependency'     ,
		         'pure functional dependency']).

ctr_eval(among_var, [reformulation(among_var_r)]).

among_var_r(NVAR, VARIABLES, []) :-
	!,
	check_type(dvar, NVAR),
	collection(VARIABLES, [dvar]),
	NVAR = 0.
among_var_r(NVAR, VARIABLES, VALUES) :-
	check_type(dvar, NVAR),
	collection(VARIABLES, [dvar]),
	collection(VALUES, [dvar]),
	get_attr1(VARIABLES, VARS),
	get_attr1(VALUES, VALS),
	length(VARIABLES, N),
	NVAR #>= 0,
	NVAR #=< N,
	among_var1(VARS, VALS, SUM_BVARS),
	call(NVAR #= SUM_BVARS).

among_var1([], _, 0).
among_var1([V|R], VALS, B+S) :-
	build_or_var_in_values(VALS, V, OR),
	call(OR #<=> B),
        among_var1(R, VALS, S).
