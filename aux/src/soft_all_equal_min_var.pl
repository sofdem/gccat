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
    ctr_total_relation/1,
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

ctr_date(soft_all_equal_min_var,['20090926']).

ctr_origin(soft_all_equal_min_var, '\\cite{HebrardMarxSullivanRazgon09}', []).

ctr_arguments(soft_all_equal_min_var,
              ['N'-dvar                        ,
               'VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(soft_all_equal_min_var,
                 [vals(['N'],int,<,dontcare,dontcare),
                  items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_restrictions(soft_all_equal_min_var,
                 ['N' >= 0                 ,
                  required('VARIABLES',var)]).

ctr_typical(soft_all_equal_min_var,
            ['N'               > 0                         ,
	     'N'               < size('VARIABLES')         ,
	     'N'               < size('VARIABLES') / 10 + 2,
             size('VARIABLES') > 1                         ]).

ctr_total_relation(soft_all_equal_min_var).

ctr_graph(soft_all_equal_min_var,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['MAX_NSCC' >= size('VARIABLES')-'N'],
          []).

ctr_example(soft_all_equal_min_var,
            soft_all_equal_min_var(1, [[var-5],[var-1],[var-5],[var-5]])).

ctr_draw_example(soft_all_equal_min_var,
                 ['VARIABLES'],
                 [[[var-5],[var-1],[var-5],[var-5]]],
                 ['CLIQUE'],
                 [1-[1,3,4],
                  2-[2],
                  3-[1,3,4],
                  4-[1,3,4]],
                 ['MAX_NSCC'([1,3,4])],
                 '','MAX_NSCC=3',
                 []).

ctr_see_also(soft_all_equal_min_var,
 [link('implied by',     xor,                    '',   []),
  link('hard version',   all_equal,              '',   []),
  link('common keyword', soft_all_equal_min_ctr, '%k', ['soft constraint']),
  link('common keyword', soft_all_equal_max_var, '%k', ['soft constraint']),
  link('common keyword', soft_alldifferent_ctr,  '%k', ['soft constraint']),
  link('common keyword', soft_alldifferent_var,  '%k', ['soft constraint']),
  link('related',        atmost_nvalue,          '',   [])]).

ctr_key_words(soft_all_equal_min_var,['soft constraint'                 ,
                                      'value constraint'                ,
                                      'relaxation'                      ,
                                      'variable-based violation measure',
                                      'arc-consistency'                 ,
                                      'sweep'                           ]).

ctr_persons(soft_all_equal_min_var,['Hebrard E.'    ,
                                    'O\'Sullivan B.',
                                    'Razgon I.'     ,
                                    'Marx D.'       ]).

ctr_eval(soft_all_equal_min_var, [      checker(soft_all_equal_min_var_c),
				  reformulation(soft_all_equal_min_var_r)]).

ctr_sol(soft_all_equal_min_var,2,0,2,21,[0-3,1-9,2-9]).
ctr_sol(soft_all_equal_min_var,3,0,3,172,[0-4,1-40,2-64,3-64]).
ctr_sol(soft_all_equal_min_var,4,0,4,1845,[0-5,1-85,2-505,3-625,4-625]).
ctr_sol(soft_all_equal_min_var,5,0,5,24426,[0-6,1-156,2-1656,3-7056,4-7776,5-7776]).
ctr_sol(soft_all_equal_min_var,6,0,6,386071,[0-7,1-259,2-4039,3-33859,4-112609,5-117649,6-117649]).
ctr_sol(soft_all_equal_min_var,7,0,7,7116320,[0-8,1-400,2-8632,3-104672,4-751472,5-2056832,6-2097152,7-2097152]).
ctr_sol(soft_all_equal_min_var,8,0,8,150156873,[0-9,1-585,2-16713,3-274761,4-2852721,5-18234801,6-42683841,7-43046721,8-43046721]).

soft_all_equal_min_var_c(N, VARIABLES) :-
    check_type(dvar_gteq(0), N),
    collection(VARIABLES,[int]),
    get_attr1(VARIABLES, VARS),
    length(VARS, L),
    (L = 0 ->
	true
    ;
        samsort(VARS, SVARS),
        SVARS = [V|R],
        max_nvalue_seq_size(R, 1, V, 1, M),
        MAX is L-M,
        N #>= MAX
    ).
    
soft_all_equal_min_var_r(N, VARIABLES) :-
    check_type(dvar_gteq(0), N),
    collection(VARIABLES,[dvar]),
    length(VARIABLES, L),
    get_attr1(VARIABLES, VARS),
    get_minimum(VARS, MINVARS),
    get_maximum(VARS, MAXVARS),
    complete_card(MINVARS, MAXVARS, L, OCC, VAL_OCC),
    global_cardinality(VARS, VAL_OCC),
    MAX_OCC in 0..L,
    eval(maximum(MAX_OCC, OCC)),
    call(N #>= L-MAX_OCC).
