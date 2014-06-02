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

:-dynamic group_skip_isolated_item_a/6.

ctr_date(group_skip_isolated_item,['20000128','20030820','20040530','20060809','20130301']).

ctr_origin(group_skip_isolated_item, 'Derived from %c.', [group]).

ctr_arguments(group_skip_isolated_item,
              ['NGROUP'-dvar                   ,
               'MIN_SIZE'-dvar                 ,
               'MAX_SIZE'-dvar                 ,
               'NVAL'-dvar                     ,
               'VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int)    ]).

ctr_exchangeable(group_skip_isolated_item,
                 [items('VARIABLES',reverse),
                  items('VALUES',all),
                  vals(['VARIABLES'^var],comp('VALUES'^val),=,dontcare,dontcare)]).

ctr_restrictions(group_skip_isolated_item            ,
                 ['NGROUP'   >=  0                   ,
		  3*'NGROUP' =< size('VARIABLES') + 1,
                  'MIN_SIZE' >=  0                   ,
		  'MIN_SIZE' =\= 1                   ,
                  'MAX_SIZE' >= 'MIN_SIZE'           ,
                  'NVAL'     >= 'MAX_SIZE'           ,
                  'NVAL'     >= 'NGROUP'             ,
                  'NVAL'     =< size('VARIABLES')    ,
                  required('VARIABLES',var)          ,
                  required('VALUES',val)             ,
                  distinct('VALUES',val)             ]).

ctr_typical(group_skip_isolated_item,
            ['NGROUP'               > 0                ,
             'MIN_SIZE'             > 0                ,
             'NVAL'                 > 'MAX_SIZE'       ,
             'NVAL'                 > 'NGROUP'         ,
             'NVAL'                 < size('VARIABLES'),
             size('VARIABLES')      > 1                ,
             range('VARIABLES'^var) > 1                ,
             size('VALUES')         > 0                ,
             size('VARIABLES')      > size('VALUES')   ]).

ctr_functional_dependency(group_skip_isolated_item, 1, [5,6]).
ctr_functional_dependency(group_skip_isolated_item, 2, [5,6]).
ctr_functional_dependency(group_skip_isolated_item, 3, [5,6]).
ctr_functional_dependency(group_skip_isolated_item, 4, [5,6]).

ctr_graph(group_skip_isolated_item,
          ['VARIABLES'],
          2,
          ['CHAIN'>>collection(variables1,variables2)],
          [in(variables1^var,'VALUES'),
           in(variables2^var,'VALUES')],
          ['NSCC'     = 'NGROUP'  ,
           'MIN_NSCC' = 'MIN_SIZE',
           'MAX_NSCC' = 'MAX_SIZE',
           'NVERTEX'  = 'NVAL'    ],
          []).

ctr_example(group_skip_isolated_item,
            group_skip_isolated_item(1,2,2,3,
                                     [[var-2],[var-8],[var-1],[var-7],[var-4],
                                      [var-5],[var-1],[var-1],[var-1]],
                                     [[val-0],[val-2],[val-4],[val-6],[val-8]])).

ctr_draw_example(group_skip_isolated_item,
                 ['VARIABLES'],
                 [[[var-2],[var-8],[var-1],[var-7],[var-4],
                   [var-5],[var-1],[var-1],[var-1]]],
                 ['CHAIN'],
                 [1-2,2-1],
                 ['NSCC'([[1,2]]),'NVERTEX'],
                 '','NSCC=1\\nMIN_NSCC=2\\nMAX_NSCC=2\\nNVERTEX=2',
                 [1.8,3.5,1.7,1.7]).

ctr_see_also(group_skip_isolated_item,
 [link('common keyword',            group,             '%k,%k', ['timetabling constraint', 'sequence']),
  link('common keyword',            change_continuity, '%k,%k', ['timetabling constraint', 'sequence']),
  link('common keyword',            stretch_path,      '%k,%k', ['timetabling constraint', 'sequence']),
  link('used in graph description', in,                '',      [])]).

ctr_key_words(group_skip_isolated_item,['timetabling constraint'             ,
                                        'sequence'                           ,
                                        'strongly connected component'       ,
                                        'automaton'                          ,
                                        'automaton with counters'            ,
                                        'automaton with same input symbol'   ,
                                        'reverse of a constraint'            ,
		                        'glue matrix'                        ,
                                        'alpha-acyclic constraint network(2)',
                                        'alpha-acyclic constraint network(3)',
                                        'functional dependency'              ]).

ctr_eval(group_skip_isolated_item, [automata(group_skip_isolated_item_a)]).

group_skip_isolated_item_a(NGROUP, MIN_SIZE, MAX_SIZE, NVAL, VARIABLES, VALUES) :-
    check_type(dvar, NGROUP),
    check_type(dvar, MIN_SIZE),
    check_type(dvar, MAX_SIZE),
    check_type(dvar, NVAL),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int]),
    length(VARIABLES, N),
    get_attr1(VALUES, VALS),
    NGROUP   #>= 0,
    MIN_SIZE #>= 0,
    MIN_SIZE #\= 1,
    MAX_SIZE #>= MIN_SIZE,
    MAX_SIZE #\= 1,
    NVAL     #>= MAX_SIZE,
    NVAL     #>= NGROUP,
    NVAL     #=< N,
    all_different(VALS),
    get_attr1(VALUES, LIST_VALUES),
    list_to_fdset(LIST_VALUES, SET_OF_VALUES),
    group_skip_isolated_item_signature(VARIABLES, SIGNATURE, SET_OF_VALUES),
    group_skip_isolated_item_ngroup(NGROUP, SIGNATURE),
    group_skip_isolated_item_min_size(MIN_SIZE, N, SIGNATURE),
    group_skip_isolated_item_max_size(MAX_SIZE, SIGNATURE),
    group_skip_isolated_item_nval(NVAL, SIGNATURE).

group_skip_isolated_item_ngroup(NGROUP, SIGNATURE) :-
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(i),sink(j),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,i      ),
               arc(i,0,s      ),
               arc(i,1,j,[C+1]),
               arc(j,1,j      ),
               arc(j,0,s      )],
              [C],[0],[NGROUP]).

group_skip_isolated_item_min_size(MIN_SIZE, NVAR, SIGNATURE) :-
    MIN_SIZE #= min(C1,D1),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(t),sink(r),sink(s)],
              [arc(s,0,s               ),
               arc(s,1,t               ),
               arc(t,0,s               ),
               arc(t,1,r,[C    ,2     ]),
               arc(r,0,s,[min(C,D),D  ]),
               arc(r,1,r,[C       ,D+1])],
              [C,D],[NVAR,0],[C1,D1]).

group_skip_isolated_item_max_size(MAX_SIZE, SIGNATURE) :-
    MAX_SIZE #= max(C1,D1),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(t),sink(r),sink(s)],
              [arc(s,0,s                 ),
               arc(s,1,t                 ),
               arc(t,0,s                 ),
               arc(t,1,r,[max(C,2),2]    ),
               arc(r,0,s                 ),
               arc(r,1,r,[max(C,D+1),D+1])],
              [C,D],[0,0],[C1,D1]).

group_skip_isolated_item_nval(NVAL, SIGNATURE) :-
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[NVAL]).

group_skip_isolated_item_signature([], [], _).
group_skip_isolated_item_signature([[var-VAR]|VARs], [S|Ss], SET_OF_VALUES) :-
    VAR in_set SET_OF_VALUES #<=> S,
    group_skip_isolated_item_signature(VARs, Ss, SET_OF_VALUES).
