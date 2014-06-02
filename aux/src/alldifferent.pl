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

ctr_date(alldifferent,['20000128','20030820','20040530','20060803','20081227','20090521']).

ctr_origin(alldifferent, '\\cite{Lauriere78}', []).

ctr_arguments(alldifferent,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(alldifferent,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int,=\=,all,dontcare)]).

ctr_synonyms(alldifferent,[alldiff           ,
                           alldistinct       ,
                           distinct          ,
                           bound_alldifferent,
                           bound_alldiff     ,
                           bound_distinct    ,
                           rel               ]).

ctr_restrictions(alldifferent,
                 [required('VARIABLES',var)]).

ctr_typical(alldifferent,
            [size('VARIABLES') > 2]).

ctr_contractible(alldifferent, [], 'VARIABLES', any).

ctr_graph(alldifferent,
          ['VARIABLES'],
          2,
          ['CLIQUE'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          ['MAX_NSCC' =< 1],
          ['ONE_SUCC']).

ctr_example(alldifferent,
            alldifferent([[var-5],[var-1],[var-9],[var-3]])).

ctr_draw_example(alldifferent,
                 ['VARIABLES'],
                 [[[var-5],[var-1],[var-9],[var-3]]],
                 ['CLIQUE'],
                 [1-1,2-2,3-3,4-4],
                 ['MAX_NSCC'([1])],
                 '','MAX_NSCC=1',
                 []).

ctr_cond_imply(alldifferent, lex_alldifferent,       [], [],                                                      int_to_col               ).
ctr_cond_imply(alldifferent, soft_alldifferent_ctr,  [], [],                                                      [none, 'VARIABLES']      ).
ctr_cond_imply(alldifferent, balance,                [], ['BALANCE' = 0],                                         [none, 'VARIABLES']      ).
ctr_cond_imply(alldifferent, soft_all_equal_max_var, [], ['N' < size('VARIABLES')],                               [none, 'VARIABLES']      ).
ctr_cond_imply(alldifferent, soft_all_equal_min_var, [], ['N' > size('VARIABLES')],                               [none, 'VARIABLES']      ).
ctr_cond_imply(alldifferent, change,                 [], ['NCHANGE' = size('VARIABLES')-1, in_list('CTR',[=\=])], [none, 'VARIABLES', none]).
ctr_cond_imply(alldifferent, circular_change,        [], ['NCHANGE' = size('VARIABLES'), in_list('CTR',[=\=])],   [none, 'VARIABLES', none]).
ctr_cond_imply(alldifferent, longest_change,         [], ['SIZE' = size('VARIABLES'), in_list('CTR',[=\=])],      [none, 'VARIABLES', none]).
ctr_cond_imply(alldifferent, length_first_sequence,  [size('VARIABLES') > 0], ['LEN' = 1],                        [none, 'VARIABLES']      ).
ctr_cond_imply(alldifferent, length_last_sequence,   [size('VARIABLES') > 0], ['LEN' = 1],                        [none, 'VARIABLES']      ).
ctr_cond_imply(alldifferent, min_nvalue,             [size('VARIABLES') > 0], ['MIN' = 1],                        [none, 'VARIABLES']      ).

ctr_see_also(alldifferent,
 [link('negation',                      some_equal,                                   '',                                                                       []),
  link('generalisation',                alldifferent_cst,                             '%e replaced by %e',                                                      [variable,variable+constant]),
  link('generalisation',                alldifferent_interval,                        '%e replaced by %e',                                                      [variable,variable/constant]),
  link('generalisation',                alldifferent_modulo,                          '%e replaced by %e',                                                      [variable,variable mod constant]),
  link('generalisation',                alldifferent_partition,                       '%e replaced by %e',                                                      [variable,in_list(variable,partition)]),
  link('generalisation',                alldifferent_between_sets,                    '%e replaced by %e',                                                      [variable,'set~variable']),
  link('generalisation',                all_min_dist,                                 '%e replaced by %e, all of the same size',                                [variable,'line~segment']),
  link('generalisation',                disjunctive,                                  '%e replaced by %e',                                                      [variable,task]),
  link('generalisation',                global_cardinality,                           'control the number of occurrence of each value with a counter variable', []),
  link('generalisation',                global_cardinality_low_up,                    'control the number of occurrence of each value with an interval',        []),
  link('generalisation',                lex_alldifferent,                             '%e replaced by %e',                                                      [variable,vector]),
  link('generalisation',                nvalue,                                       'count number of distinct values',                                        []),
  link('generalisation',                diffn,                                        '%e replaced by %k',                                                      [variable,orthotope]),
  link('soft variant',                  soft_alldifferent_var,                        '%k',                                                                     ['variable-based violation measure']),
  link('soft variant',                  soft_alldifferent_ctr,                        '%k',                                                                     ['decomposition-based violation measure']),
  link('soft variant',                  open_alldifferent,                            '%k',                                                                     ['open constraint']),
  link('soft variant',                  alldifferent_except_0,                        'value %e can be used several times',                                     [0]),
  link('cost variant',                  minimum_weight_alldifferent,                  '',                                                                       []),
  link('cost variant',                  weighted_partial_alldiff,                     '',                                                                       []),
  link('common keyword',                circuit,                                      '%k',                                                                     [permutation]),
  link('common keyword',                proper_circuit,                               '%k',                                                                     [permutation]),
  link('common keyword',                cycle,                                        '%k',                                                                     [permutation]),
  link('common keyword',                derangement,                                  '%k',                                                                     [permutation]),
  link('common keyword',                symmetric_alldifferent,                       '%k',                                                                     [permutation]),
  link('common keyword',                circuit_cluster,                              '%k',                                                                     [permutation]),
  link('common keyword',                golomb,                                       '%k',                                                                     ['all different']),
  link('common keyword',                size_max_seq_alldifferent,                    '%k,%k',                                                                  ['all different', 'disequality']),
  link('common keyword',                size_max_starting_seq_alldifferent,           '%k,%k',                                                                  ['all different', 'disequality']),
  link('system of constraints',         k_alldifferent,                               '',                                                                       []),
  link('part of system of constraints', neq,                                          '',                                                                       []),
  link('implies',                       alldifferent_except_0,                        '',                                                                       []),
  link('implies',                       multi_global_contiguity,                      '',                                                                       []),
  link('implies',                       not_all_equal,                                '',                                                                       []),
  link('implied by',                    alldifferent_consecutive_values,              '',                                                                       []),
  link('implied by',                    circuit,                                      '',                                                                       []),
  link('implied by',                    cycle,                                        '',                                                                       []),
  link('implied by',                    strictly_increasing,                          '',                                                                       []),
  link('implied by',                    strictly_decreasing,                          '',                                                                       []),
  link('shift of concept',              alldifferent_on_intersection,                 '',                                                                       []),
  link('shift of concept',              alldifferent_same_value,                      '',                                                                       []),
  link('used in reformulation',         in_interval_reified,                          '%k preserving reformulation',                                            ['bound-consistency']),
  link('used in reformulation',         sort,                                         '',                                                                       []),
  link('used in reformulation',         strictly_increasing,                          '',                                                                       []),
  link('uses in its reformulation',     cycle,                                        '',                                                                       []),
  link('uses in its reformulation',     elements_alldifferent,                        '',                                                                       []),
  link('uses in its reformulation',     sort_permutation,                             '',                                                                       [])]).

ctr_key_words(alldifferent,['core'                                         ,
                            'system of constraints'                        ,
                            'value constraint'                             ,
                            'permutation'                                  ,
                            'all different'                                ,
                            'disequality'                                  ,
			    'sort based reformulation'                     ,
                            'bipartite matching'                           ,
                            'bipartite matching in convex bipartite graphs',
                            'convex bipartite graph'                       ,
                            'flow'                                         ,
                            'maximum clique'                               ,
                            'n-Amazons'                                    ,
                            'n-queens'                                     ,
                            'Costas arrays'                                ,
                            'Euler knight'                                 ,
                            'Golomb ruler'                                 ,
                            'magic hexagon'                                ,
                            'magic square'                                 ,
                            'zebra puzzle'                                 ,
                            'Sudoku'                                       ,
                            'graph colouring'                              ,
                            'Hall interval'                                ,
                            'arc-consistency'                              ,
                            'bound-consistency'                            ,
                            'automaton'                                    ,
                            'automaton with array of counters'             ,
                            'one\\_succ'                                   ,
                            'SAT'                                          ,
                            'DFS-bottleneck'                               ,
                            'entailment'                                   ]).

ctr_persons(alldifferent,['Lauri\\`ere J.-L.'   ,
                          'Costa M.-C.'         ,
                          'R\\\'egin J.-C.'     ,
                          'Petersen J.'         ,
                          'Berge C.'            ,
                          'Leconte M.'          ,
                          'Bleuzen-Guernalec N.',
                          'Colmerauer A.'       ,
                          'Puget J.-F.'         ,
                          'L\\\'opez-Ortiz A.'  ,
                          'Quimper C.-G.'       ,
                          'van Beek P.'         ,
                          'Mehlhorn K.'         ,
                          'Thiel S.'            ,
                          'van Hoeve W.-J.'     ,
                          'Glover F.'           ,
                          'Asratian A. S.'      ,
                          'Denley T. M. J.'     ,
                          'H\\"aggkvist R.'     ,
                          'Williams H. P.'      ,
                          'Yan H.'              ,
                          'Hooker J. N.'        ,
                          'Zanarini A.'         ,
                          'Pesant G.'           ,
                          'Valiant L. G.'       ,
                          'Sloane N. J. A.'     ,
                          'Gent I. P.'          ,
                          'Nightingale P.'      ,
                          'Vellino A.'          ,
                          'Bron C.'             ,
                          'Kerbosch J.'         ,
                          'Eppstein D.'         ,
                          'Strash D.'           ,
                          'Bessi\\`ere C.'      ,
                          'Katsirelos G.'       ,
                          'Narodytska N.'       ,
                          'Walsh T.'            ,
                          'Rochart G.'          ,
                          'Jussien N.'          ,
                          'Barichard V.'        ,
                          'Roy B.'              ,
                          'Watkins J. J.'       ,
			  'Gauss K. F.'         ,
			  'D\\"urr C.'          ,
			  'Cymer R.'            ]).

ctr_eval(alldifferent, [checker(alldifferent_c),
			builtin(alldifferent_b),
			reformulation(alldifferent_r1),
                        reformulation(alldifferent_r2)]).

ctr_sol(alldifferent,2,0,2,6,-).
ctr_sol(alldifferent,3,0,3,24,-).
ctr_sol(alldifferent,4,0,4,120,-).
ctr_sol(alldifferent,5,0,5,720,-).
ctr_sol(alldifferent,6,0,6,5040,-).
ctr_sol(alldifferent,7,0,7,40320,-).
ctr_sol(alldifferent,8,0,8,362880,-).
ctr_sol(alldifferent,9,0,9,3628800,-).
ctr_sol(alldifferent,10,0,10,39916800,-).

alldifferent_c([V,V|_]) :-
	!,
	fail.
alldifferent_c(VARIABLES) :-
	collection(VARIABLES, [int]),
	get_attr1(VARIABLES, VARS),
	sort(VARS, SVARS),
	length(VARS, N),
	length(SVARS, N).

alldifferent_b(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	all_distinct(VARS).

alldifferent_r1(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	get_minimum(VARS, MIN),
	get_maximum(VARS, MAX),
	length(VARS, N),
	length(L, N),
	domain(L, MIN, MAX),
	gen_collection(L, var, SORTED_VARIABLES),
	eval(sort(VARIABLES, SORTED_VARIABLES)),
	eval(strictly_increasing(SORTED_VARIABLES)).

alldifferent_r2(VARIABLES) :-
	collection(VARIABLES, [dvar]),
	get_attr1(VARIABLES, VARS),
	get_minimum(VARS, MIN),
	get_maximum(VARS, MAX),
	alldifferent_r20(MIN, MAX, VARS).

alldifferent_r20(L, MAX, _) :-
    L > MAX, !.
alldifferent_r20(L, MAX, VARS) :-
    alldifferent_r21(L, MAX, VARS),
    L1 is L+1,
    alldifferent_r20(L1, MAX, VARS).

alldifferent_r21(L, U, _) :-
    L > U, !.
alldifferent_r21(L, U, VARS) :-
    alldifferent_r22(VARS, L, U, T),
    S is U-L+1,
    call(T #=< S),
    U1 is U-1,
    alldifferent_r21(L, U1, VARS).

alldifferent_r22([], _, _, 0) :- !.
alldifferent_r22([Vi|R], L, U, Bilu+S) :-
    Vi in L..U #<=> Bilu,
    alldifferent_r22(R, L, U, S).
