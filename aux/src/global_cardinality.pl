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

ctr_date(global_cardinality,['20030820','20040530','20060809','20091218']).

ctr_origin(global_cardinality, '\\index{CHARME|indexuse}CHARME \\cite{OplobeduMarcovitchTourbier89}', []).

ctr_arguments(global_cardinality,
              ['VARIABLES'-collection(var-dvar),
               'VALUES'-collection(val-int, noccurrence-dvar)]).

ctr_exchangeable(global_cardinality,
                 [items('VARIABLES',all),
                  items('VALUES',all),
                  vals(['VARIABLES'^var],all(notin('VALUES'^val)),=,dontcare,dontcare),
                  vals(['VARIABLES'^var,'VALUES'^val],int,=\=,all,dontcare)]).

ctr_restrictions(global_cardinality,
                 [required('VARIABLES',var)                ,
                  required('VALUES',[val,noccurrence])     ,
                  distinct('VALUES',val)                   ,
                  'VALUES'^noccurrence >= 0                ,
                  'VALUES'^noccurrence =< size('VARIABLES')]).

ctr_typical(global_cardinality,
            [size('VARIABLES')           > 1              ,
             range('VARIABLES'^var)      > 1              ,
             size('VALUES')              > 1              ,
             size('VARIABLES')           >= size('VALUES'),
             minval('VARIABLES'^var)=0 #\/ in_attr('VARIABLES',var,'VALUES',val)]).

ctr_pure_functional_dependency(global_cardinality, []).
ctr_functional_dependency(global_cardinality, 2-2, [1,2-1]).

ctr_contractible(global_cardinality, [], 'VALUES', any).

ctr_synonyms(global_cardinality,[count                      ,
                                 distribute                 ,
                                 distribution               ,
                                 gcc                        ,
                                 card_var_gcc               ,
                                 egcc                       ,
                                 extended_global_cardinality]).

ctr_graph(global_cardinality,
          ['VARIABLES'],
          1,
          foreach('VALUES',['SELF'>>collection(variables)]),
          [variables^var = 'VALUES'^val],
          ['NVERTEX' = 'VALUES'^noccurrence],
          []).

ctr_example(global_cardinality,
            global_cardinality([[var-3],[var-3],[var-8],[var-6]],
                               [[val-3, noccurrence-2],
                                [val-5, noccurrence-0],
                                [val-6, noccurrence-1]])).

ctr_draw_example(global_cardinality,
                 ['VARIABLES'],
                 [[[var-3],[var-3],[var-8],[var-6]]],
                 ['SELF'],
                 [1-1,2-2,4-4],
                 ['NVERTEX',
                  'FOREACH'('VALUES',[3-[1,2],5-[],6-[4]])],
                 '','3:NVERTEX=2, 5:NVERTEX=0, 6:NVERTEX=1',
                 [2.145,2.145,2.145,2.145]).

ctr_cond_imply(global_cardinality, and,                        [minval('VARIABLES'^var) = 0], ['VAR' = 0],                     [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, or,                         [maxval('VARIABLES'^var) = 1], ['VAR' = 1],                     [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, min_size_full_zero_stretch, [minval('VARIABLES'^var) > 0], ['MINSIZE' = size('VARIABLES')], [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, min_size_full_zero_stretch, [maxval('VARIABLES'^var) < 0], ['MINSIZE' = size('VARIABLES')], [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, among_diff_0,
               [range('VALUES'^val) = nval('VALUES'^val),
                minval('VALUES'^val) =< minval('VARIABLES'^var),
                maxval('VALUES'^val) >= maxval('VARIABLES'^var)], [], [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, atmost_nvalue,
               [range('VALUES'^val) = nval('VALUES'^val),
                minval('VALUES'^val) =< minval('VARIABLES'^var),
                maxval('VALUES'^val) >= maxval('VARIABLES'^var)], [], [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, balance,
               [range('VALUES'^noccurrence) = 1,
		range('VALUES'^val) = nval('VALUES'^val),
		minval('VALUES'^val) = minval('VARIABLES'^var),
		maxval('VALUES'^val) = maxval('VARIABLES'^var)], ['BALANCE' = 0], [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, max_n,
               [range('VALUES'^val) = nval('VALUES'^val),
                minval('VALUES'^val) =< minval('VARIABLES'^var),
                maxval('VALUES'^val) >= maxval('VARIABLES'^var)], [], [none, none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, max_nvalue,
               [range('VALUES'^val) = nval('VALUES'^val),
                minval('VALUES'^val) =< minval('VARIABLES'^var),
                maxval('VALUES'^val) >= maxval('VARIABLES'^var)], [], [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, min_n,
               [range('VALUES'^val) = nval('VALUES'^val),
                minval('VALUES'^val) =< minval('VARIABLES'^var),
                maxval('VALUES'^val) >= maxval('VARIABLES'^var)], [], [none, none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, min_nvalue,
               [range('VALUES'^val) = nval('VALUES'^val),
                minval('VALUES'^val) =< minval('VARIABLES'^var),
                maxval('VALUES'^val) >= maxval('VARIABLES'^var)], [], [none, 'VARIABLES']).
ctr_cond_imply(global_cardinality, range_ctr,
               [range('VALUES'^val) = nval('VALUES'^val),
                minval('VALUES'^val) =< minval('VARIABLES'^var),
                maxval('VALUES'^val) >= maxval('VARIABLES'^var)], [], ['VARIABLES', none, none]).

ctr_see_also(global_cardinality,
 [link('soft variant',                  open_global_cardinality,        'a %e %e defines the set of variables that are actually considered',
                                                                        [set, variable]),
  link('specialisation',                alldifferent,                   'each value should occur at most once',
                                                                        []),
  link('specialisation',                global_cardinality_low_up,      '%e replaced by %e %e',
                                                                        [variable, fixed, interval]),
  link('specialisation',                cardinality_atleast,            'individual %e for each value replaced by single %e',
                                                                        ['count~variable','count~variable']),
  link('specialisation',                cardinality_atmost,             'individual %e for each value replaced by single %e',
                                                                        ['count~variable','count~variable']),
  link('specialisation',                cardinality_atmost_partition,   'individual %e for each value replaced by single %e and %e replaced by %e',
                                                                        ['count~variable','count~variable',in_list(variable,partition),variable]),
  link('cost variant',                  global_cardinality_with_costs,  '%e associated with each %e,%e pair',
                                                                        [cost, variable, value]),
  link('implied by',                    global_cardinality_with_costs,  'forget about cost',
                                                                        []),
  link('implied by',                    same_and_global_cardinality,    'conjoin %c and %c',
                                                                        [same, global_cardinality]),
  link('system of constraints',         colored_matrix,                 'one %c constraint for each %e and each %e of a %e of %e',
                                                                        [global_cardinality, row, column, matrix, variables]),
  link('part of system of constraints', among,                          '',
                                                                        []),
  link('shift of concept',              symmetric_gcc,                  '',
                                                                        []),
  link('shift of concept',              symmetric_cardinality,          '',
                                                                        []),
  link('shift of concept',              global_cardinality_no_loop,     'assignment of a %e to its position is ignored',
                                                                        [variable]),
  link('shift of concept',              ordered_global_cardinality,     'restrictions are done on nested sets of values, all starting from first value',
                                                                        []),
  link('common keyword',                count,                          '%k,%k',
                                                                        ['value constraint', 'counting constraint']),
  link('common keyword',                max_nvalue,                     '%k,%k',
                                                                        ['value constraint', 'counting constraint']),
  link('common keyword',                min_nvalue,                     '%k,%k',
                                                                        ['value constraint', 'counting constraint']),
  link('common keyword',                nvalue,                         '%k',
                                                                        ['counting constraint'], '\\\\ '),
  link('common keyword',                open_global_cardinality_low_up, '%k,%k',
                                                                        ['assignment', 'counting constraint']),
  link('related',                       roots,                          '',
                                                                        []),
  link('related',                       sliding_card_skip0,             '%k of a set of values on maximal sequences',
                                                                        ['counting constraint']),
  link('uses in its reformulation',     tree_range,                     '',
                                                                        []),
  link('uses in its reformulation',     tree_resource,                  '',
                                                                        [])]).

ctr_key_words(global_cardinality,['core'                            ,
                                  'value constraint'                ,
                                  'counting constraint'             ,
                                  'assignment'                      ,
                                  'functional dependency'           ,
		                  'pure functional dependency'      ,
                                  'magic series'                    ,
                                  'autoref'                         ,
                                  'Hall interval'                   ,
                                  'bound-consistency'               ,
                                  '3-SAT'                           ,
                                  'flow'                            ,
                                  'duplicated variables'            ,
                                  'automaton'                       ,
                                  'automaton with array of counters',
                                  'DFS-bottleneck'                  ,
                                  'system of constraints'           ]).

ctr_persons(global_cardinality,['Oplobedu A.'       ,
                                'Marcovitch J.'     ,
                                'Tourbier Y.'       ,
                                'Van Hentenryck P.' ,
		                'R\\\'egin J.-C.'   ,
                                'Bourdais S.'       ,
                                'Galinier P.'       ,
                                'Pesant G.'         ,
                                'Katriel I.'        ,
                                'Thiel S.'          ,
                                'Quimper C.-G.'     ,
                                'van Beek P.'       ,
                                'L\\\'opez-Ortiz A.',
                                'Golynski A.'       ,
                                'Sadjad S. B.'      ,
                                'van Hoeve W.-J.'   ,
                                'Rousseau L.-M.'    ,
                                'Pitrat J.'         ]).

ctr_eval(global_cardinality, [builtin(global_cardinality_b),
			      checker(global_cardinality_c)]).

global_cardinality_b(VARIABLES, VALUES) :-
    length(VARIABLES, N),
    collection(VARIABLES, [dvar]),
    collection(VALUES, [int,dvar(0,N)]),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),
    get_attr2(VALUES, NOCCS),
    all_different(VALS),
    get_minimum(VARS, MINVARS),
    get_maximum(VARS, MAXVARS),
    get_minimum(VALS, MINVALS),
    get_maximum(VALS, MAXVALS),
    MIN is min(MINVARS,MINVALS),
    MAX is max(MAXVARS,MAXVALS),
    complete_card(MIN, MAX, N, VALS, NOCCS, VN),
    global_cardinality(VARS, VN).

global_cardinality_c(VARIABLES, VALUES) :-
    length(VARIABLES, N),
    collection(VARIABLES, [int]),
    collection(VALUES, [int,int(0,N)]),
    get_attr1(VARIABLES, VARS),
    get_attr1(VALUES, VALS),
    sort(VALS, SVALS),
    length(VALS, M),
    length(SVALS, M),
    create_pairs(VARS, PVARS),
    keysort(PVARS, SVARS),
    get_attr12(VALUES, VALOCCS),
    keysort(VALOCCS, SVALOCCS),
    global_cardinality_c1(SVARS, SVALOCCS).

global_cardinality_c1(_, []) :- !.          % no constraint on values
global_cardinality_c1([], [_-0|R]) :- !,    % empty list of variable: remaining occurrences are all set to 0
    global_cardinality_c1([], R).
global_cardinality_c1([U-U|R], [V-O|S]) :-  % U<V: so U is not mentionned in the values, we skip it
    U < V, !,
    global_cardinality_c1(R, [V-O|S]).
global_cardinality_c1([U-U|R], [V-0|S]) :-  % U>V: so V is not mentionned in the sequence of variables, it occ. should be 0
    U > V, !,
    global_cardinality_c1([U-U|R], S).
global_cardinality_c1([U-U|R], [U-1|S]) :-  % both values coincide and only one occurrence: skip both
    !,
    global_cardinality_c1(R, S).
global_cardinality_c1([U-U|R], [U-O|S]) :-  % both values coincide but need more than one occurrence: decrement number of occurrences
    O1 is O-1,
    global_cardinality_c1(R, [U-O1|S]).
