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

ctr_date(pattern,['20031008','20090717']).

ctr_origin(pattern, '\\cite{BourdaisGalinierPesant03}', []).

ctr_types(pattern,
          ['PATTERN'-collection(var-int)]).

ctr_arguments(pattern,
              ['VARIABLES'-collection(var-dvar),
               'PATTERNS'-collection(pat-'PATTERN')]).

ctr_exchangeable(pattern,
                 [items('PATTERNS',all),
                  items_sync('VARIABLES','PATTERNS'^pat,reverse),
                  vals(['VARIABLES'^var,'PATTERNS'^pat^var],int,=\=,all,dontcare)]).

ctr_restrictions(pattern,
                 [required('PATTERN',var)  ,
                  'PATTERN'^var >= 0       ,
                  change(0,'PATTERN',=)    ,
                  size('PATTERN') > 1      ,
                  required('VARIABLES',var),
                  required('PATTERNS',pat) ,
                  size('PATTERNS') > 0     ,
                  same_size('PATTERNS',pat)]).

ctr_typical(pattern,
            [size('VARIABLES')      > 2,
             range('VARIABLES'^var) > 1]).

ctr_contractible(pattern, [], 'VARIABLES', prefix).
ctr_contractible(pattern, [], 'VARIABLES', suffix).

ctr_example(pattern,
            pattern([[var-1],[var-1],[var-2],[var-2],
                     [var-2],[var-1],[var-3],[var-3]],
                    [[pat-[[var-1], [var-2], [var-1]]],
                     [pat-[[var-1], [var-2], [var-3]]],
                     [pat-[[var-2], [var-1], [var-3]]]])).

ctr_see_also(pattern,
 [link('common keyword', stretch_circuit,        '%k,%k', ['sliding sequence constraint', 'timetabling constraint']),
  link('common keyword', stretch_path,           '%k,%k', ['sliding sequence constraint', 'timetabling constraint'], '\\\\ '),
  link('common keyword', sliding_distribution,   '%k',    ['sliding sequence constraint'], '\\\\ '),
  link('common keyword', stretch_path_partition, '%k',    ['sliding sequence constraint']),
  link('common keyword', group,                  '%k',    ['timetabling constraint'], '\\\\ ')]).

ctr_key_words(pattern,['timetabling constraint'          ,
                       'sliding sequence constraint'     ,
                       'arc-consistency'                 ,
		       'Berge-acyclic constraint network',
		       'automaton'                       ,
		       'automaton without counters'      ,
                       'reified automaton constraint'    ]).

ctr_persons(pattern,['Bourdais S.',
                     'Galinier P.',
                     'Pesant G.'  ]).

ctr_eval(pattern, [automaton(pattern_a)]).

pattern_a(FLAG, VARIABLES, PATTERNS) :-
    collection(VARIABLES, [dvar]),
    collection(PATTERNS, [col([int_gteq(0)])]),
    same_size(PATTERNS),
    length(PATTERNS, NPATTERNS),
    NPATTERNS > 0,
    get_attr1(VARIABLES, VARS),
    get_col_attr1(PATTERNS, 1, PATTS),
    PATTS = [PATT|_],
    length(PATT, K),
    K > 1,
    pattern_change(PATTERNS),
    remove_duplicates(PATTS, PATTS_NO_DUPLICATES),
    pattern_build_tree(PATTS_NO_DUPLICATES, ID_PATTS, node(-1-0, []), 1, _, TREE),
    flattern(PATTS_NO_DUPLICATES, FLAT_PATTS),
    remove_duplicates(FLAT_PATTS, VALUES),
    pattern_next(ID_PATTS, ID_PATTS, VALUES, ADDITIONAL_TRANSITIONS),
    pattern_gen_states(TREE, STATES, TRANSITIONS),
    append(TRANSITIONS, ADDITIONAL_TRANSITIONS, ALL_TRANSITIONS),
    AUTOMATON = automaton(VARS, _, VARS, STATES, ALL_TRANSITIONS, [], [], []),
    automaton_bool(FLAG, VALUES, AUTOMATON).

pattern_gen_states(node(-1-0, LIST_SUNS), [source(NAME),sink(NAME)|R], TRANSITIONS) :- !,
    number_codes(-1, CODE),
    atom_codes(ATOM, CODE),
    atom_concat('s', ATOM, NAME),
    pattern_gen_states1(LIST_SUNS, -1, R, TRANSITIONS).
pattern_gen_states(node(ID-_VAL, LIST_SUNS), [sink(NAME)|R], TRANSITIONS) :-
    ID >= 0,
    number_codes(ID, IDCODE),
    atom_codes(IDATOM, IDCODE),
    atom_concat('s', IDATOM, NAME),
    pattern_gen_states1(LIST_SUNS, ID, R, TRANSITIONS).

pattern_gen_states1([], _, [], []).
pattern_gen_states1([N|R], ID1, ST, TRANSITIONS) :-
    N = node(ID2-VAL2,_),
    pattern_gen_states(N, S, TRANSITIONS1),
    pattern_gen_states1(R, ID1, T, TRANSITIONS2),
    append(S, T, ST),
    number_codes(ID1, IDCODE1), atom_codes(IDATOM1, IDCODE1), atom_concat('s', IDATOM1, IDNAME1),
    number_codes(ID2, IDCODE2), atom_codes(IDATOM2, IDCODE2), atom_concat('s', IDATOM2, IDNAME2),
    append([arc(IDNAME1,VAL2,IDNAME2),arc(IDNAME2,VAL2,IDNAME2)], TRANSITIONS1, T1),
    append(T1, TRANSITIONS2, TRANSITIONS).

pattern_change([]).
pattern_change([[_-P]|R]) :-
    eval(change(0, P, =)),
    pattern_change(R).

pattern_next([], _, _, []).
pattern_next([PID-P|R], ID_PATTS, VALUES, TRANSITIONS) :-
    P = [_|RP],
    pattern_next1(VALUES, PID, RP, ID_PATTS, TRANSITIONS1),
    pattern_next(R, ID_PATTS, VALUES, TRANSITIONS2),
    append(TRANSITIONS1, TRANSITIONS2, TRANSITIONS).

pattern_next1([], _, _, _, []).
pattern_next1([V|R], PID, RP, ID_PATTS, [arc(PIDNAME,V,NEWPIDNAME)|S]) :-
    append(RP, [V], NEWP),
    pattern_search(ID_PATTS, NEWP, NEWPID),
    number_codes(   PID,    PIDCODE), atom_codes(   PIDATOM,    PIDCODE), atom_concat('s',    PIDATOM,    PIDNAME),
    number_codes(NEWPID, NEWPIDCODE), atom_codes(NEWPIDATOM, NEWPIDCODE), atom_concat('s', NEWPIDATOM, NEWPIDNAME),
    !,
    pattern_next1(R, PID, RP, ID_PATTS, S).
pattern_next1([_|R], PID, RP, ID_PATTS, S) :-
    pattern_next1(R, PID, RP, ID_PATTS, S).

pattern_search([ID-PAT|_], PAT, ID) :-
    !.
pattern_search([_|R], PAT, ID) :-
    pattern_search(R, PAT, ID).

pattern_build_tree([], [], TREE, NODE_ID, NODE_ID, TREE).
pattern_build_tree([PATTERN|R], [PATTERN_ID-PATTERN|S], OLD_TREE, OLD_NODE_ID, NEW_NODE_ID, NEW_TREE) :-
    pattern_insert(PATTERN, OLD_TREE, OLD_NODE_ID, CUR_NODE_ID, CUR_TREE, PATTERN_ID),
    pattern_build_tree(R, S, CUR_TREE, CUR_NODE_ID, NEW_NODE_ID, NEW_TREE).

pattern_insert([], TREE, NODE_ID, NODE_ID, TREE, _).
pattern_insert([I|R], OLD_TREE, OLD_NODE_ID, NEW_NODE_ID, node(LABEL, NEW_TREE), PATTERN_ID) :-
    OLD_TREE = node(LABEL, LIST_NODES),
    pattern_occurs(I, LIST_NODES, [], BEFORE, SUBTREE, AFTER),
    !,
    pattern_insert(R, SUBTREE, OLD_NODE_ID, NEW_NODE_ID, NEW_SUBTREE, PATTERN_ID),
    append(BEFORE, [NEW_SUBTREE], TEMPO_TREE),
    append(TEMPO_TREE, AFTER, NEW_TREE).
pattern_insert([I|R], node(LABEL, LIST_NODES), OLD_NODE_ID, NEW_NODE_ID, node(LABEL, [BRANCH|LIST_NODES]), PATTERN_ID) :-
    pattern_create_branch([I|R], OLD_NODE_ID, NEW_NODE_ID, BRANCH, PATTERN_ID).

pattern_create_branch([I], OLD_NODE_ID, NEW_NODE_ID, node(OLD_NODE_ID-I,[]), OLD_NODE_ID) :- !,
    NEW_NODE_ID is OLD_NODE_ID+1.
pattern_create_branch([I,J|R], OLD_NODE_ID, NEW_NODE_ID, node(OLD_NODE_ID-I, [S]), PATTERN_ID) :-
    CUR_NODE_ID is OLD_NODE_ID+1,
    pattern_create_branch([J|R], CUR_NODE_ID, NEW_NODE_ID, S, PATTERN_ID).

pattern_occurs(I, [node(Id-I,L)|AFTER], BEFORE, BEFORE, node(Id-I,L), AFTER) :-
    !.
pattern_occurs(I, [NODE|AFTER_CUR], BEFORE_CUR, BEFORE, NODE_FOUND, AFTER) :-
    pattern_occurs(I, AFTER_CUR, [NODE|BEFORE_CUR], BEFORE, NODE_FOUND, AFTER).
