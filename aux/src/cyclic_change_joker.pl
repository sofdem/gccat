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

:-dynamic cyclic_change_joker_a/4.

ctr_date(cyclic_change_joker,['20000128','20030820','20040530','20060807']).

ctr_origin(cyclic_change_joker, 'Derived from %c.', [cyclic_change]).

ctr_arguments(cyclic_change_joker,
              ['NCHANGE'-dvar                  ,
               'CYCLE_LENGTH'-int              ,
               'VARIABLES'-collection(var-dvar),
               'CTR'-atom                      ]).

ctr_exchangeable(cyclic_change_joker,
                 [items('VARIABLES',shift)]).

ctr_restrictions(cyclic_change_joker,
                 ['NCHANGE'       >= 0                ,
                  'NCHANGE'       <  size('VARIABLES'),
                  'CYCLE_LENGTH'  >  0                ,
                  required('VARIABLES',var)           ,
                  'VARIABLES'^var >= 0                ,
                  in_list('CTR',[=,=\=,<,>=,>,=<])    ]).

ctr_typical(cyclic_change_joker,
            ['NCHANGE'               >  0             ,
             'CYCLE_LENGTH'          >  1             ,
             size('VARIABLES')       >  1             ,
             range('VARIABLES'^var)  >  1             ,
             maxval('VARIABLES'^var) >= 'CYCLE_LENGTH',
             in_list('CTR',[=\=])                     ]).

ctr_pure_functional_dependency(cyclic_change_joker, []).
ctr_functional_dependency(cyclic_change_joker, 1, [2,3,4]).

ctr_graph(cyclic_change_joker,
          ['VARIABLES'],
          2,
          ['PATH'>>collection(variables1,variables2)],
          ['CTR'(((variables1^var+1) mod 'CYCLE_LENGTH'),variables2^var),
           variables1^var < 'CYCLE_LENGTH'                              ,
           variables2^var < 'CYCLE_LENGTH'                              ],
          ['NARC' = 'NCHANGE'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(cyclic_change_joker,
            cyclic_change_joker(2,4,
                                [[var-3],[var-0],[var-2],[var-4],[var-4],
                                 [var-4],[var-3],[var-1],[var-4]],
                                =\=)).

ctr_draw_example(cyclic_change_joker,
                 ['VARIABLES'],
                 [[[var-3],[var-0],[var-2],[var-4],[var-4],
                   [var-4],[var-3],[var-1],[var-4]]],
                 ['PATH'],
                 [2-3,7-8],
                 ['NARC'],
                 '','NARC=2',
                 [3,2.8,1.4,1]).

ctr_see_also(cyclic_change_joker,
 [link('implied by'    , cyclic_change, '',   []),
  link('common keyword', cyclic_change, '%k', ['number of changes']),
  link('common keyword', change,        '%k', ['number of changes'])]).

ctr_key_words(cyclic_change_joker,['timetabling constraint'                 ,
                                   'number of changes'                      ,
                                   'cyclic'                                 ,
                                   'joker value'                            ,
                                   'automaton'                              ,
                                   'automaton with counters'                ,
                                   'sliding cyclic(1) constraint network(2)',
                                   'acyclic'                                ,
                                   'bipartite'                              ,
                                   'no loop'                                ,
                                   'functional dependency'                  ,
		                   'pure functional dependency'             ]).

ctr_eval(cyclic_change_joker, [automaton(cyclic_change_joker_a)]).

% CTR: =
% 0: (VAR1+1)mod CYCLE_LENGTH =\= VAR2 or  VAR1 >= CYCLE_LENGTH or  VAR2 >= CYCLE_LENGTH
% 1: (VAR1+1)mod CYCLE_LENGTH  =  VAR2 and VAR1 <  CYCLE_LENGTH and VAR2 <  CYCLE_LENGTH
%
% CTR: =\=
% 0: (VAR1+1)mod CYCLE_LENGTH  =  VAR2 or  VAR1 >= CYCLE_LENGTH or  VAR2 >= CYCLE_LENGTH
% 1: (VAR1+1)mod CYCLE_LENGTH =\= VAR2 and VAR1 <  CYCLE_LENGTH and VAR2 <  CYCLE_LENGTH
%
% CTR: <
% 0: (VAR1+1)mod CYCLE_LENGTH >= VAR2 or  VAR1 >= CYCLE_LENGTH or  VAR2 >= CYCLE_LENGTH
% 1: (VAR1+1)mod CYCLE_LENGTH <  VAR2 and VAR1 <  CYCLE_LENGTH and VAR2 <  CYCLE_LENGTH
%
% CTR: >=
% 0: (VAR1+1)mod CYCLE_LENGTH <  VAR2 or  VAR1 >= CYCLE_LENGTH or  VAR2 >= CYCLE_LENGTH
% 1: (VAR1+1)mod CYCLE_LENGTH >= VAR2 and VAR1 <  CYCLE_LENGTH and VAR2 <  CYCLE_LENGTH
%
% CTR: >
% 0: (VAR1+1)mod CYCLE_LENGTH =< VAR2 or  VAR1 >= CYCLE_LENGTH or  VAR2 >= CYCLE_LENGTH
% 1: (VAR1+1)mod CYCLE_LENGTH >  VAR2 and VAR1 <  CYCLE_LENGTH and VAR2 <  CYCLE_LENGTH
%
% CTR: =<
% 0: (VAR1+1)mod CYCLE_LENGTH >  VAR2 or  VAR1 >= CYCLE_LENGTH or  VAR2 >= CYCLE_LENGTH
% 1: (VAR1+1)mod CYCLE_LENGTH =< VAR2 and VAR1 <  CYCLE_LENGTH and VAR2 <  CYCLE_LENGTH
cyclic_change_joker_a(FLAG, NCHANGE, CYCLE_LENGTH, VARIABLES, CTR) :-
    integer(CYCLE_LENGTH),
    CYCLE_LENGTH > 0,
    collection(VARIABLES, [dvar_gteq(0)]),
    length(VARIABLES, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    memberchk(CTR, [=, =\=, <, >=, >, =<]),
    cyclic_change_joker_signature(VARIABLES, SIGNATURE, CYCLE_LENGTH, CTR),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NCHANGE #<=> FLAG.

cyclic_change_joker_signature([], [], _, _).
cyclic_change_joker_signature([_], [], _, _) :- !.
cyclic_change_joker_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], CYCLE_LENGTH, =) :- !,
    (VAR1+1)mod CYCLE_LENGTH #= VAR2 #/\ VAR1 #< CYCLE_LENGTH
                                     #/\ VAR2 #< CYCLE_LENGTH #<=> S,
    cyclic_change_joker_signature([[var-VAR2]|VARs], Ss, CYCLE_LENGTH, =).
cyclic_change_joker_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], CYCLE_LENGTH, =\=) :- !,
    (VAR1+1)mod CYCLE_LENGTH #\= VAR2 #/\ VAR1 #< CYCLE_LENGTH
                                      #/\ VAR2 #< CYCLE_LENGTH #<=> S,
    cyclic_change_joker_signature([[var-VAR2]|VARs], Ss, CYCLE_LENGTH, =\=).
cyclic_change_joker_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], CYCLE_LENGTH, <) :- !,
    (VAR1+1)mod CYCLE_LENGTH #< VAR2 #/\ VAR1 #< CYCLE_LENGTH
                                     #/\ VAR2 #< CYCLE_LENGTH #<=> S,
    cyclic_change_joker_signature([[var-VAR2]|VARs], Ss, CYCLE_LENGTH, <).
cyclic_change_joker_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], CYCLE_LENGTH, >=) :- !,
    (VAR1+1)mod CYCLE_LENGTH #>= VAR2 #/\ VAR1 #< CYCLE_LENGTH
                                      #/\ VAR2 #< CYCLE_LENGTH #<=> S,
    cyclic_change_joker_signature([[var-VAR2]|VARs], Ss, CYCLE_LENGTH, >=).
cyclic_change_joker_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], CYCLE_LENGTH, >) :- !,
    (VAR1+1)mod CYCLE_LENGTH #> VAR2 #/\ VAR1 #< CYCLE_LENGTH
                                     #/\ VAR2 #< CYCLE_LENGTH #<=> S,
    cyclic_change_joker_signature([[var-VAR2]|VARs], Ss, CYCLE_LENGTH, >).
cyclic_change_joker_signature([[var-VAR1],[var-VAR2]|VARs], [S|Ss], CYCLE_LENGTH, =<) :- !,
    (VAR1+1)mod CYCLE_LENGTH #=< VAR2 #/\ VAR1 #< CYCLE_LENGTH
                                      #/\ VAR2 #< CYCLE_LENGTH #<=> S,
    cyclic_change_joker_signature([[var-VAR2]|VARs], Ss, CYCLE_LENGTH, =<).
