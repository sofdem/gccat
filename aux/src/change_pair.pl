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

:-dynamic change_pair_a/4.

ctr_date(change_pair,['20030820','20040530','20060805']).

ctr_origin(change_pair, 'Derived from %c.', [change]).

ctr_arguments(change_pair,
              ['NCHANGE'-dvar                    ,
               'PAIRS'-collection(x-dvar, y-dvar),
               'CTRX'-atom                       ,
               'CTRY'-atom                       ]).

ctr_exchangeable(change_pair,
                 [translate(['PAIRS'^x]),
                  translate(['PAIRS'^y])]).

ctr_restrictions(change_pair,
                 ['NCHANGE' >= 0                   ,
                  'NCHANGE' < size('PAIRS')        ,
                  required('PAIRS',[x,y])          ,
                  in_list('CTRX',[=,=\=,<,>=,>,=<]),
                  in_list('CTRY',[=,=\=,<,>=,>,=<])]).

ctr_typical(change_pair,
            ['NCHANGE'        > 0,
             size('PAIRS')    > 1,
             range('PAIRS'^x) > 1,
             range('PAIRS'^y) > 1]).

ctr_pure_functional_dependency(change_pair, []).
ctr_functional_dependency(change_pair, 1, [2,3,4]).

ctr_graph(change_pair,
          ['PAIRS'],
          2,
          ['PATH'>>collection(pairs1,pairs2)],
          ['CTRX'(pairs1^x,pairs2^x) #\/ 'CTRY'(pairs1^y,pairs2^y)],
          ['NARC' = 'NCHANGE'],
          ['ACYCLIC', 'BIPARTITE', 'NO_LOOP']).

ctr_example(change_pair,
            change_pair(3,
                        [[x-3, y-5],
                         [x-3, y-7],
                         [x-3, y-7],
                         [x-3, y-8],
                         [x-3, y-4],
                         [x-3, y-7],
                         [x-1, y-3],
                         [x-1, y-6],
                         [x-1, y-6],
                         [x-3, y-7]],
                        =\=,
                        >)).

ctr_draw_example(change_pair,
                 ['PAIRS'],
                 [[[x-3, y-5],
                   [x-3, y-7],
                   [x-3, y-7],
                   [x-3, y-8],
                   [x-3, y-4],
                   [x-3, y-7],
                   [x-1, y-3],
                   [x-1, y-6],
                   [x-1, y-6],
                   [x-3, y-7]]],
                 ['PATH'],
                 [4-5,6-7,9-10],
                 ['NARC'],
                 '','NARC=3',
                 [2.145,5,2.145,1.2]).

ctr_see_also(change_pair,
 [link(specialisation, change,         '%e of %e replaced by %e', [pair,variables,variable]),
  link(generalisation, change_vectors, '%e of %e replaced by %e', [pair,variables,vector  ])]).

ctr_key_words(change_pair,['timetabling constraint'                 ,
                           'number of changes'                      ,
                           'pair'                                   ,
                           'automaton'                              ,
                           'automaton with counters'                ,
                           'sliding cyclic(2) constraint network(2)',
                           'acyclic'                                ,
                           'bipartite'                              ,
                           'no loop'                                ,
                           'functional dependency'                  ,
		           'pure functional dependency'             ]).

ctr_application(change_pair, [2]).

ctr_eval(change_pair, [automaton(change_pair_a)]).

change_pair_a(FLAG, NCHANGE, PAIRS, CTRX, CTRY) :-
    collection(PAIRS, [dvar,dvar]),
    length(PAIRS, N),
    N_1 is N-1,
    check_type(dvar(0,N_1), NCHANGE),
    memberchk(CTRX, [=, =\=, <, >=, >, =<]),
    memberchk(CTRY, [=, =\=, <, >=, >, =<]),
    change_pair_signature(PAIRS, SIGNATURE, CTRX, CTRY),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= NCHANGE #<=> FLAG.

change_pair_signature([], [], _, _).
change_pair_signature([_], [], _, _) :- !.

change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =, =  ) :- !,
    X1 #= X2 #\/ Y1 #= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =, =).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =, =\=) :- !,
    X1 #= X2 #\/ Y1 #\= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =, =\=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =, <  ) :- !,
    X1 #= X2 #\/ Y1 #< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =, <).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =, >= ) :- !,
    X1 #= X2 #\/ Y1 #>= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =, >=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =, >  ) :- !,
    X1 #= X2 #\/ Y1 #> Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =, >).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =, =< ) :- !,
    X1 #= X2 #\/ Y1 #=< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =, =<).

change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =\=, =  ) :- !,
    X1 #\= X2 #\/ Y1 #= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =\=, =).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =\=, =\=) :- !,
    X1 #\= X2 #\/ Y1 #\= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =\=, =\=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =\=, <  ) :- !,
    X1 #\= X2 #\/ Y1 #< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =\=, <).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =\=, >= ) :- !,
    X1 #\= X2 #\/ Y1 #>= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =\=, >=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =\=, >  ) :- !,
    X1 #\= X2 #\/ Y1 #> Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =\=, >).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =\=, =< ) :- !,
    X1 #\= X2 #\/ Y1 #=< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =\=, =<).

change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], <, =  ) :- !,
    X1 #< X2 #\/ Y1 #= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, <, =).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], <, =\=) :- !,
    X1 #< X2 #\/ Y1 #\= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, <, =\=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], <, <  ) :- !,
    X1 #< X2 #\/ Y1 #< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, <, <).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], <, >= ) :- !,
    X1 #< X2 #\/ Y1 #>= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, <, >=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], <, >  ) :- !,
    X1 #< X2 #\/ Y1 #> Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, <, >).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], <, =< ) :- !,
    X1 #< X2 #\/ Y1 #=< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, <, =<).

change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >=, =  ) :- !,
    X1 #>= X2 #\/ Y1 #= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >=, =).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >=, =\=) :- !,
    X1 #>= X2 #\/ Y1 #\= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >=, =\=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >=, <  ) :- !,
    X1 #>= X2 #\/ Y1 #< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >=, <).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >=, >= ) :- !,
    X1 #>= X2 #\/ Y1 #>= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >=, >=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >=, >  ) :- !,
    X1 #>= X2 #\/ Y1 #> Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >=, >).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >=, =< ) :- !,
    X1 #>= X2 #\/ Y1 #=< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >=, =<).

change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >, =  ) :- !,
    X1 #> X2 #\/ Y1 #= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >, =).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >, =\=) :- !,
    X1 #> X2 #\/ Y1 #\= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >, =\=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >, <  ) :- !,
    X1 #> X2 #\/ Y1 #< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >, <).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >, >= ) :- !,
    X1 #> X2 #\/ Y1 #>= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >, >=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >, >  ) :- !,
    X1 #> X2 #\/ Y1 #> Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >, >).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], >, =< ) :- !,
    X1 #> X2 #\/ Y1 #=< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, >, =<).

change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =<, =  ) :- !,
    X1 #=< X2 #\/ Y1 #= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =<, =).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =<, =\=) :- !,
    X1 #=< X2 #\/ Y1 #\= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =<, =\=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =<, <  ) :- !,
    X1 #=< X2 #\/ Y1 #< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =<, <).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =<, >= ) :- !,
    X1 #=< X2 #\/ Y1 #>= Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =<, >=).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =<, >  ) :- !,
    X1 #=< X2 #\/ Y1 #> Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =<, >).
change_pair_signature([[x-X1,y-Y1],[x-X2,y-Y2]|PAIRs], [S|Ss], =<, =< ) :- !,
    X1 #=< X2 #\/ Y1 #=< Y2 #<=> S,
    change_pair_signature([[x-X2,y-Y2]|PAIRs], Ss, =<, =<).
