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

ctr_date(open_minimum,['20090506']).

ctr_origin(open_minimum, 'Derived from %c', [minimum]).

ctr_arguments(open_minimum,
              ['MIN'-dvar                                 ,
               'VARIABLES'-collection(var-dvar, bool-dvar)]).

ctr_exchangeable(open_minimum,
                 [items('VARIABLES',all),
                  translate(['MIN','VARIABLES'^var])]).

ctr_restrictions(open_minimum,
                 [size('VARIABLES') > 0           ,
                  required('VARIABLES',[var,bool]),
                  'VARIABLES'^bool >= 0           ,
                  'VARIABLES'^bool =< 1           ]).

ctr_typical(open_minimum,
            [size('VARIABLES')      > 1,
             range('VARIABLES'^var) > 1]).

ctr_example(open_minimum,
            open_minimum(3,[[var-3,bool-1],[var-1,bool-0],[var-7,bool-0],[var-5,bool-1],[var-5,bool-1]])).

ctr_see_also(open_minimum,
 [link('hard version',              minimum,      '', []),
  link('comparison swapped',        open_maximum, '', []),
  link('uses in its reformulation', tree_range,   '', []),
  link('used in graph description', in_set,       '', [])]).

ctr_key_words(open_minimum,['order constraint'                        ,
                            'open constraint'                         ,
                            'open automaton constraint'               ,
                            'minimum'                                 ,
                            'automaton'                               ,
                            'automaton without counters'              ,
                            'reified automaton constraint'            ,
                            'centered cyclic(1) constraint network(1)']).

ctr_eval(open_minimum, [automaton(open_minimum_a)]).

% 0: B=1 and MIN<VAR
% 1: B=1 and MIN=VAR
% 2: B=1 and MIN>VAR
% 3: B=0 and MIN<VAR
% 4: B=0 and MIN=VAR
% 5: B=0 and MIN>VAR
open_minimum_a(FLAG, MIN, VARIABLES) :-
    check_type(dvar, MIN),
    collection(VARIABLES, [dvar, dvar(0,1)]),
    length(VARIABLES, N),
    N > 0,
    open_minimum_signature(VARIABLES, SIGNATURE, MIN),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE,
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(s,3,s),
                           arc(s,4,s),
                           arc(s,5,s),
                           arc(t,1,t),
                           arc(t,0,t),
                           arc(t,3,t),
                           arc(t,4,t),
                           arc(t,5,t)],
                           [],[],[]),
    automaton_bool(FLAG, [0,1,2,3,4,5], AUTOMATON).

open_minimum_signature([], [], _).
open_minimum_signature([[var-VAR,bool-B]|VARs], [S|Ss], MIN) :-
    S in 0..5,
    B #= 1 #/\ MIN #< VAR #<=> S #= 0,
    B #= 1 #/\ MIN #= VAR #<=> S #= 1,
    B #= 1 #/\ MIN #> VAR #<=> S #= 2,
    B #= 0 #/\ MIN #< VAR #<=> S #= 3,
    B #= 0 #/\ MIN #= VAR #<=> S #= 4,
    B #= 0 #/\ MIN #> VAR #<=> S #= 5,
    open_minimum_signature(VARs, Ss, MIN).
