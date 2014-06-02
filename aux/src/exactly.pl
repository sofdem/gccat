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

:-dynamic exactly_a/3.

ctr_date(exactly,['20040807','20060809']).

ctr_origin(exactly, 'Derived from %c and %c.', [atleast,atmost]).

ctr_arguments(exactly,
              ['N'-int                         ,
               'VARIABLES'-collection(var-dvar),
               'VALUE'-int                     ]).

ctr_exchangeable(exactly,
                 [items('VARIABLES',all),
                  vals(['VARIABLES'^var],int(=\=('VALUE')),=\=,dontcare,dontcare)]).

ctr_synonyms(exactly,[count]).

ctr_restrictions(exactly,
                 ['N' >= 0                 ,
                  'N' =< size('VARIABLES') ,
                  required('VARIABLES',var)]).

ctr_typical(exactly,
            ['N' > 0                ,
             'N' < size('VARIABLES'),
              size('VARIABLES') > 1 ]).

ctr_pure_functional_dependency(exactly, []).
ctr_functional_dependency(exactly, 1, [2,3]).

% exactly('N1', 'VARIABLES1', 'VALUE') and
% exactly('N2', 'VARIABLES2', 'VALUE') =>
% exactly('N1'+'N2', union('VARIABLES1','VARIABLES2'), 'VALUE')
ctr_aggregate(exactly, [], [+, union, id]).

ctr_graph(exactly,
          ['VARIABLES'],
          1,
          ['SELF'>>collection(variables)],
          [variables^var = 'VALUE'],
          ['NARC' = 'N'],
          []).

ctr_example(exactly,
            exactly(2,[[var-4],[var-2],[var-4],[var-5]],4)).

ctr_draw_example(exactly,
                 ['VARIABLES'],
                 [[[var-4],[var-2],[var-4],[var-5]]],
                 ['SELF'],
                 [1-1,3-3],
                 ['NARC'],
                 '','NARC=2',
                 [2.145,2.145,1,0.6]).

ctr_see_also(exactly,
 [link(generalisation, among,   '%e replaced by %e and %e replaced by %e of %e',     [constant,variable,value,list,values]),
  link(implies,        atleast, '$=\\argument{N}$ replaced by $\\geq\\argument{N}$', []),
  link(implies,        atmost,  '$=\\argument{N}$ replaced by $\\leq\\argument{N}$', [])]).

ctr_key_words(exactly,['value constraint'                   ,
                       'counting constraint'                ,
                       'automaton'                          ,
                       'automaton with counters'            ,
                       'reverse of a constraint'            ,
 		       'glue matrix'                        ,
                       'alpha-acyclic constraint network(2)',
                       'arc-consistency'                    ,
                       'functional dependency'              ,
		       'pure functional dependency'         ]).

ctr_eval(exactly, [reformulation(exactly_r),
                   automaton(exactly_a)]).

exactly_r(N, VARIABLES, VALUE) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, NVAR),
    check_type(int(0,NVAR), N),
    integer(VALUE),
    get_attr1(VARIABLES, VARS),
    get_minimum(VARS, MINVARS),
    get_maximum(VARS, MAXVARS),
    MIN is min(MINVARS, VALUE),
    MAX is max(MAXVARS, VALUE),
    complete_card(MIN, MAX, NVAR, [VALUE], [N], VN),
    global_cardinality(VARS, VN).

% 0: VAR<>VALUE
% 1: VAR= VALUE
exactly_a(FLAG, N, VARIABLES, VALUE) :-
    collection(VARIABLES, [dvar]),
    length(VARIABLES, NVAR),
    check_type(int(0,NVAR), N),
    integer(VALUE),
    exactly_signature(VARIABLES, SIGNATURE, VALUE),
    automaton(SIGNATURE, _,
              SIGNATURE, 
              [source(s),sink(s)],
              [arc(s,0,s      ),
               arc(s,1,s,[C+1])],
              [C],[0],[COUNT]),
    COUNT #= N #<=> FLAG.

exactly_signature([], [], _).
exactly_signature([[var-VAR]|VARs], [S|Ss], VALUE) :-
    VAR #= VALUE #<=> S,
    exactly_signature(VARs, Ss, VALUE).
