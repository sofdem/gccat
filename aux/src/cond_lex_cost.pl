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

ctr_date(cond_lex_cost,['20060416']).

ctr_origin(cond_lex_cost, 'Inspired by \\cite{WallaceWilson06}.', []).

ctr_types(cond_lex_cost, ['TUPLE_OF_VALS'-collection(val-int)]).

ctr_arguments(cond_lex_cost,
              ['VECTOR'-collection(var-dvar)                       ,
               'PREFERENCE_TABLE'-collection(tuple-'TUPLE_OF_VALS'),
               'COST'-dvar                                         ]).

ctr_exchangeable(cond_lex_cost,
                 [items_sync('VECTOR','PREFERENCE_TABLE'^tuple,all),
                  vals(['VECTOR','PREFERENCE_TABLE'^tuple],int,=\=,all,dontcare)]).

ctr_restrictions(cond_lex_cost,
                 [size('TUPLE_OF_VALS') >= 1              ,
                  required('TUPLE_OF_VALS',val)           ,
                  required('VECTOR',var)                  ,
                  size('VECTOR') = size('TUPLE_OF_VALS')  ,
                  required('PREFERENCE_TABLE',tuple)      ,
                  same_size('PREFERENCE_TABLE',tuple)     ,
                  distinct('PREFERENCE_TABLE',[])         ,
                  in_relation('VECTOR','PREFERENCE_TABLE'),
                  'COST' >= 1                             ,
                  'COST' =< size('PREFERENCE_TABLE')      ]).

ctr_typical(cond_lex_cost,
            [size('TUPLE_OF_VALS')    > 1,
             size('VECTOR')           > 1,
             size('PREFERENCE_TABLE') > 1]).

ctr_example(cond_lex_cost,
            cond_lex_cost([[var-0],[var-1]], [[tuple-[[val-1],[val-0]]],
                                              [tuple-[[val-0],[val-1]]],
                                              [tuple-[[val-0],[val-0]]],
                                              [tuple-[[val-1],[val-1]]]], 2)).

ctr_see_also(cond_lex_cost,
 [link(specialisation,             element,            '%e of %e replaced by single %e', [tuple,variables,variable]),
  link('attached to cost variant', in_relation,        '%e parameter removed',           ['COST']),
  link('common keyword',           cond_lex_less,      '%k',                             [preferences]),
  link('common keyword',           cond_lex_lesseq,    '%k',                             [preferences]),
  link('common keyword',           cond_lex_greater,   '%k',                             [preferences]),
  link('common keyword',           cond_lex_greatereq, '%k',                             [preferences])]).

ctr_key_words(cond_lex_cost,['order constraint'                ,
                             'vector'                          ,
                             'lexicographic order'             ,
                             'Berge-acyclic constraint network',
                             'automaton'                       ,
                             'automaton without counters'      ,
                             'reified automaton constraint'    ,
                             'arc-consistency'                 ,
                             'cost filtering constraint'       ,
                             'preferences'                     ]).

ctr_persons(cond_lex_cost,['Wallace R. J.',
                           'Wilson N.'    ]).

ctr_eval(cond_lex_cost, [automata(cond_lex_cost_a)]).

cond_lex_cost_a(VECTOR, PREFERENCE_TABLE, COST) :-
    collection(VECTOR, [dvar]),
    collection(PREFERENCE_TABLE, [col([dvar])]),
    same_size(PREFERENCE_TABLE),
    check_type(dvar, COST),
    length(PREFERENCE_TABLE, LP),
    COST #>= 1,
    COST #=< LP,
    PREFERENCE_TABLE = [[_-L]|_],
    length(VECTOR, LV),
    length(L, N),
    N >= 1,
    LV = N,    
    create_collection(PREFERENCE_TABLE, vec, var, PREF),
    eval(lex_alldifferent(PREF)),
    eval(in_relation(VECTOR,PREFERENCE_TABLE)),
    cond_lex(VECTOR, PREFERENCE_TABLE, COST).
