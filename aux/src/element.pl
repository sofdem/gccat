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

ctr_date(element,['20000128','20030820','20040530','20060808','20090923']).

ctr_origin(element, '\\cite{VanHentenryckCarillon88}', []).

ctr_arguments(element,
              ['INDEX'-dvar,
               'TABLE'-collection(value-dvar),
               'VALUE'-dvar]).

ctr_exchangeable(element,
                 [vals(['TABLE'^value,'VALUE'],int,=\=,all,dontcare)]).

ctr_synonyms(element,[nth, element_var, array]).

ctr_restrictions(element,
                 ['INDEX' >= 1            ,
                  'INDEX' =< size('TABLE'),
                  size('TABLE') > 0       ,
                  required('TABLE',value) ]).

ctr_typical(element,
            [size('TABLE')        > 1,
             range('TABLE'^value) > 1]).

ctr_pure_functional_dependency(element, []).
ctr_functional_dependency(element, 3, [1,2]).

ctr_extensible(element, [], 'TABLE', suffix).

ctr_derived_collections(element,
                        [col('ITEM'-collection(index-dvar,value-dvar),
                             [item(index-'INDEX',value-'VALUE')])]).

ctr_graph(element,
          ['ITEM','TABLE'],
          2,
          ['PRODUCT'>>collection(item,table)],
          [item^index = table^key,
           item^value = table^value],
          ['NARC' = 1],
          []).

ctr_example(element,
            element(3, [[value-6],[value-9],[value-2],[value-9]], 2)).

ctr_draw_example(element,
                 ['ITEM','TABLE'],
                 [[[index-3,value-2]],
                  [[value-6],[value-9],[value-2],[value-9]]],
                 ['PRODUCT'],
                 [1-3],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,2.145,1.15,1.15]).

ctr_see_also(element,
 [link('implies',                   elem,                  '',                                                 []),
  link('implied by',                elem,                  '',                                                 []),
  link('common keyword',            element_greatereq,     '%k',                                               ['array constraint']),
  link('common keyword',            element_lesseq,        '%k',                                               ['array constraint']),
  link('common keyword',            element_sparse,        '%k',                                               ['array constraint']),
  link('common keyword',            element_matrix,        '%k',                                               ['array constraint']),
  link('common keyword',            element_product,       '%k',                                               ['array constraint']),
  link('common keyword',            elem_from_to,          '%k',                                               ['array constraint']),
  link('common keyword',            elements_sparse,       '%k',                                               ['data constraint']),
  link('common keyword',            elementn,              '%k',                                               ['data constraint']),
  link('common keyword',            in_relation,           '%k',                                               ['data constraint']),
  link('common keyword',            stage_element,         '%k',                                               ['data constraint']),
  link('common keyword',            sum,                   '%k',                                               ['data constraint']),
  link('system of constraints',     elements,              '',                                                 []),
  link('generalisation',            cond_lex_cost,         '%e replaced by %e of %e',                          [variable,tuple,variables]),
  link('uses in its reformulation', cycle,                 '',                                                 []),
  link('uses in its reformulation', elements_alldifferent, '',                                                 []),
  link('uses in its reformulation', tree_range,            '',                                                 []),
  link('uses in its reformulation', tree_resource,         '',                                                 []),
  link('uses in its reformulation', sort_permutation,      '',                                                 []),
  link('related',                   twin,                  '(pairs linked by an element with the same table)', [])]).

ctr_key_words(element,['core'                                    ,
                       'array constraint'                        ,
                       'data constraint'                         ,
                       'table'                                   ,
                       'functional dependency'                   ,
		       'pure functional dependency'              ,
                       'variable indexing'                       ,
                       'variable subscript'                      ,
                       'disjunction'                             ,
                       'assignment to the same set of values'    ,
                       'sequence dependent set-up'               ,
                       'automaton'                               ,
                       'automaton without counters'              ,
                       'reified automaton constraint'            ,
                       'centered cyclic(2) constraint network(1)',
                       'derived collection'                      ,
                       'arc-consistency'                         ,
                       'zebra puzzle'                            ,
                       'labelling by increasing cost'            ,
                       'regret based heuristics'                 ]).

ctr_persons(element,['Van Hentenryck P.',
                     'Carillon J.-P.'   ]).

ctr_eval(element, [builtin(element_b), automaton(element_a)]).

element_b(INDEX, TABLE, VALUE) :-
    check_type(dvar, INDEX),
    collection(TABLE, [dvar]),
    check_type(dvar, VALUE),
    length(TABLE, N),
    N > 0,
    INDEX #>= 1,
    INDEX #=< N,
    get_attr1(TABLE, VALUES),
    element(INDEX, VALUES, VALUE).

% 0: INDEX=\=TABLE_KEY or  VALUE=\=TABLE_VALUE
% 1: INDEX = TABLE_KEY and VALUE = TABLE_VALUE
element_a(FLAG, INDEX, TABLE, VALUE) :-
    check_type(dvar, INDEX),
    collection(TABLE, [dvar]),
    check_type(dvar, VALUE),
    length(TABLE, N),
    N > 0,
    INDEX #>= 1,
    INDEX #=< N,
    element_signature(TABLE, INDEX, VALUE, 1, SIGNATURE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE,
                          [source(s),sink(t)],
                          [arc(s,0,s),
                           arc(s,1,t),
                           arc(t,0,t),
                           arc(t,1,t)],
                          [],[],[]),
    automaton_bool(FLAG, [0,1], AUTOMATON).

element_signature([], _, _, _, []).
element_signature([[value-TABLE_VALUE]|Ts], INDEX, VALUE, TABLE_KEY, [B|Bs]) :-
        INDEX#=TABLE_KEY #/\ VALUE#=TABLE_VALUE #<=> B,
        TABLE_KEY1 is TABLE_KEY+1,
        element_signature(Ts, INDEX, VALUE, TABLE_KEY1, Bs).
