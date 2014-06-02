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

ctr_date(elem_from_to,['20091115']).

ctr_origin(elem_from_to, 'Derived from %c.', [elem]).

ctr_arguments(elem_from_to,
              ['ITEM'-collection(from-dvar   ,
                                 cst_from-int,
                                 to-dvar     ,
                                 cst_to-int  ,
                                 value-dvar  ),
               'TABLE'-collection(index-int,value-dvar)]).

ctr_exchangeable(elem_from_to,
                 [vals(['ITEM'^value,'TABLE'^value],int,=\=,all,dontcare)]).

ctr_synonyms(elem_from_to,[element_from_to]).

ctr_restrictions(elem_from_to,
                 [required('ITEM',[from,cst_from,to,cst_to,value]),
                  'ITEM'^from >= 1               ,
                  'ITEM'^from =< size('TABLE')   ,
                  'ITEM'^to >= 1                 ,
                  'ITEM'^to =< size('TABLE')     ,
                  'ITEM'^from =< 'ITEM'^to       ,
                  size('ITEM') = 1               ,
                  required('TABLE',[index,value]),
                  'TABLE'^index >= 1             ,
                  'TABLE'^index =< size('TABLE') ,
                  increasing_seq('TABLE',[index])]).

ctr_typical(elem_from_to,
            ['ITEM'^cst_from      >=  0,
             'ITEM'^cst_from      =<  1,
             'ITEM'^cst_to        >= -1,
             'ITEM'^cst_to        =<  1,
             size('TABLE')        >   1,
             range('TABLE'^value) >   1]).

ctr_example(elem_from_to,
            elem_from_to([[from-1, cst_from-1, to-4, cst_to-(-1), value-2]],
                         [[index-1, value-6],
                          [index-2, value-2],
                          [index-3, value-2],
                          [index-4, value-9],
                          [index-5, value-9]])).

ctr_see_also(elem_from_to,
 [link('common keyword', elem,    '%k', ['array constraint']),
  link('common keyword', element, '%k', ['array constraint'])]).

ctr_key_words(elem_from_to,['array constraint'            ,
                            'data constraint'             ,
                            'table'                       ,
                            'variable indexing'           ,
                            'variable subscript'          ,
                            'automaton'                   ,
                            'automaton without counters'  ,
                            'reified automaton constraint',
                            'arc-consistency'             ]).

ctr_eval(elem_from_to, [automaton(elem_from_to_a)]).

% let F denotes the quantity max(1,FROM+CST_FROM)
% let T denotes the quantity min(|TABLE|,TO+CST_TO)
% 0: 1 =< FROM and FROM =< TO and TO =< |TABLE| and F  > T
% 1: 1 =< FROM and FROM =< TO and TO =< |TABLE| and F =< T and F  > TABLE_INDEX
% 2: 1 =< FROM and FROM =< TO and TO =< |TABLE| and F =< T and T  < TABLE_INDEX
% 3: 1 =< FROM and FROM =< TO and TO =< |TABLE| and F =< T and F =< TABLE_INDEX and TABLE_INDEX =< T and VALUE = TABLE_VALUE
% 4: 1 =< FROM and FROM =< TO and TO =< |TABLE| and F =< T and F =< TABLE_INDEX and TABLE_INDEX =< T and VALUE=\=TABLE_VALUE
elem_from_to_a(FLAG, ITEM, TABLE) :-
    length(TABLE, N),
    collection(ITEM, [dvar(1,N),int,dvar(1,N),int,dvar]),
    collection(TABLE, [int(1,N),dvar]),
    collection_increasing_seq(TABLE,[1]),
    ITEM = [[from-FROM, cst_from-CST_FROM, to-TO, cst_to-CST_TO, value-VALUE]],
    FROM #=< TO,
    F #= max(1,FROM+CST_FROM),
    T #= min(N,TO+CST_TO),
    elem_from_to_signature(TABLE, SIGNATURE, N, FROM, TO, F, T, VALUE),
    AUTOMATON = automaton(SIGNATURE, _,
                          SIGNATURE, 
                          [source(s),sink(s)],
                          [arc(s,0,s),
                           arc(s,1,s),
                           arc(s,2,s),
                           arc(s,3,s)],
                           [],[],[]),
    automaton_bool(FLAG, [0,1,2,3,4], AUTOMATON).

elem_from_to_signature([], [], _, _, _, _, _, _).
elem_from_to_signature([[index-TABLE_INDEX,value-TABLE_VALUE]|TABLEs], [S|Ss], N, FROM, TO, F, T, VALUE) :-
    S in 0..4,
    (1 #=< FROM #/\ FROM #=< TO #/\ TO #=< N #/\ F  #> T)                                                                       #<=> S #= 0,
    (1 #=< FROM #/\ FROM #=< TO #/\ TO #=< N #/\ F #=< T #/\ F  #> TABLE_INDEX)                                                 #<=> S #= 1,
    (1 #=< FROM #/\ FROM #=< TO #/\ TO #=< N #/\ F #=< T #/\ T  #< TABLE_INDEX)                                                 #<=> S #= 2,
    (1 #=< FROM #/\ FROM #=< TO #/\ TO #=< N #/\ F #=< T #/\ F #=< TABLE_INDEX #/\ TABLE_INDEX #=< T #/\ VALUE  #= TABLE_VALUE) #<=> S #= 3,
    (1 #=< FROM #/\ FROM #=< TO #/\ TO #=< N #/\ F #=< T #/\ F #=< TABLE_INDEX #/\ TABLE_INDEX #=< T #/\ VALUE #\= TABLE_VALUE) #<=> S #= 4,
    elem_from_to_signature(TABLEs, Ss, N, FROM, TO, F, T, VALUE).
