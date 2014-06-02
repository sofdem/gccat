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

ctr_date(bin_packing_capa,['20091404']).

ctr_origin(bin_packing_capa, 'Derived from %c.', [bin_packing]).

ctr_arguments(bin_packing_capa,
              ['BINS'-collection(id-int, capa-int),
               'ITEMS'-collection(bin-dvar, weight-int)]).

ctr_exchangeable(bin_packing_capa,
                 [items('BINS',all),
                  items('ITEMS',all),
                  vals(['BINS'^capa],int,<,dontcare,dontcare),
                  vals(['ITEMS'^weight],int(>=(0)),>,dontcare,dontcare),
                  vals(['BINS'^id,'ITEMS'^bin],int,=\=,all,dontcare)]).

ctr_restrictions(bin_packing_capa,
                 [size('BINS')   > 0            ,
                  required('BINS',[id,capa])    ,
                  distinct('BINS',id)           ,
                  'BINS'^id      >= 1           ,
                  'BINS'^id      =< size('BINS'),
                  'BINS'^capa    >= 0           ,
                  required('ITEMS',[bin,weight]),
                  in_attr('ITEMS',bin,'BINS',id),
                  'ITEMS'^weight >= 0           ]).

ctr_typical(bin_packing_capa,
            [size('BINS')          > 1                     ,
             range('BINS'^capa)    > 1                     ,
             'BINS'^capa           > maxval('ITEMS'^weight),
             'BINS'^capa          =< sum('ITEMS'^weight)   ,
             size('ITEMS')         > 1                     ,
             range('ITEMS'^bin)    > 1                     ,
             range('ITEMS'^weight) > 1                     ,
             'ITEMS'^weight        > 0                     ]).

ctr_contractible(bin_packing_capa, [], 'ITEMS', any).

ctr_predefined(bin_packing_capa).

ctr_example(bin_packing_capa,
            bin_packing_capa([[id-1, capa-4],
                              [id-2, capa-3],
                              [id-3, capa-5],
                              [id-4, capa-3],
                              [id-5, capa-3]],
                             [[bin-3, weight-4],
                              [bin-1, weight-3],
                              [bin-3, weight-1]])).

ctr_see_also(bin_packing_capa,
 [link(specialisation, bin_packing, 'non\\nobreakdash-fixed capacity replaced by fixed overall capacity', []),
  link(generalisation, indexed_sum, 'negative contribution also allowed',                                 [])]).

ctr_key_words(bin_packing_capa,['predefined constraint'               ,
                                'resource constraint'                 ,
                                'assignment'                          ,
                                'assignment dimension'                ,
                                'assignment to the same set of values']).

ctr_application(bin_packing_capa, [2]).

ctr_eval(bin_packing_capa, [reformulation(bin_packing_capa_r)]).

bin_packing_capa_r(BINS, ITEMS) :-
    length(BINS, N),
    collection(BINS, [int(1,N),int_gteq(0)]),
    collection(ITEMS, [dvar,int_gteq(0)]),
    get_attr1(BINS, IDS),
    get_attr2(BINS, CAPAS),
    get_maximum(CAPAS, MAX),
    MAX1 is MAX+1,
	all_different(IDS),
    bin_packing1(ITEMS, 1, TASKS),
    length(ITEMS, M),
    M1 is M+1,
    bin_packing_capa1(BINS, M1, MAX, COMPLEMENTS),
    append(COMPLEMENTS, TASKS, COMPLEMENTS_TASKS),
    cumulative(COMPLEMENTS_TASKS, [limit(MAX1)]).

bin_packing_capa1([], _, _, []).
bin_packing_capa1([[_-I,_-W]|R], ID, MAX, [task(I,1,I1,H,ID)|S]) :-
    I1 is I+1,
    H is MAX-W+1,
    bin_packing_capa1(R, ID, MAX, S).
