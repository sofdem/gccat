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

ctr_date(inside_sboxes,['20070622','20090725']).

ctr_origin(inside_sboxes,'Geometry, derived from \\cite{RandellCuiCohn92}', []).

ctr_types(inside_sboxes,
          ['VARIABLES'-collection(v-dvar),
           'INTEGERS'-collection(v-int)  ,
           'POSITIVES'-collection(v-int) ]).

ctr_arguments(inside_sboxes,
              ['K'-int                                                  ,
               'DIMS'-sint                                              ,
               'OBJECTS'-collection(oid-int, sid-dvar, x-'VARIABLES')   ,
               'SBOXES'-collection(sid-int, t-'INTEGERS', l-'POSITIVES')]).

ctr_exchangeable(inside_sboxes,
                 [items('SBOXES',all),
                  items_sync('OBJECTS'^x,'SBOXES'^t,'SBOXES'^l,all)]).

ctr_synonyms(inside_sboxes,[inside]).

ctr_restrictions(inside_sboxes,
                 [size('VARIABLES') >= 1              ,
                  size('INTEGERS')  >= 1              ,
                  size('POSITIVES') >= 1              ,
                  required('VARIABLES',v)             ,
                  size('VARIABLES') = 'K'             ,
                  required('INTEGERS',v)              ,
                  size('INTEGERS')  = 'K'             ,
                  required('POSITIVES',v)             ,
                  size('POSITIVES') = 'K'             ,
                  'POSITIVES'^v     >  0              ,
                  'K'               >  0              ,
                  'DIMS'            >= 0              ,
                  'DIMS'            < 'K'             ,
                  increasing_seq('OBJECTS',[oid])     ,
                  required('OBJECTS',[oid,sid,x])     ,
                  'OBJECTS'^oid     >= 1              ,
                  'OBJECTS'^oid     =< size('OBJECTS'),
                  'OBJECTS'^sid     >= 1              ,
                  'OBJECTS'^sid     =< size('SBOXES') ,
                  size('SBOXES')    >= 1              ,
                  required('SBOXES',[sid,t,l])        ,
                  'SBOXES'^sid      >= 1              ,
                  'SBOXES'^sid      =< size('SBOXES') ,
                  do_not_overlap('SBOXES')            ]).

ctr_typical(inside_sboxes,
            [size('OBJECTS') > 1]).

ctr_contractible(inside_sboxes, [], 'OBJECTS', suffix).

ctr_example(inside_sboxes,
            inside_sboxes(2,
                          {0,1},
                          [[oid-1,sid-1,x-[[v-3],[v-3]]],
                           [oid-2,sid-2,x-[[v-2],[v-2]]],
                           [oid-3,sid-3,x-[[v-1],[v-1]]]],
                          [[sid-1,t-[[v-0],[v-0]],l-[[v-1],[v-1]]],
                           [sid-2,t-[[v-0],[v-0]],l-[[v-3],[v-3]]],
                           [sid-3,t-[[v-0],[v-0]],l-[[v-5],[v-5]]]])).

ctr_see_also(inside_sboxes,
 [link('common keyword', contains_sboxes,    '%k',    ['rcc8']),
  link('common keyword', coveredby_sboxes,   '%k',    ['rcc8']),
  link('common keyword', covers_sboxes,      '%k',    ['rcc8']),
  link('common keyword', disjoint_sboxes,    '%k',    ['rcc8']),
  link('common keyword', equal_sboxes,       '%k',    ['rcc8']),
  link('common keyword', meet_sboxes,        '%k',    ['rcc8']),
  link('common keyword', overlap_sboxes,     '%k',    ['rcc8']),
  link('common keyword', non_overlap_sboxes, '%k,%k', ['geometrical constraint', 'logic'])]).

ctr_key_words(inside_sboxes,['logic'                 ,
                             'geometrical constraint',
                             'rcc8'                  ,
                             'obscure'               ]).

ctr_persons(inside_sboxes,['Randell D. A.',
                           'Cui Z.'       ,
                           'Cohn A. G.'   ]).

ctr_application(inside_sboxes, [3]).

ctr_eval(inside_sboxes, [logic(inside_sboxes_g)]).

ctr_logic(inside_sboxes,
	  [DIMENSIONS, OIDS],
	  [(origin(O1,S1,D) ---> O1^x(D)+S1^t(D)),
	   (end(O1,S1,D) ---> O1^x(D)+S1^t(D)+S1^l(D)),
	   (inside_sboxes(Dims, O1, S1, O2, S2) --->
	    forall(D, Dims,
		   origin(O2,S2,D) #< origin(O1,S1,D) #/\
		   end(O1,S1,D) #< end(O2,S2,D))),
	   (inside_objects(Dims, O1, O2) --->
	    forall(S1, sboxes([O1^sid]),
		   exists(S2, sboxes([O2^sid]),
			  inside_sboxes(Dims, O1, S1, O2, S2)))),
	   (all_inside(Dims, OIDS) --->
	    forall(O1,objects(OIDS),
		   forall(O2,objects(OIDS),
			  (O1^oid #< O2^oid #=> inside_objects(Dims, O1, O2))))),
	   all_inside(DIMENSIONS, OIDS)]).

inside_sboxes_g(K, _, [], _) :-
    !,
    check_type(int_gteq(1), K).
inside_sboxes_g(K, _DIMS, OBJECTS, SBOXES) :-
    length(OBJECTS, O),
    length(SBOXES, S),
    O > 0,
    S > 0,
    check_type(int_gteq(1), K),
    collection(OBJECTS, [int(1,O),dvar(1,S),col(K,[dvar])]),
    collection(SBOXES, [int(1,S),col(K,[int]),col(K,[int_gteq(1)])]),
    get_attr1(OBJECTS, OIDS),
    get_attr2(OBJECTS, SIDS),
    get_col_attr3(OBJECTS, 1, XS),
    get_attr1(SBOXES, SIDES),
    get_col_attr2(SBOXES, 1, TS),
    get_col_attr3(SBOXES, 1, TL),
    collection_increasing_seq(OBJECTS,[1]),
    geost1(OIDS, SIDS, XS, Objects),
    geost2(SIDES, TS, TL, Sboxes),
    geost_dims(1, K, DIMENSIONS),
    ctr_logic(inside_sboxes, [DIMENSIONS, OIDS], Rules),
    geost(Objects, Sboxes, [overlap(true)], Rules).
