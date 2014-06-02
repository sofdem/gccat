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

ctr_date(twin,['20111129']).

ctr_origin(twin, 'Pairs of variables related by hiden %c constraints sharing the same table.', [element]).

ctr_arguments(twin,
              ['PAIRS'-collection(x-dvar,y-dvar)]).

ctr_restrictions(twin,
                 [required('PAIRS',x),
                  required('PAIRS',y),
		  size('PAIRS') > 0]).

ctr_typical(twin,
            [size('PAIRS') > 1                ,
	     size('PAIRS') > nval('PAIRS'^x)  ,
	     size('PAIRS') > nval('PAIRS'^y)  ,
	     nval('PAIRS'^x) > 1              ,
	     nval('PAIRS'^y) > 1              ,
	     nval('PAIRS'^x) = nval('PAIRS'^y),
	     nval('PAIRS'^x) < size('PAIRS')  ,
             nval('PAIRS'^y) < size('PAIRS')  ]).

ctr_contractible(twin, [], 'PAIRS', any).

ctr_predefined(twin).

ctr_example(twin,
            twin([[x-1,y-8],[x-9,y-6],[x-1,y-8],[x-5,y-0],[x-6,y-7],[x-9,y-6]])).

ctr_see_also(twin,
             [link('implied by', circuit,                     '',                                               []),
	      link('implied by', derangement,                 '',                                               []),
	      link('implied by', proper_circuit,              '',                                               []),
	      link('implied by', symmetric_alldifferent_loop, '',                                               []),
	      link(related,      element,                     'pairs linked by an element with the same table', [])]).

ctr_key_words(twin,['predefined constraint',
		    'pair'                 ]).

ctr_eval(twin, [checker(twin_c),
		reformulation(twin_r)]).

twin_c(PAIRS) :-
    collection(PAIRS, [int,int]),
    length(PAIRS, N),
    N > 0,
    get_attr12(PAIRS, P12),
    sort(P12, S12),
    twin1(S12),
    get_attr21(PAIRS, P21),
    sort(P21, S21),
    twin1(S21).

twin1([]) :- !.
twin1([_]) :- !.
twin1([X1-_,X2-Y|R]) :-
    X1 \== X2,
    twin1([X2-Y|R]).

twin_r(PAIRS) :-
    collection(PAIRS, [dvar,dvar]),
    length(PAIRS, N),
    N > 0,
    get_attr1(PAIRS, XS),
    get_attr2(PAIRS, YS),
    get_min_list_dvar(XS, _, MinX),
    get_min_list_dvar(YS, _, MinY),
    get_max_list_dvar(YS, _, MaxY),
    RangeY is MaxY-MinY+1,
    twin1(XS, YS, MinX, MinY, RangeY, XYS),
    NPAIRS in 1..N,
    nvalue(NPAIRS, XS),
    nvalue(NPAIRS, YS),
    nvalue(NPAIRS, XYS).

twin1([], [], _, _, _, []) :- !.
twin1([X|RX], [Y|RY], MinX, MinY, RangeY, [XY|RXY]) :-
    XY #= RangeY*(X-MinX)+(Y-MinY),
    twin1(RX, RY, MinX, MinY, RangeY, RXY).
