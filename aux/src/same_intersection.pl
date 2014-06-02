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

ctr_date(same_intersection,['20040530','20060814']).

ctr_origin(same_intersection, 'Derived from %c and %c.', [same,common]).

ctr_arguments(same_intersection,
              ['VARIABLES1'-collection(var-dvar),
               'VARIABLES2'-collection(var-dvar)]).

ctr_exchangeable(same_intersection,
                 [args([['VARIABLES1','VARIABLES2']]),
                  items('VARIABLES1',all),
                  items('VARIABLES2',all),
                  vals(['VARIABLES1'^var,'VARIABLES2'^var],int,=\=,all,dontcare)]).

ctr_restrictions(same_intersection,
                 [required('VARIABLES1',var),
                  required('VARIABLES2',var)]).

ctr_typical(same_intersection,
            [size('VARIABLES1')      > 1,
             range('VARIABLES1'^var) > 1,
             size('VARIABLES2')      > 1,
             range('VARIABLES2'^var) > 1]).

ctr_graph(same_intersection,
          ['VARIABLES1','VARIABLES2'],
          2,
          ['PRODUCT'>>collection(variables1,variables2)],
          [variables1^var = variables2^var],
          [for_all('CC','NSOURCE' = 'NSINK')],
          []).

ctr_example(same_intersection,
            same_intersection([[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                              [[var-9],[var-1],[var-1],[var-1],[var-3],[var-5],[var-8]])).

ctr_draw_example(same_intersection,
                 ['VARIABLES1','VARIABLES2'],
                 [[[var-1],[var-9],[var-1],[var-5],[var-2],[var-1]],
                  [[var-9],[var-1],[var-1],[var-1],[var-3],[var-5],[var-8]]],
                 ['PRODUCT'],
                 [1-[2,3,4],
                  2-1,
                  3-[2,3,4],
                  4-6,
                  6-[2,3,4]],
                 [
                  'NCC'([[1,3,6,8,9,10],[2,7],[4,12]])],
                  '','CC#1:NSOURCE=3,NSINK=3\\nCC#2:NSOURCE=1,NSINK=1\\nCC#3:NSOURCE=1,NSINK=1',
                 [2.145,2.3,2.8,2.27]).

ctr_see_also(same_intersection,
 [link('implied by',     same,                         '',                               []),
  link('implied by',     alldifferent_on_intersection, '',                               []),
  link('common keyword', nvalue_on_intersection,       'constraint on the intersection', []),
  link('common keyword', common,                       'constraint on the intersection', [])]).

ctr_key_words(same_intersection,['constraint between two collections of variables',
                                 'constraint on the intersection'                 ]).

ctr_eval(same_intersection, [reformulation(same_intersection_r)]).

same_intersection_r(VARIABLES1, VARIABLES2) :-
	collection(VARIABLES1, [dvar]),
	collection(VARIABLES2, [dvar]),
	length(VARIABLES1, N1),
	length(VARIABLES2, N2),
    (N1 = 0 -> true ;                        
     N2 = 0 -> true ;
               get_attr1(VARIABLES1, VARS1),           
               get_attr1(VARIABLES2, VARS2),
               get_minimum(VARS1, MINVARS1),
               get_minimum(VARS2, MINVARS2),
               get_maximum(VARS1, MAXVARS1),
               get_maximum(VARS2, MAXVARS2),
               MIN is min(MINVARS1,MINVARS2),
               MAX is max(MAXVARS1,MAXVARS2),
               complete_card(MIN, MAX, N1, [], [], VN1),
               complete_card(MIN, MAX, N2, [], [], VN2),
               global_cardinality(VARS1, VN1),
               global_cardinality(VARS2, VN2),
               same_intersection1(VN1,VN2)
    ).

same_intersection1([], []).
same_intersection1([V-O1|R], [V-O2|S]) :-
    O1 #> 0 #/\ O2 #>0 #=> O2 #= O1,
    same_intersection1(R, S).
