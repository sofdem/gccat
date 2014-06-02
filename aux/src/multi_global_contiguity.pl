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

ctr_date(multi_global_contiguity,['20120212']).

ctr_origin(multi_global_contiguity, 'Derived from %c.', [global_contiguity]).

ctr_arguments(multi_global_contiguity,
              ['VARIABLES'-collection(var-dvar)]).

ctr_exchangeable(multi_global_contiguity,
                 [items('VARIABLES',reverse)]).

ctr_synonyms(multi_global_contiguity,[multi_contiguity]).

ctr_restrictions(multi_global_contiguity,
                 [required('VARIABLES',var),
                  'VARIABLES'^var >= 0     ]).

ctr_typical(multi_global_contiguity,
            [size('VARIABLES') > 3]).

ctr_predefined(multi_global_contiguity).

ctr_contractible(multi_global_contiguity, [], 'VARIABLES', any).

ctr_example(multi_global_contiguity,
            multi_global_contiguity([[var-0],[var-2],[var-2],[var-1],[var-1],[var-0],[var-0],[var-5]])).

ctr_see_also(multi_global_contiguity,
 [link('implied by',     alldifferent,          '',   []),
  link('implied by',     alldifferent_except_0, '',   []),
  link('implied by',     all_equal,             '',   []),
  link('implied by',     decreasing,            '',   []),
  link('implied by',     increasing,            '',   []),
  link('implied by',     global_contiguity,     '',   []),
  link('common keyword', group,                 '%k', ['sequence'])]).

ctr_key_words(multi_global_contiguity,['predefined constraint',
				       'sequence'             ]).

ctr_eval(multi_global_contiguity, [checker(multi_global_contiguity_c)]).

ctr_sol(multi_global_contiguity,2,0,2,9,-).
ctr_sol(multi_global_contiguity,3,0,3,55,-).
ctr_sol(multi_global_contiguity,4,0,4,413,-).
ctr_sol(multi_global_contiguity,5,0,5,3656,-).
ctr_sol(multi_global_contiguity,6,0,6,37147,-).
ctr_sol(multi_global_contiguity,7,0,7,425069,-).
ctr_sol(multi_global_contiguity,8,0,8,5400481,-).

multi_global_contiguity_c([]) :- !.
multi_global_contiguity_c(VARIABLES) :-
	collection(VARIABLES, [int_gteq(0)]),
	get_kattr1(VARIABLES, 1, VARKEYS),
	sort(VARKEYS, SVARKEYS),
	multi_global_contiguity_c1(SVARKEYS).

multi_global_contiguity_c1([]) :- !.
multi_global_contiguity_c1([_]) :- !.
multi_global_contiguity_c1([0-_|R]) :-
	!,
	multi_global_contiguity_c1(R).
multi_global_contiguity_c1([I-P,I-Q|R]) :-
	!,
	Q is P+1,
	multi_global_contiguity_c1([I-Q|R]).
multi_global_contiguity_c1([_,J-Q|R]) :-
	multi_global_contiguity_c1([J-Q|R]).
