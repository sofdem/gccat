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

ctr_date(diffn_column,['20030820']).

ctr_origin(diffn_column, '\\index{CHIP|indexuse}CHIP: option guillotine cut (column) of %c.', [diffn]).

ctr_types(diffn_column,
          ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_arguments(diffn_column,
              ['ORTHOTOPES'-collection(orth-'ORTHOTOPE'),
               'DIM'-int                                ]).

ctr_exchangeable(diffn_column,
                 [items('ORTHOTOPES',all),
                  translate(['ORTHOTOPES'^orth^ori,'ORTHOTOPES'^orth^end])]).

ctr_restrictions(diffn_column,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >= 0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  required('ORTHOTOPES',orth)                  ,
                  same_size('ORTHOTOPES',orth)                 ,
                  'DIM' > 0                                    ,
                  'DIM' =< size('ORTHOTOPE')                   ,
                  diffn('ORTHOTOPES')                          ]).

ctr_typical(diffn_column,
            [size('ORTHOTOPE')  > 1,
             'ORTHOTOPE'^siz    > 0,
             size('ORTHOTOPES') > 1]).

ctr_contractible(diffn_column, [], 'ORTHOTOPES', any).

ctr_graph(diffn_column,
          ['ORTHOTOPES'],
          2,
          ['CLIQUE'(<)>>collection(orthotopes1,orthotopes2)],
          [two_orth_column(orthotopes1^orth,orthotopes2^orth,'DIM')],
          ['NARC' = (size('ORTHOTOPES') * (size('ORTHOTOPES')-1)) / 2],
          []).

ctr_example(diffn_column,
            diffn_column([[orth-[[ori-1,  siz-3, end-4 ], [ori-3, siz-2, end-5]] ],
                          [orth-[[ori-9,  siz-1, end-10], [ori-4, siz-3, end-7]] ],
                          [orth-[[ori-4,  siz-2, end-6 ], [ori-3, siz-4, end-7]] ],
                          [orth-[[ori-1,  siz-3, end-4 ], [ori-6, siz-1, end-7]] ],
                          [orth-[[ori-6,  siz-2, end-8 ], [ori-1, siz-4, end-5]] ],
                          [orth-[[ori-10, siz-1, end-11], [ori-1, siz-1, end-2]] ],
                          [orth-[[ori-9,  siz-1, end-10], [ori-1, siz-1, end-2]] ],
                          [orth-[[ori-6,  siz-2, end-8 ], [ori-6, siz-1, end-7]] ]], 1)).

ctr_draw_example(diffn_column,
                 ['ORTHOTOPES'],
                 [[[orth-[[ori-1,  siz-3, end-4 ], [ori-3, siz-2, end-5]]],
                   [orth-[[ori-9,  siz-1, end-10], [ori-4, siz-3, end-7]]],
                   [orth-[[ori-4,  siz-2, end-6 ], [ori-3, siz-4, end-7]]]]],
                 ['CLIQUE'(<)],
                 [1-[2,3],
                  2-[3]],
                 ['NARC'],
                 '','NARC=3',
                 [1.5,1.6,1.6,1.6]).

ctr_see_also(diffn_column,
 [link('common keyword',            diffn,           '%k,%k',    ['geometrical constraint', 'orthotope']),
  link('common keyword',            diffn_include,   '%k,%k,%k', ['geometrical constraint', 'orthotope', 'positioning constraint']),
  link('implies',                   diffn_include,   '',         []),
  link('used in graph description', two_orth_column, '',         [])]).

ctr_key_words(diffn_column,['decomposition'         ,
                            'geometrical constraint',
                            'positioning constraint',
                            'orthotope'             ,
                            'guillotine cut'        ]).

ctr_application(diffn_column, [1]).

ctr_eval(diffn_column, [reformulation(diffn_column_r)]).

diffn_column_r([], DIM) :-
    integer(DIM),
    DIM > 0.
diffn_column_r(ORTHOTOPES, DIM) :-
    ORTHOTOPES = [[_-ORTH1]|_],
    length(ORTH1, K),
    collection(ORTHOTOPES, [col(K,[dvar,dvar_gteq(0),dvar])]),
    check_type(int(1,K), DIM),
    eval(diffn(ORTHOTOPES)),
    get_attr1(ORTHOTOPES, ORTHOTOPES1),
    diffn_column1(ORTHOTOPES1, DIM).

diffn_column1([], _).
diffn_column1([_], _) :- !.
diffn_column1([O1,O2|R], DIM) :-
    eval(two_orth_column(O1, O2, DIM)),
    diffn_column1([O2|R], DIM).
