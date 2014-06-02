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

ctr_date(diffn,['20000128','20030820','20040530','20051001','20060808']).

ctr_origin(diffn, '\\cite{BeldiceanuContejean94}', []).

ctr_types(diffn,
          ['ORTHOTOPE'-collection(ori-dvar, siz-dvar, end-dvar)]).

ctr_arguments(diffn,
              ['ORTHOTOPES'-collection(orth-'ORTHOTOPE')]).

ctr_exchangeable(diffn,
                 [items('ORTHOTOPES',all),
                  items_sync('ORTHOTOPES'^orth,all),
                  vals(['ORTHOTOPES'^orth^siz],int(>=(0)),>,dontcare,dontcare),
                  translate(['ORTHOTOPES'^orth^ori,'ORTHOTOPES'^orth^end])]).

ctr_synonyms(diffn,[disjoint ,
                    disjoint1,
                    disjoint2,
                    diff2    ]).

ctr_restrictions(diffn,
                 [size('ORTHOTOPE') > 0                        ,
                  require_at_least(2,'ORTHOTOPE',[ori,siz,end]),
                  'ORTHOTOPE'^siz >= 0                         ,
                  'ORTHOTOPE'^ori =< 'ORTHOTOPE'^end           ,
                  required('ORTHOTOPES',orth)                  ,
                  same_size('ORTHOTOPES',orth)                 ]).

ctr_typical(diffn,
            [size('ORTHOTOPE')  > 1,
             'ORTHOTOPE'^siz    > 0,
             size('ORTHOTOPES') > 1]).

ctr_contractible(diffn, [], 'ORTHOTOPES', any).

ctr_graph(diffn,
          ['ORTHOTOPES'],
          1,
          ['SELF'>>collection(orthotopes)],
          [orth_link_ori_siz_end(orthotopes^orth)],
          ['NARC' = size('ORTHOTOPES')],
          []).

ctr_graph(diffn,
          ['ORTHOTOPES'],
          2,
          ['CLIQUE'(=\=)>>collection(orthotopes1,orthotopes2)],
          [two_orth_do_not_overlap(orthotopes1^orth,orthotopes2^orth)],
          ['NARC' = size('ORTHOTOPES')*size('ORTHOTOPES')-size('ORTHOTOPES')],
          []).

ctr_example(diffn,
            diffn([[orth-[[ori-2, siz-2, end-4 ], [ori-1, siz-2, end-3]] ],
                   [orth-[[ori-4, siz-4, end-8 ], [ori-2, siz-2, end-4]] ],
                   [orth-[[ori-6, siz-5, end-11], [ori-5, siz-2, end-7]] ]])).

ctr_draw_example(diffn,
                 ['ORTHOTOPES'],
                 [[[orth-[[ori-2, siz-2, end-4 ], [ori-1, siz-2, end-3]] ],
                   [orth-[[ori-4, siz-4, end-8 ], [ori-2, siz-2, end-4]] ],
                   [orth-[[ori-6, siz-5, end-11], [ori-5, siz-1, end-7]] ]]],
                 ['CLIQUE'(=\=)],
                 [1-[2,3],
                  2-[1,3],
                  3-[1,2]],
                 ['NARC'],
                 '','NARC=6',
                 []).

ctr_see_also(diffn,
 [link(specialisation,              k_alldifferent,          'when rectangles heights are all equal to %e and rectangles starts in the first dimension are all fixed', [1]),
  link(specialisation,              lex_alldifferent,        '%e replaced by %e',                              [orthotope,vector]),
  link(specialisation,              cumulatives,             '%e replaced by %e with %e %e and %e attributes', [orthotope,task,machine,assignment,origin]),
  link(specialisation,              disjunctive,             '%k replaced by %e of %e %e',                     [orthotope,task,heigth,1]),
  link(specialisation,              all_min_dist,            '%e replaced by %e, of same length',              [orthotope,'line~segment']),
  link(specialisation,              alldifferent,            '%e replaced by %e',                              [orthotope,variable]),
  link(implies,                     cumulative,              'implies one %c constraint for each dimension',   [cumulative]),
  link('implied by',                orths_are_connected,     '',                                               []),
  link('common keyword',            diffn_column,            '%k,%k',                                          ['geometrical constraint', 'orthotope']),
  link('common keyword',            diffn_include,           '%k,%k',                                          ['geometrical constraint', 'orthotope']),
  link('common keyword',            geost,                   '%k,%k',                                          ['geometrical constraint', 'non-overlapping']),
  link('common keyword',            geost_time,              '%k,%k',                                          ['geometrical constraint', 'non-overlapping']),
  link('common keyword',            non_overlap_sboxes,      '%k,%k',                                          ['geometrical constraint', 'non-overlapping']),
  link('common keyword',            calendar,                '%k,\\\\ %k',                                     ['multi-site employee scheduling with calendar constraints',
                                                                                                                'scheduling with machine choice, calendars and preemption']),
  link('common keyword',            visible,                 '%k',                                             ['geometrical constraint']),
  link('used in graph description', orth_link_ori_siz_end,   '',                                               []),
  link('used in graph description', two_orth_do_not_overlap, '',                                               []),
  link('related',                   cumulative_two_d,        '%c is a necessary condition for %c: forget one dimension when the number of dimensions is equal to %e', [cumulative_two_d,diffn,3]),
  link('related',                   lex_chain_lesseq,        'lexicographic ordering on the origins of %e, %e, $\\ldots$', [tasks, rectangles]),
  link('related',                   lex_chain_less,          'lexicographic ordering on the origins of %e, %e, $\\ldots$', [tasks, rectangles]),
  link('related',                   two_orth_column,         '',                                                           []),
  link('related',                   two_orth_include,        '',                                                           [])]).

ctr_key_words(diffn,['core'                                                       ,
                     'decomposition'                                              ,
                     'geometrical constraint'                                     ,
                     'timetabling constraint'                                     ,
                     'orthotope'                                                  ,
                     'polygon'                                                    ,
                     'non-overlapping'                                            ,
                     'disjunction'                                                ,
                     'assignment dimension'                                       ,
                     'assignment to the same set of values'                       ,
                     'assigning and scheduling tasks that run in parallel'        ,
                     'relaxation'                                                 ,
                     'relaxation dimension'                                       ,
                     'business rules'                                             ,
                     'Klee measure problem'                                       ,
                     'sweep'                                                      ,
                     'floor planning problem'                                     ,
                     'squared squares'                                            ,
                     'packing almost squares'                                     ,
                     'Partridge'                                                  ,
                     'pentomino'                                                  ,
                     'sequence dependent set-up'                                  ,
                     'Shikaku'                                                    ,
                     'smallest square for packing consecutive dominoes'           ,
                     'smallest square for packing rectangles with distinct sizes' ,
                     'smallest rectangle area'                                    ,
                     'Conway packing problem'                                     ,
                     'strip packing'                                              ,
                     'two-dimensional orthogonal packing'                         ,
                     'pallet loading'                                             ,
                     'quadtree'                                                   ,
                     'compulsory part'                                            ,
                     'constructive disjunction'                                   ,
                     'sequencing with release times and deadlines'                ,
                     'multi-site employee scheduling with calendar constraints'   ,
                     'scheduling with machine choice, calendars and preemption'   ,
                     'heuristics for two-dimensional rectangle placement problems',
                     'SAT'                                                        ]).

ctr_persons(diffn,['Beldiceanu N.'       ,
                   'Contejean E.'        ,
                   'Klee V.'             ,
                   'Nelissen J.'         ,
                   'Szymanek R.'         ,
                   'Kuchcinski K.'       ,
                   'Carlsson M.'         ,
                   'Guo Q.'              ,
                   'Thiel S.'            ,
                   'Ribeiro C.'          ,
                   'Carravilla M. A.'    ,
                   'Gambini I.'          ,
                   'Rochon du Verdier F.',
                   'Samet H.'            ,
                   'Li K.'               ,
                   'Cheng K.-H.'         ,
                   'Gehring H.'          ,
                   'Menschner K.'        ,
                   'Meyer M.'            ,
                   'Szczygiel T.'        ,
                   'Golomb S. W.'        ]).

ctr_application(diffn, [1]).

ctr_eval(diffn, [reformulation(diffn_r), density(diffn_d)]).

diffn_r([]) :-
    !.
diffn_r(ORTHOTOPES) :-
    ORTHOTOPES = [[_-ORTH1]|_],
    length(ORTH1, K),
    collection(ORTHOTOPES, [col(K,[dvar,dvar_gteq(0),dvar])]),
    get_col_attr1(ORTHOTOPES, 1, ORIS),
    get_col_attr1(ORTHOTOPES, 2, SIZS),
    get_col_attr1(ORTHOTOPES, 3, ENDS),
    (K = 2                  -> diffn0(ORIS, SIZS, ENDS, RECTS), disjoint2(RECTS) ;
     diffn_fixed_size(SIZS) -> length(Zeros, K), domain(Zeros, 0, 0), diffn5(ORIS, SIZS, ENDS, 1, Zeros, OBJS, SHAPES), geost(OBJS, SHAPES) ;
                               diffn1(ORIS, SIZS, ENDS)).

diffn_fixed_size([]).
diffn_fixed_size([L|R]) :-
    diffn_fixed_size1(L),
    diffn_fixed_size(R).

diffn_fixed_size1([]).
diffn_fixed_size1([S|R]) :-
    integer(S), S \= 0, % since geost does not accept 0 sizes
    diffn_fixed_size1(R).

diffn0([], [], [], []).
diffn0([[X,Y]|ORIS], [[L,H]|SIZS], [END|ENDS], [t(X,L,Y,H)|R]) :-
    diffn2([X,Y], [L,H], END),
    diffn0(ORIS, SIZS, ENDS, R).

diffn1([ORI1], [SIZ1], [END1]) :- !,
    diffn2(ORI1, SIZ1, END1).
diffn1([ORI1,ORI2|ORIS], [SIZ1,SIZ2|SIZS], [END1,END2|ENDS]) :-
    diffn2(ORI1, SIZ1, END1),
    diffn3([ORI2|ORIS], [END2|ENDS], ORI1, END1),
    diffn1([ORI2|ORIS], [SIZ2|SIZS],  [END2|ENDS]).

diffn2([], [], []).
diffn2([O|RO], [S|RS], [E|RE]) :-
    E #= O + S,
    diffn2(RO, RS, RE).    

diffn3([], [], _, _).
diffn3([ORI2|ORIS], [END2|ENDS], ORI1, END1) :-
    diffn4(ORI1, END1, ORI2, END2, Disjunction),
    call(Disjunction),
    diffn3(ORIS, ENDS, ORI1, END1).

diffn4([], [], [], [], 0).
diffn4([O1|R], [E1|S], [O2|T], [E2|U], E1 #=< O2 #\/ E2 #=< O1 #\/ V) :-
    diffn4(R, S, T, U, V).

diffn5([], [], [], _, _, [], []).
diffn5([ORI|ORIS], [SIZ|SIZS], [END|ENDS], I, Zeros, [object(I,I,ORI)|OBJS], [sbox(I,Zeros,SIZ)|SHAPES]) :-
    diffn2(ORI, SIZ, END),
    I1 is I+1,
    diffn5(ORIS, SIZS, ENDS, I1, Zeros, OBJS, SHAPES).

diffn_d(0, []) :- !.
diffn_d(Density, [O|R]) :-
    O = [orth-L],
    length(L, N),
    length(LMin, N),
    length(LMax, N),
    diffn_minmax([O|R], LMin, LMax, Min, Max),
    diffn_availabel(Min, Max, 1, Available),
    diffn_needed([O|R], 0, Needed),
    Density is Needed/Available.

diffn_minmax([], Min, Max, Min, Max) :- !.
diffn_minmax([[orth-O]|R], LMin, LMax, Min, Max) :-
    diffn_minmax1(O, LMin, LMax, LMin1, LMax1),
    diffn_minmax(R, LMin1, LMax1, Min, Max).

diffn_minmax1([], [], [], [], []) :- !.
diffn_minmax1([[_-O, _, _-E]|R], [MinCur|S], [MaxCur|T], [MinNew|U], [MaxNew|V]) :-
    (var(MinCur) -> MinNew = O ; MinNew is min(O,MinCur)),
    (var(MaxCur) -> MaxNew = E ; MaxNew is max(E,MaxCur)),
    diffn_minmax1(R, S, T, U, V).

diffn_availabel([], [], A, A) :- !.
diffn_availabel([Min|R], [Max|S], Cur, Res) :-
    NewCur is Cur * (Max-Min),
    diffn_availabel(R, S, NewCur, Res).

diffn_needed([], N, N) :- !.
diffn_needed([[orth-O]|R], Cur, Res) :-
    diffn_vol(O, 1, Vol),
    NewCur is Cur + Vol,
    diffn_needed(R, NewCur, Res).

diffn_vol([], V, V) :- !.
diffn_vol([[_, _-S, _]|R], Cur, Res) :-
    NewCur is Cur*S,
    diffn_vol(R, NewCur, Res).
