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

ctr_date(geost,['20060919','20080609','20090116','20090725']).

ctr_origin(geost, 'Generalisation of %c.', [diffn]).

ctr_types(geost,
          ['VARIABLES'-collection(v-dvar),
           'INTEGERS'-collection(v-int)  ,
           'POSITIVES'-collection(v-int) ]).

ctr_arguments(geost,
              ['K'-int                             ,        % number of dimensions of the placement space
               'OBJECTS'-collection(oid-int        ,        % identifier of the object
                                    sid-dvar       ,        % potential shapes that can take the object
                                    x-'VARIABLES'  ),       % coordinates of the origin of the object
               'SBOXES'-collection(sid-int         ,        % identifier of the shape corresponding to the sbox
                                   t-'INTEGERS'    ,        % shift offsets from the origin of the object
                                   l-'POSITIVES'   )]).     % sizes of the sbox

ctr_exchangeable(geost,
                 [items('OBJECTS',all),
                  items('SBOXES',all),
                  items_sync('OBJECTS'^x,'SBOXES'^t,'SBOXES'^l,all),
                  vals(['SBOXES'^l^v],int(>=(1)),>,dontcare,dontcare)]).

ctr_restrictions(geost,
                 [size('VARIABLES') >= 1               ,
                  size('INTEGERS')  >= 1               ,
                  size('POSITIVES') >= 1               ,
                  required('VARIABLES',v)              ,
                  size('VARIABLES')  = 'K'             ,
                  required('INTEGERS',v)               ,
                  size('INTEGERS')   = 'K'             ,
                  required('POSITIVES',v)              ,
                  size('POSITIVES')  = 'K'             ,
                  'POSITIVES'^v      >  0              ,
                  'K'                >  0              ,
                  required('OBJECTS',[oid,sid,x])      ,
                  distinct('OBJECTS',oid)              ,
                  'OBJECTS'^oid      >= 1              ,
                  'OBJECTS'^oid      =< size('OBJECTS'),
                  'OBJECTS'^sid      >= 1              ,
                  'OBJECTS'^sid      =< size('SBOXES') ,
                  size('SBOXES')     >= 1              ,
                  required('SBOXES',[sid,t,l])         ,
                  'SBOXES'^sid       >= 1              ,
                  'SBOXES'^sid       =< size('SBOXES') ,
                  do_not_overlap('SBOXES')             ]).

ctr_typical(geost,
            [size('OBJECTS') > 1]).

ctr_predefined(geost).

ctr_example(geost,
            geost(2,
                  [[oid-1,sid-1,x-[[v-1],[v-2]]],
                   [oid-2,sid-5,x-[[v-2],[v-1]]],
                   [oid-3,sid-8,x-[[v-4],[v-1]]]],
                  [[sid-1,t-[[v-0   ],[v-0]],l-[[v-2],[v-1]]],    % the 3 sboxes of shape s1
                   [sid-1,t-[[v-0   ],[v-1]],l-[[v-1],[v-2]]],
                   [sid-1,t-[[v-1   ],[v-2]],l-[[v-3],[v-1]]],                    
                   [sid-2,t-[[v-0   ],[v-0]],l-[[v-3],[v-1]]],    % the 3 sboxes of shape s2
                   [sid-2,t-[[v-0   ],[v-1]],l-[[v-1],[v-3]]],
                   [sid-2,t-[[v-2   ],[v-1]],l-[[v-1],[v-1]]],
                   [sid-3,t-[[v-0   ],[v-0]],l-[[v-2],[v-1]]],    % the 3 sboxes of shape s3
                   [sid-3,t-[[v-1   ],[v-1]],l-[[v-1],[v-2]]],
                   [sid-3,t-[[v-(-2)],[v-2]],l-[[v-3],[v-1]]],
                   [sid-4,t-[[v-0   ],[v-0]],l-[[v-3],[v-1]]],    % the 3 sboxes of shape s4
                   [sid-4,t-[[v-0   ],[v-1]],l-[[v-1],[v-1]]],
                   [sid-4,t-[[v-2   ],[v-1]],l-[[v-1],[v-3]]],
                   [sid-5,t-[[v-0   ],[v-0]],l-[[v-2],[v-1]]],    % the 3 sboxes of shape s5
                   [sid-5,t-[[v-1   ],[v-1]],l-[[v-1],[v-1]]],
                   [sid-5,t-[[v-0   ],[v-2]],l-[[v-2],[v-1]]],
                   [sid-6,t-[[v-0   ],[v-0]],l-[[v-3],[v-1]]],    % the 3 sboxes of shape s6
                   [sid-6,t-[[v-0   ],[v-1]],l-[[v-1],[v-1]]],
                   [sid-6,t-[[v-2   ],[v-1]],l-[[v-1],[v-1]]],
                   [sid-7,t-[[v-0   ],[v-0]],l-[[v-3],[v-2]]],    % the sbox of shape s7
                   [sid-8,t-[[v-0   ],[v-0]],l-[[v-2],[v-3]]]])). % the sbox of shape s8

ctr_see_also(geost,
 [link('generalisation', geost_time,         '%e %e added to %e %e', [temporal,dimension,geometrical,dimensions]),
  link('specialisation', k_alldifferent,     'when rectangles heights are all equal to %e and rectangles starts in the first dimension are all fixed', [1]),
  link('specialisation', lex_alldifferent,   '%e replaced by %e',    [object, vector]),
  link('common keyword', calendar,           '%k,\\\\ %k',           ['multi-site employee scheduling with calendar constraints',
                                                                      'scheduling with machine choice, calendars and preemption']),
  link('common keyword', diffn,              '%k,%k',                ['geometrical constraint', 'non-overlapping'],'\\\\ '),
  link('common keyword', non_overlap_sboxes, '%k,%k',                ['geometrical constraint', 'non-overlapping']),
  link('common keyword', visible,            '%k,%k',                ['geometrical constraint', 'sweep']),
  link('common keyword', lex_chain_less,     '%k',                   ['symmetry']),
  link('common keyword', lex_chain_lesseq,   '%k',                   ['symmetry'],'\\\\ ')]).

ctr_key_words(geost,['logic'                                                      ,
		     'decomposition'                                              ,
                     'geometrical constraint'                                     ,
                     'timetabling constraint'                                     ,
                     'multi-site employee scheduling with calendar constraints'   ,
                     'scheduling with machine choice, calendars and preemption'   ,
                     'predefined constraint'                                      ,
                     'non-overlapping'                                            ,
                     'disjunction'                                                ,
                     'assignment dimension'                                       ,
                     'assigning and scheduling tasks that run in parallel'        ,
                     'assignment to the same set of values'                       ,
                     'relaxation'                                                 ,
                     'relaxation dimension'                                       ,
                     'business rules'                                             ,
                     'sweep'                                                      ,
                     'symmetry'                                                   ,
                     'squared squares'                                            ,
                     'packing almost squares'                                     ,
                     'Partridge'                                                  ,
                     'pentomino'                                                  ,
                     'Shikaku'                                                    ,
                     'smallest square for packing consecutive dominoes'           ,                     
                     'smallest square for packing rectangles with distinct sizes' ,
                     'smallest rectangle area'                                    ,
                     'Conway packing problem'                                     ,
                     'floor planning problem'                                     ,
                     'strip packing'                                              ,
                     'two-dimensional orthogonal packing'                         ,
                     'pallet loading'                                             ,
                     'heuristics for two-dimensional rectangle placement problems']).

ctr_persons(geost,['Beldiceanu N.',
                   'Carlsson M.'  ,
                   'Poder E.'     ,
                   'Sadek R.'     ,
                   'Truchet C.'   ,
                   '\\r{A}gren M.',
                   'Sbihi M.'     ,
                   'Zampelli S.'  ]).

ctr_application(geost, [2]).

ctr_eval(geost, [builtin(geost_b)]).

geost_b(K, [], _) :-
    !,
    check_type(int_gteq(1), K).
geost_b(K, OBJECTS, SBOXES) :-
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
    geost1(OIDS, SIDS, XS, Objects),
    geost2(SIDES, TS, TL, Sboxes),
    catch(geost(Objects, Sboxes),
          _Flag,
          fail).

