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

% should have value-flt X-rvar Z-rvar but has to modify checker.pl
ctr_date(element_product,['20051229','20060808']).

ctr_origin(element_product, '\\cite{OttossonThorsteinsson00}', []).

ctr_arguments(element_product,
              ['Y'-dvar                     ,
               'TABLE'-collection(value-int),
               'X'-dvar                     ,
	         'Z'-dvar                     ]).

ctr_synonyms(element_product,[element]).

ctr_restrictions(element_product,
                 ['Y' >= 1               ,
                  'Y' =< size('TABLE')   ,
                  'X' >= 0               ,
                  'Z' >= 0               ,
                  required('TABLE',value),
                  'TABLE'^value >= 0     ]).

ctr_typical(element_product,
            ['X' > 0                 ,
             'Z' > 0                 ,
             size('TABLE')        > 1,
             range('TABLE'^value) > 1,
             'TABLE'^value > 0       ]).

ctr_pure_functional_dependency(element_product, []).
ctr_functional_dependency(element_product, 4, [1,2,3]).

ctr_extensible(element_product, [], 'TABLE', suffix).

ctr_derived_collections(element_product,
                        [col('ITEM'-collection(y-dvar,x-dvar,z-dvar),
                             [item(y-'Y',x-'X',z-'Z')])]).

ctr_graph(element_product,
          ['ITEM','TABLE'],
          2,
          ['PRODUCT'>>collection(item,table)],
          [item^y = table^key         ,
           item^z = item^x*table^value],
          ['NARC' = 1],
          []).

ctr_example(element_product,
            element_product(3, [[value-6],[value-9],[value-2],[value-9]], 5, 10)).

ctr_draw_example(element_product,
                 ['ITEM','TABLE'],
                 [[[y-3,x-5,z-10]],
                  [[value-6],[value-9],[value-2],[value-9]]],
                 ['PRODUCT'],
                 [1-3],
                 ['NARC'],
                 '','NARC=1',
                 [2.145,2.145,1.15,1.15]).

ctr_see_also(element_product,
 [link('common keyword', element,           '%k', ['array constraint']),
  link('common keyword', elem,              '%k', ['array constraint']),
  link('common keyword', element_greatereq, '%k', ['array constraint']),
  link('common keyword', element_lesseq,    '%k', ['array constraint'])]).

ctr_key_words(element_product,['array constraint'          ,
                               'data constraint'           ,
                               'table'                     ,
                               'functional dependency'     ,
		               'pure functional dependency',
                               'variable subscript'        ,
                               'configuration problem'     ]).

ctr_persons(element_product,['Ottosson G.'        ,
                             'Thorsteinsson E. S.']).

ctr_eval(element_product, [reformulation(element_product_r)]).

element_product_r(Y, TABLE, X, Z) :-
    check_type(dvar, Y),
    collection(TABLE, [int_gteq(0)]),
    check_type(dvar, X),
    check_type(dvar, Z),
    length(TABLE, N),
    Y #>= 1,
    Y #=< N,
    X #>= 0,
    Z #>= 0,
    get_attr1(TABLE, VALUES),
    element(Y, VALUES, VAL),
    Z #= VAL*X.
