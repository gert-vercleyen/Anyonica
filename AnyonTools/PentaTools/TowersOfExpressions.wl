(*
+---------------------------------------------------------------------------+
|                                                                           |
|                      BUILDING TOWERS OF EXPRESSIONS                       |
|                                                                           |
+---------------------------------------------------------------------------+
*)
Options[TowerOfExpressions] =
  Options[ VarExprPairs ];

TowerOfExpressions[ {}, s_, opts:OptionsPattern[] ] :=
  { {}, {} };

TowerOfExpressions[ exprList_, s_, opts:OptionsPattern[] ] :=
  Which[
    Head[exprList] =!= List,
      Message[TowerOfExpressions::wrongformat,exprList],
    Head[s] =!= Symbol,
      Message[TowerOfExpressions::notasymbol,s],
    True,
      VarExprPairs[ exprList, s, opts ] //
      GroupExpressions
  ];

Options[VarExprPairs] = Options[LeastVarsExpr];
VarExprPairs[ expressions_List, s_, opts:OptionsPattern[] ] :=
  Block[{
    $IterationLimit = Infinity,
    $RecursionLimit = Infinity},
    Module[{ SowExprVarPairs, asso },
      asso =
        Association @@
        Table[ i -> expressions[[i]], { i, Length[expressions] } ];

      SowExprVarPairs[ a_Association, excludedVars_List ] :=
        If[ Length[ a ] > 1,
          (* THEN *)
          With[{ minKey = LeastVarsExpr[ a, s, excludedVars, opts ] },
            Sow[ { a[ minKey[[1]] ] }, "expr" ];
            SowExprVarPairs[
              KeyDrop[ a, minKey ],
              Join[
                excludedVars,
                Sow[ AddOptions[opts][GetVars][ a[ minKey[[1]] ], s, excludedVars ], "vars" ]
              ]
            ]
          ],
          (* ELSE *)
          Sow[ {a[[1]]}, "expr" ]; Sow[ AddOptions[opts][GetVars][ a[[1]], s, excludedVars ], "vars" ]
        ];
      Reverse /@ Transpose[ Reap[ SowExprVarPairs[ asso, {} ] ][[2]]/.(a_->b_) :> b ]
    ]
  ];

Options[ LeastVarsExpr ] =
  Join[
    Options[ GetVars ],
    { "WeighedBy" -> Function[ { x , y }, Length[x] ] }
  ];

LeastVarsExpr[ expressions_, s_, opts:OptionsPattern[] ] :=
  With[{
    l =
      OptionValue["WeighedBy"][ AddOptions[opts][GetVars][ #, s ], # ]& /@
      expressions
    },
    First @
    FirstPosition[ l, Min[l] ]
  ];

LeastVarsExpr[ expressions_, s_, excludedVars_List, opts:OptionsPattern[] ] :=
  With[{
    l =
      OptionValue["WeighedBy"][ AddOptions[opts][GetVars][ #, s, excludedVars ], # ]& /@
      expressions
    },
    First @
    FirstPosition[ l, Min[l] ]
  ];

GroupExpressions[ varExprLists_List ] :=
  With[{
    nonEmptyVarPositions =
      Append[
        Flatten@
        Position[ varExprLists, { {__}, _ } ],
        Length @ varExprLists
      ]
    },
    Table[
      {
        (* Vars *)
        varExprLists[[ nonEmptyVarPositions[[i]], 1 ]],
        (* All expressions with those vars *)
        Join @@
        varExprLists[[
          nonEmptyVarPositions[[i]] ;; nonEmptyVarPositions[[i + 1]] - 1,
          2
        ]]
      },
      { i, Length[nonEmptyVarPositions] - 1 }
    ]
  ];
