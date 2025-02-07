
Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                      BUILDING TOWERS OF EXPRESSIONS                       |
|                                                                           |
+---------------------------------------------------------------------------+
*)
PackageExport["TowerOfExpressions"]

TowerOfExpressions::usage =
  "TowerOfExpressions[exprList,s] returns a tower of couples containing variables" <>
  " and expressions in those variables.";

TowerOfExpressions::wrongformat =
  "Argument `1` must be a list of expressions.";

TowerOfExpressions::notasymbol =
  "Argument `1` is not a symbol";
(*"such that the first couple contains polynomials with the least number of unknowns, \*)
(*the second couple contains polynomials with the least number of \*)
(*unknowns under the assumption that the unknowns of the first couple \*)
(*are known etc. Options include \"Symbol\" -> x to assume the \*)
(*variables are of the form x[__], and \"LevelSpec\" -> n to take only \*)
(*for variables up to level n into account.";*)

Options[TowerOfExpressions] :=
  Options[ VarExprPairs ];

TowerOfExpressions[ {}, s_, opts:OptionsPattern[] ] :=
  { {}, {} };

TowerOfExpressions[ exprList_, s_, opts:OptionsPattern[] ] :=
  Which[
    Head[exprList] =!= List
    ,
    Message[TowerOfExpressions::wrongformat,exprList];
    Abort[]
    ,
    Head[s] =!= Symbol
    ,
    Message[ TowerOfExpressions::notasymbol, s ];
    Abort[]
    ,
    True
    ,
    VarExprPairs[ exprList, s, opts ]//
    GroupExpressions
  ];

Options[VarExprPairs] :=
  Options[LeastVarsExpr];

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
                Sow[ AddOptions[opts][GetVariables][ a[ minKey[[1]] ], s, excludedVars ], "vars" ]
              ]
            ]
          ],
          (* ELSE *)
          Sow[ {a[[1]]}, "expr" ]; Sow[ AddOptions[opts][GetVariables][ a[[1]], s, excludedVars ], "vars" ]
        ];
      Reverse /@ Transpose[ Reap[ SowExprVarPairs[ asso, {} ] ][[2]]/.(a_->b_) :> b ]
    ]
  ];

PackageScope["LeastVarsExpr"]

LeastVarsExpr::usage =
  "LeastVarsExpr[exprList,s] returns the expression containing the least number of different s variables."


LeastVarsExpr::novarsinexpressions =
  "Warning: `1` contains no variables in the symbol `2`";

(*LeastVarsExpr[exprList,s,excludedVars] returns the expression from \*)
(*exprList with the least amount of s variables, not counting the \*)
(*variables in excludedVars. Options include \"LevelSpec\" -> n to take \*)
(*only variables up to level n into account";*)

Options[ LeastVarsExpr ] :=
  Join[
    Options[ GetVariables ],
    { "WeighedBy" -> Function[ { x , y }, Length[x] ] }
  ];

LeastVarsExpr[ expressions_, s_, opts:OptionsPattern[] ] :=
  With[{
    l =
      OptionValue["WeighedBy"][ AddOptions[opts][GetVariables][ #, s ], # ]& /@
      expressions
    },
    First @
    FirstPosition[ l, Min[l] ]
  ];

LeastVarsExpr[ expressions_, s_, excludedVars_List, opts:OptionsPattern[] ] :=
  With[{
    l =
      OptionValue["WeighedBy"][ AddOptions[opts][GetVariables][ #, s, excludedVars ], # ]& /@
      expressions
    },
    First @
    FirstPosition[ l, Min[l] ]
  ];

GroupExpressions[ varExprLists_List ] :=
  With[{
    nonEmptyVarPositions =
      Append[ Length[varExprLists] + 1 ] @
      Flatten@
      Position[ varExprLists, { {__}, _ } ]
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
