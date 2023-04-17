(* ::Package:: *)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                  SOLVING NON-SINGULAR MONOMIAL EQUATIONS                  |
|                                                                           |
+---------------------------------------------------------------------------+
*)
(* Solve a set of Binomial equations with possible symmetry, assuming none of
the variables are 0 *)


Options[SolveNonSingularBinomialSystem] = {
  "Symmetries" -> None,
  "InvertibleMatrices" -> {},
  "PolynomialConstraints" -> {},
  "PreEqualCheck" -> Identity,
  "UseDatabaseOfSmithDecompositions" -> False,
  "StoreDecompositions" -> False
};

SolveNonSingularBinomialSystem[ eqns_?BinomialSystemQ, vars_, param_, opts:OptionsPattern[] ] :=
  Module[{
    newInvertibleMats, newPolConstraints, newEqns, newVars, revertVars, constraints, preSolutions,
    gaugeMat, polConstraints, symmetries, invertibleMats, memoize, store, preEqCheck, symbol, eqnList, procID,
    internalParam, result, absTime
    },
    polConstraints =
      OptionValue["PolynomialConstraints"];
    symmetries =
      OptionValue["Symmetries"];
    invertibleMats =
      OptionValue["InvertibleMatrices"];
    memoize =
      OptionValue["UseDatabaseOfSmithDecompositions"];
    store =
      OptionValue["StoreDecompositions"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    
    symbol =
      Unique["x"];
    internalParam =
      Unique["z"];
    eqnList =
      DeleteCases[True] @ eqns;
    procID =
      ToString[Unique[]];
    
    printlog["SNSMS:init", {procID,eqnList,vars,param,{opts}}];
    
    { absTime, result } =
    AbsoluteTiming[
      (* Check whether trivial system *)
      If[
        Length[ DeleteCases[True] @ eqnList ] === 0,
        printlog["Gen:trivial_system", {procID}];
        Return[ { MapIndexed[ #1 -> param @@ #2 &, vars] } ]
      ];

      If[
        vars === {},
        printlog["Gen:no_vars", {procID,eqnList}];
        Message[SolveNonSingularBinomialSystem::novars, eqnList ];
        Return[{}]
      ];

      (* Check whether inconsistent system *)
      If[
        MemberQ[ False | HoldPattern[ 0 == Times[__] ] | HoldPattern[ Times[__] == 0 ]  ] @ Expand[eqnList],
        printlog["SNSMS:has_false_or_zero", {procID,Expand[eqnList]}];
        Return[{}]
      ];

      (* Check whether symmetries are multiplicative *)
      If[
        symmetries =!= None && symmetries["Transforms"][[;;,1]] =!= vars && symmetries["Transforms"] =!= {},
        Message[ SolveNonSingularBinomialSystem::wrongsymmetriesformat, symmetries["Transforms"][[;;,1]], vars ];
        Return[$Failed]
      ];

      (* Rewrite system in terms of single-indexed variables *)
      { { newEqns, newInvertibleMats, newPolConstraints }, newVars, revertVars } =
        SimplifyVariables[ { eqnList, invertibleMats, polConstraints }, vars, symbol ];

      (* Solve the logarithm of the binomial equations *)
      If[
        symmetries === None,
        gaugeMat = {},
        gaugeMat = MultiplicativeGaugeMatrix[symmetries]
      ];
      
      preSolutions =
        Thread[ newVars -> # ]& /@
        Catch[
          SolveSemiExponentiatedSystem[
            MonPolEqnsToSemiExponentiatedSystem[
              newEqns,
              Length[newVars],
              symbol,
              "NonSingular" -> True
            ],
            internalParam,
            "OrthogonalTo" -> If[ Flatten[gaugeMat] === {}, None, gaugeMat ],
            "UseDatabaseOfSmithDecompositions" -> memoize,
            "StoreDecompositions" -> store,
            "PreEqualCheck" -> preEqCheck
          ],
          "ZeroVariableInNonSingularSystem"
        ];

      (* Check for empty solution set *)
      If[
        preSolutions === {},
        printlog["SNSMS:no_solutions_log_mon", {procID}];
        Return[{}]
      ];

      constraints =
        Join[
          newPolConstraints,
          DeterminantConditions @ newInvertibleMats
        ];

      (* Check solutions against set of constraints and revert the variables for the valid solutions *)
      ReplaceAll[
        Select[ NotInvalidNonZeroSolutionQ[ constraints, preEqCheck ] ] @
        preSolutions,
        revertVars
      ]/.internalParam->param
    ];

    printlog["SNSMS:solutions", {procID,result}];
    printlog["Gen:results", {procID,result,absTime}];

    Remove[symbol,internalParam];

    result
  ];


DeterminantConditions[ mats_List ] :=
Map[
  ( Expand[Det[#]] != 0 )&,
  mats
];
