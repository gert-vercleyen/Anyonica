(* ::Package:: *)

Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                  SOLVING NON-SINGULAR MONOMIAL EQUATIONS                  |
|                                                                           |
+---------------------------------------------------------------------------+
*)
(* Solve a set of Binomial equations with possible symmetry, assuming none of
the variables are 0 *)

PackageScope["SolveNonSingularBinomialSystem"]

SolveNonSingularBinomialsystem::nonbineqns =
  "`1` is not a system of binomial polynomial equations.";

SolveNonSingularBinomialSystem::notimplementedyet =
  "Only symmetries that multiply variables by numbers are implemented at the moment.";

SolveNonSingularBinomialSystem::wrongsymmetriesformat =
  "The list of variables appearing in the symmetries should be the same list as the list of given variables, " <>
  "i.e. `1` should be `2`";

SolveNonSingularBinomialSystem::novars =
  "Nonempty set of equations, `1`, with empty set of variables. Aborting calculations."<>
  " If equalities are unresolved but True, then adding the option " <>
  "\"PreEqualCheck\"-> f (where f is a function that simplifies expressions) can get rid of unresolved equalities.";

Options[SolveNonSingularBinomialSystem] :=
  {
    "Symmetries" -> None,
    "InvertibleMatrices" -> {},
    "PolynomialConstraints" -> {},
    "PreEqualCheck" -> Identity,
    "UseDatabaseOfSmithDecompositions" -> False,
    "StoreDecompositions" -> False,
    "SimplifyIntermediateResultsBy" -> Identity
  };

CheckArgs[ eqns_, vars_ ] :=
  Which[
    !BinomialSystemQ[ eqns ]
    ,
    Message[ SolveNonSingularBinomialsystem::nonbineqns, eqns ];
    Abort[]
    ,
    Length[vars] === 0
    ,
    Message[ SolveNonSingularBinomialsystem::novars, vars ];
    Abort[]
  ];

SolveNonSingularBinomialSystem[ eqns_?BinomialSystemQ, vars_, param_, opts:OptionsPattern[] ] :=
(
  CheckArgs[ eqns, vars ];
  Module[
    {
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

    printlog["SNSMS:init", {procID, eqnList, vars, param, {opts}}];

    { absTime, result } =
    AbsoluteTiming[
      (* Check whether trivial system *)
      If[
        Length[ DeleteCases[True] @ eqnList ] === 0,
        printlog["Gen:trivial_system", {procID}];
        Return[ { MapIndexed[ #1 -> param @@ #2 &, vars ] } ] (* TODO: use thread *)
      ];

      If[
        vars === {},
        printlog["Gen:no_vars", {procID, eqnList}];
        Message[SolveNonSingularBinomialSystem::novars, eqnList ];
        Abort[]
      ];

      (* Check whether inconsistent system *)
      If[
        MemberQ[ False | HoldPattern[ 0 == Times[__] ] | HoldPattern[ Times[__] == 0 ]  ] @ Expand[eqnList],
        printlog["SNSMS:has_false_or_zero", {procID, Expand[eqnList]}];
        Return @ {}
      ];

      (* Check whether symmetries are multiplicative *)
      If[
        symmetries =!= None && symmetries["Transforms"][[;;, 1]] =!= vars && symmetries["Transforms"] =!= {},
        Message[ SolveNonSingularBinomialSystem::wrongsymmetriesformat, symmetries["Transforms"][[;;, 1]], vars ];
        Abort[]
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
        Catch[
          Thread[ newVars -> # ]& /@
          SolveSemiLinModZ[
            BinToSemiLin[
              newEqns,
              Length[newVars],
              symbol,
              "SimplifyBy" -> OptionValue["SimplifyIntermediateResultsBy"]
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
        preSolutions === {} || preSolutions === { {}, {} },
        printlog["SNSMS:no_solutions_log_mon", {procID}];
        Return @ {}
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
      ] /. internalParam -> param
    ];

    printlog["SNSMS:solutions", {procID, result}];
    printlog["Gen:results", {procID, result, absTime}];

    Remove[symbol, internalParam];

    result
  ]
);


PackageScope["SNSBS"]

SNSBS =
  SolveNonSingularBinomialSystem;



PackageScope["DeterminantConditions"]

DeterminantConditions[ mats_List ] :=
  Map[
    ( Expand[Det[#]] != 0 )&,
    mats
  ];
