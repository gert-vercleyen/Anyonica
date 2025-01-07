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

SolveNonSingularBinomialSystem::nonbineqns =
  "`1` is not a system of binomial polynomial equations.";

SolveNonSingularBinomialSystem::notimplementedyet =
  "Only symmetries that multiply variables by numbers are implemented at the moment.";

SolveNonSingularBinomialSystem::wrongsymmetriesformat =
  "The list of variables appearing in the symmetries should be the same list as the list of given variables, " <>
  "i.e. `1` should be `2`";

SolveNonSingularBinomialSystem::novars =
  "Set of equations, `1`, with empty set of variables. Aborting calculations."<>
  " If equalities are unresolved but True, then adding the option " <>
  "\"SimplifyIntermediateResultsBy\"-> f (where f is a function that simplifies expressions) can get rid of unresolved equalities.";

Options[SolveNonSingularBinomialSystem] :=
  Join[
    {
      "Symmetries" -> None,
      "InvertibleMatrices" -> {},
      "PolynomialConstraints" -> {},
      "PreEqualCheck" -> Identity,
      "UseDatabaseOfSmithDecompositions" -> False,
      "StoreDecompositions" -> False,
      "SimplifyIntermediateResultsBy" -> Identity,
      "Parallel" -> False,
      "DivideAndConquer" -> True
    },
    Options[ReduceBinomialSystem]
  ];

CheckArgs[ eqns_, vars_ ] :=
  Which[
    !BinomialSystemQ[ eqns ]
    ,
    Message[ SolveNonSingularBinomialSystem::nonbineqns, eqns ];
    Abort[]
    ,
    Length[vars] === 0
    ,
    Message[ SolveNonSingularBinomialSystem::novars, eqns ];
    Abort[]
  ];

SolveNonSingularBinomialSystem[ eqns_?BinomialSystemQ, vars_, param_, opts:OptionsPattern[] ] :=
(
  CheckArgs[ eqns, vars ];
  Module[
    {
      newInvertibleMats, newPolConstraints, newEqns, newVars, revertVars, constraints, preSolutions,
      gaugeMat, polConstraints, symmetries, invertibleMats, preEqCheck, symbol, eqnList, procID,
      internalParam, result, absTime, invalidPos
    },
    polConstraints =
      OptionValue["PolynomialConstraints"];
    symmetries =
      OptionValue["Symmetries"];
    invertibleMats =
      OptionValue["InvertibleMatrices"];
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

    printlog["SNSBS:init", {procID, eqnList, vars, param, {opts}}];

    { absTime, result } =
    AbsoluteTiming[
      (* Check whether trivial system *)
      If[
        Length[ DeleteCases[True] @ eqnList ] === 0,
        printlog["Gen:trivial_system", {procID}];
        Return[ { MapIndexed[ #1 -> param @@ #2 &, vars ] } ] (* TODO: use thread *)
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
      
      (* Check whether inconsistent system *)
      If[
        !AddOptions[ConsistentQ][opts][ newEqns, ValidEqnQ[symbol] ]
        ,
        printlog["SNSBS:has_false_or_zero", {procID, Expand[eqnList]}];
        Return @ {}
      ];

      (* Simplify the system using a Hermite decomposition *)
      reducedSystem = 
        ( # == 0 )& /@ (* Convert polynomials to equations *)
        (AddOptions[opts][ReduceBinomialSystem][ newEqns, newVars ])["Polynomials"];
      
      (* Check whether inconsistent system *)
      If[
        !AddOptions[ConsistentQ][opts][ newEqns, ValidEqnQ[symbol] ]
        ,
        printlog["SNSBS:has_false_or_zero", {procID, Expand[eqnList]}];
        Return @ {}
      ];

      (* Solve the logarithm of the binomial equations *)
      If[
        symmetries === None,
        gaugeMat = {},
        gaugeMat = MultiplicativeGaugeMatrix[symmetries]
      ];

      semiLinearSystem = 
        AddOptions[opts][BinToSemiLin][ reducedSystem, newVars, symbol ];

      If[ (* system contains a product equal to 0 *)
        MemberQ[0] @ preEqCheck @ semiLinearSystem[[2]], 
        printlog["SNSBS:has_false_or_zero", {procID, Expand[eqnList]}];
        Return @ {}
      ];

      preSolutions =
        Catch[
          Thread[ newVars -> # ]& /@
          AddOptions[opts][SolveSemiLinModZ][
            semiLinearSystem,
            internalParam,
            "OrthogonalTo" -> If[ Flatten[gaugeMat] === {}, None, gaugeMat ]
          ],
          "Inconsistent"
        ];
      
      (* Check for empty solution set *)
      If[
        preSolutions === {} || preSolutions === { {}, {} },
        printlog["SNSBS:no_solutions_log_mon", {procID}];
        Return @ {}
      ];
      
      (* Check for inconsistency solution set *)
      If[
        StringQ[ First @ preSolutions ],
        printLog["SNSBS:has_false_or_zero_2", { procID, newEqns, preSolutions } ];
        printlog["SNSBS:no_solutions_log_mon", { procID } ];
        Return @ {}
      ];

      constraints =
        Join[
          newPolConstraints,
          DeterminantConditions @ newInvertibleMats
        ];

      (* Check solutions against set of constraints and revert the variables for the valid solutions *)
      invalidPos = (* TODO: check whether parallelization is feasible and useful *)
        Position[
          preSolutions,
          sol_/; Not[ NotInvalidNonZeroSolutionQ[ constraints, preEqCheck ] @ sol ],
          {1},
          Heads -> False
        ];
      
      If[ invalidPos != 0, printlog[ "SNSBS:constraints_not_satisfied", { procID, preSolutions, constraints, invalidPos } ] ];
      
      Delete[ preSolutions, invalidPos ] /. revertVars /. internalParam -> param
     
    ];

    printlog["SNSBS:solutions", {procID, result} ];
    printlog["Gen:results", {procID, result, absTime} ];

    Remove[symbol, internalParam];

    result
  ]
);


ValidEqnQ[symbol_][ eqn_ ] :=
  Not @ 
  MatchQ[ Expand @ eqn,
    False | HoldPattern[ 0 == expr_ ] | HoldPattern[ expr_ == 0 ] /;
    MatchQ[ expr, HoldPattern[Times[__]] | HoldPattern[Power[symbol[_],_]] | symbol[_] ]
  ];

PackageScope["SNSBS"]

SNSBS =
  SolveNonSingularBinomialSystem;



PackageScope["DeterminantConditions"]

DeterminantConditions[ mats_List ] :=
  Map[
    ( Expand[Det[#]] != 0 )&,
    mats
  ];
