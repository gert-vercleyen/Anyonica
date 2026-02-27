(* ::Package:: *)


Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                        SOLVING BINOMIAL EQUATIONS                         |
|                                                                           |
+---------------------------------------------------------------------------+
*)

PackageExport["SolveBinomialSystem"]

SolveBinomialSystem::usage =
  "SolveBinomialSystem[binEqns,vars,s] solves the system of binomial " <>
  "equations binEqns in variables vars and returns a solution parametrized by s";

SolveBinomialSystem::nonbineqns =
  "`1` is not a system of binomial polynomial equations.";

SolveBinomialSystem::notlistofvars =
  "`1` is not a list of variables.";

(* Solve a set of Binomial equations with possible symmetry *)
Options[SolveBinomialSystem] :=
  Join[
    Options[SolveNonSingularBinomialSystem],
    {
      "NonSingular" -> False,
      "ZeroValues" -> None
    }
  ];

CheckArgs[ eqnList_, vars_ ][ code_ ] :=
  Which[
    !BinomialSystemQ[eqnList]
    ,
    Message[ SolveBinomialSystem::nonbineqns, eqnList ];
    Abort[]
    ,
    !ListQ[vars]
    ,
    Message[ SolveBinomialSystem::notlistofvars, vars ];
    Abort[]
    ,
    True
    ,
    code
  ];

SolveBinomialSystem[ eqnList_, vars_, param_, opts:OptionsPattern[] ] :=
  CheckArgs[ eqnList, vars ] @
  Module[{
    invertibleMats, symmetries, polConstraints, useDataBase, storeDecomps, filteredEqns, preEqCheck, s1, gs,
    procID, zeros, unionZeros, sharedBinomialSystem, sharedVars, sharedSolutions, remainingEquations,
    InsertZeros, UpdateMultiplicativeSymmetries, updateNonSharedVars, solutions, sharedSymmetries,
    nonSingularQ, zeroValues, newVars, result, absTime
    },
    symmetries =
      OptionValue["Symmetries"];
    nonSingularQ =
      OptionValue["NonSingular"];
    invertibleMats =
      OptionValue["InvertibleMatrices"];
    polConstraints =
      OptionValue["PolynomialConstraints"];
    zeroValues =
      OptionValue["ZeroValues"];
    useDataBase =
      OptionValue["UseDatabaseOfSmithDecompositions"];
    storeDecomps =
      OptionValue["StoreDecompositions"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    
    filteredEqns =
      TEL @ eqnList;
    s1 =
      Unique["x"];
    gs =
      Unique["g"];
    procID =
      ToString[Unique[]];
    
    InsertZeros[zeros_][solution_] :=
      SortBy[
        Join[
          solution,
          Normal[zeros]
        ],
        First
      ];

    printlog["SMS:init", {procID,eqnList,vars,param,{opts}}];
    
    { absTime, result } =
    AbsoluteTiming[
      (* Check for trivial system *)
      If[
        filteredEqns === {},
        printlog["Gen:trivial_system", {procID}];
        Return[ { MapIndexed[ #1 -> param @@ #2 &, vars] } ]
      ];

      (* Check for inconsistent system *)
      If[
        MemberQ[False] @ filteredEqns,
        printlog["Gen:has_False"];
        Return[ {} ]
      ];

      (* Find out which variables could be 0 *)

      If[ (* No vars are 0 by assumption *)
        nonSingularQ,
        Return @
        AddOptions[opts][SolveNonSingularBinomialSystem][
          eqnList,
          vars,
          param
        ]
      ];

      If[ (* Zeros are given as data *)
        zeroValues =!= None,
        (* THEN *)
        zeros =
          Dispatch /@ zeroValues,
        (* ELSE *)
        zeros =
          ( Dispatch @* DeleteCases[ _ -> x_ /; x =!= 0 ] ) /@
          FindZeroValues[ filteredEqns, vars,  "InvertibleMatrices" -> invertibleMats ]
      ];

      (* If there are no solutions to the Diophantine Equations then the system is inconsistent*)
      If[
        zeros === {},
        printlog["SMS:no_zero_vals", {procID}];
        Return[{}]
      ];

      (* Solve binomial equations containing none of the vars that are 0 in any of the configurations *)
      unionZeros =
        Union @@ Normal[zeros][[;;,;;,1]];

      sharedVars =
        Complement[ vars, unionZeros ];

      { sharedBinomialSystem, remainingEquations } =
        BinSplit[
          filteredEqns,
          FreeQ[ Alternatives @@ unionZeros ] @* preEqCheck
        ];

      SetAttributes[ s1, NHoldAll ];

      sharedSymmetries =
        If[
          symmetries === None,
          None,
          MapAt[
            Cases[ HoldPattern[ f_ -> _] /; MemberQ[ sharedVars, f ] ],
            symmetries,
            {1}
          ]
        ];

      sharedSolutions =
        AddOptions[opts][SolveNonSingularBinomialSystem][
          sharedBinomialSystem,
          sharedVars,
          s1,
          "Symmetries" -> sharedSymmetries
        ];

      If[
        sharedSolutions === {},
        Return[{}]
      ];

      (* We need to fix the gauges for the shared variables, substitute the unshared variables with new parameters and
         add trivial gauge constraints for all new parameters that are not associated to unshared variables *)
      UpdateMultiplicativeSymmetries[ None, __ ] :=
        None;
      UpdateMultiplicativeSymmetries[ symmetries_, zeros_, newVars_, updateNonSharedVars_ ] :=
        Module[{ restrictedGauges },
          restrictedGauges =
            RestrictMultiplicativeSymmetries[ symmetries, sharedVars, gs ]/.zeros;
          <|
            "Transforms" ->
              SortBy[
                Join[
                  DeleteCases[ HoldPattern[ 0 -> _ ] ] @
                  restrictedGauges["Transforms"]/.updateNonSharedVars,
                  Thread[ newVars -> newVars ]
                ],
                First
              ],
            "Symbols" -> {gs}
          |>
        ];

      (* Refine shared solutions by using remaining equations with 0-values inserted *)
      Reap[
        With[{
          ums = UpdateMultiplicativeSymmetries,
          uim = UpdateInvertibleMatrices,
          uc  = UpdateConstraints
          },
          Do[
            newVars =
              GetVariables[ ss, s1 ];
            
            updateNonSharedVars =
              With[{
                nonSharedVars = Complement[ vars, sharedVars, Normal[z][[;;,1]] ], (* Variables neither shared, nor 0*)
                i0 = Length @ newVars (* Amount of new variables in solution shared system*)
                },
                Thread[ nonSharedVars -> Table[ s1[i], { i, i0 + 1, i0 + Length[nonSharedVars] } ] ]
              ];
            
            With[{
              specificSoln =
                SolveNonSingularBinomialSystem[
                  DeleteCases[True] @ ( remainingEquations/.z/.ss/.updateNonSharedVars ),
                  Join[ newVars, updateNonSharedVars[[;;,2]] ],
                  param,
                  "Symmetries" -> ums[ symmetries, z, newVars, updateNonSharedVars ] ,
                  "InvertibleMatrices" ->  uim[ uim[ invertibleMats, z ], ss ],
                  "PolynomialConstraints" -> uc[ uc[ invertibleMats, z ], ss ]
                ]
              },
              If[ (* There are solutions *)
                specificSoln =!= {},
                (* THEN: add zeros, sort solution and throw on stack *)
                Sow /@
                ReplaceAll[
                  InsertZeros[z][ Join[ ss, updateNonSharedVars] ],
                  specificSoln
                ]
              ]
            ],
            { z, zeros },
            { ss, sharedSolutions }
          ]
        ]
      ][[2]] // If[ # != {}, #[[1]], {} ]&
    ];

    Remove[s1,gs];
    
    printlog["SMS:solutions", { procID, result } ];
    printlog["Gen:results", { procID, result, absTime } ];
    
    result
  ];

PackageExport["SBS"]

SBS::usage =
  "Shorthand for SolveBinomialSystem.";

SBS =
  SolveBinomialSystem;



(*
+---------------------------------------------------------------------------+
|                                                                           |
|                  SOLVING NON-SINGULAR BINOMIAL EQUATIONS                  |
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

      (* Check whether system is consistent *)
      If[
        TrueQ[ MemberQ[False] @ reducedSystem ],
        printlog["SNSBS:has_false_or_zero", {procID, Expand[reducedSystem]}];
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

      preSolutions = EchoLabel["PreSolutions"] @
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
      invalidPos = EchoLabel["InvalidPos"] @ (* TODO: check whether parallelization is feasible and useful *)
        Position[
          preSolutions,
          sol_/; Not[ NotInvalidNonZeroSolutionQ[ constraints, preEqCheck ] @ sol ],
          {1},
          Heads -> False
        ];
      
      If[ 
        Length @ Flatten @ invalidPos =!= 0, 
        printlog[ "SNSBS:constraints_not_satisfied", { procID, preSolutions, constraints, invalidPos } ] 
      ];
      
      Delete[ preSolutions, invalidPos ] /. revertVars /. internalParam -> param
     
    ];

    printlog["SNSBS:solutions", {procID, result} ];
    printlog["Gen:results", {procID, result, absTime} ];

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