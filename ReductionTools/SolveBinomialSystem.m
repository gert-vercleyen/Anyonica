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
Options[SolveBinomialSystem] =
  {
    "Symmetries" -> None,
    "NonSingular" -> False,
    "InvertibleMatrices" -> {},
    "PolynomialConstraints" -> {},
    "ZeroValues" -> None,
    "UseDatabaseOfSmithDecompositions" -> False,
    "StoreDecompositions" -> False,
    "PreEqualCheck" -> Identity
  };

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
      DeleteCases[True] @
      eqnList;
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
        solutions =
          AddOptions[opts][SolveNonSingularBinomialSystem][
            eqnList,
            vars,
            param
          ];
        Return[solutions]
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
        SolveNonSingularBinomialSystem[
          sharedBinomialSystem,
          sharedVars,
          s1,
          "Symmetries" -> sharedSymmetries,
          "InvertibleMatrices" -> invertibleMats,
          "PolynomialConstraints" -> polConstraints,
          "UseDatabaseOfSmithDecompositions" -> useDataBase,
          "StoreDecompositions" -> storeDecomps,
          "PreEqualCheck" -> preEqCheck
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


