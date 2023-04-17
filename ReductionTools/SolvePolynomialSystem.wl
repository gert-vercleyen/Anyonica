(* ::Package:: *)

Options[ SolvePolynomialSystem ] = {
  "Symmetries" -> None,
  "NonSingular" -> False,
  "PolynomialConstraints" -> {},
  "InvertibleMatrices" -> {},
  "ZeroValues" -> None,
  "FindZerosUsingSums" -> False,
  "SimplifyIntermediateResultsBy" -> Identity,
  "PreEqualCheck" -> Identity,
  "UseDatabaseOfSmithDecompositions" -> False,
  "StoreDecompositions" -> False
};

SolvePolynomialSystem[ eqns_List, vars_List, symbol_, opts:OptionsPattern[] ] :=
  Module[{
    newEquations, newSymmetries, newPolConstraints, newInvertibleMats, newVars, newZeroVals, revertVars,
    binomialEquations, sumEquations, sumEqnSolnPairs, sumEqns, monSol, sumSolution, lastVars, solutions,
    constraints, sym, nonSingularQ, polConstraints, invertibleMats, zeroVals, useSumsQ, simplify,
    preEqCheck, useDataBase, storeDecomps, procID, s, absTime, result
    },
    sym =
      OptionValue["Symmetries"];
    nonSingularQ =
      OptionValue["NonSingular"];
    polConstraints =
      OptionValue["PolynomialConstraints"];
    invertibleMats =
      OptionValue["InvertibleMatrices"];
    zeroVals =
      OptionValue["ZeroValues"];
    useSumsQ =
      OptionValue["FindZerosUsingSums"];
    simplify =
      OptionValue["SimplifyIntermediateResultsBy"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    useDataBase =
      OptionValue["UseDatabaseOfSmithDecompositions"];
    storeDecomps =
      OptionValue["StoreDecompositions"];
    procID =
      ToString[ Unique[] ];
    s =
      Unique["x"];

    printlog[ "SPS:init", {procID,eqns,vars,symbol,{opts}}];
    
    { absTime, result } =
    AbsoluteTiming[
      (* Optionally find zero values using all equations *)
      If[
        !nonSingularQ && zeroVals === None && useSumsQ,
        zeroVals =
          ( DeleteCases[ _ -> x_ /; x =!= 0 ] ) /@
          FindZeroValues[ eqns, vars, "InvertibleMatrices" -> invertibleMats ]
      ];

      { { newEquations, newSymmetries, newPolConstraints, newInvertibleMats, newZeroVals }, newVars, revertVars } =
        SimplifyVariables[ { eqns, sym, polConstraints, invertibleMats, Normal @ zeroVals }, vars, s ];

      { binomialEquations, sumEquations } =
          BinomialSplit[
            DeleteCases[True] @
            newEquations ];

      sumEqnSolnPairs =
        ReduceByBinomials[
          sumEquations,
          binomialEquations,
          newVars,
          symbol,
          "Symmetries" -> newSymmetries,
          "PolynomialConstraints" -> newPolConstraints,
          "InvertibleMatrices" -> newInvertibleMats,
          "NonSingular" -> nonSingularQ,
          "ZeroValues" -> newZeroVals,
          "SimplifyIntermediateResultsBy" -> simplify,
          "PreEqualCheck" -> preEqCheck,
          "UseDatabaseOfSmithDecompositions" -> useDataBase,
          "StoreDecompositions" -> storeDecomps
        ];

      constraints =
        Join[ polConstraints, DeterminantConditions @ invertibleMats ];

      (* TODO: would be interesting to check how long this step takes *)
      Select[
        Flatten[ #, 2 ]& @
        Reap[
          Do[
            { sumEqns, monSol } =
              eqnSolPair;

            lastVars =
              GetVars[ monSol, symbol ];

            sumSolution =
              SolveUsingReduce[ Join[ sumEqns, Thread[ lastVars != 0 ] ], lastVars ];

            If[
              sumSolution =!= {},
              Sow[ monSol/.sumSolution ]
            ],
            { eqnSolPair, sumEqnSolnPairs }
          ]
        ][[2]]/.revertVars,
        ValidSolutionQ[ constraints, preEqCheck ]
      ]
    ];
    Remove[s];

    printlog["SPS:solutions", {procID,result}];
    printlog["Gen:results", {procID,result,absTime}];

    result
  ];
