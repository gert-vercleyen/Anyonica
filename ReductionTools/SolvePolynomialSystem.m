(* ::Package:: *)

Package["Anyonica`"]

PackageExport["SolvePolynomialSystem"]

SolvePolynomialSystem::usage =
  "SolvePolynomialSystem[ eqns, vars, s ] returns the solutions, parametrized by s, "<>
  "to the system of polynomial equations eqns in the variables vars.\n" <>
  "SolvePolynomialSystem[ polynomials, vars, s ] returns the solutions, parametrized by s, "<>
  "to the system of polynomials in the variables vars.\n"<>
  "SolvePolynomialSystem[ sys, s ] returns the solutions, parametrized by s, "<>
  "to the Standard Polynomial System sys.";

Options[ SolvePolynomialSystem ] :=
  Options[SolveBinomialSystem];

(*SolvePolynomialSystem[ eqns_List, vars_List, symbol_, opts:OptionsPattern[] ] :=
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
      False;
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
              GetVariables[ monSol, symbol ];

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
*)

SolvePolynomialSystem[ sys_SPS, s_, opts:OptionsPattern[] ] :=
  Module[{ OV, pols, assum, rules, vars, zeroConfigs, bins, nonbins, procID, validPos, systems, symmetries },
    OV = OptionValue;
    
    procID = ToString @ Unique[];
    
    printlog["SPS:init",{ procID, sys, s, {opts} }];
    
    { pols, assum, rules, vars } = GetData[ sys ];
    
    { time, results } =
    AbsoluteTiming[
      
      { bins, nonbins } =
        BinomialSplit @ pols;
      
      printlog[ "SPS:zero_FS", { procID } ];
      
      (* ================================================= *)
      (* Find which configurations of variables can be 0 = *)
      (* ================================================= *)
      
      zeroConfigs =
        Dispatch /@
        Which[
          OV["NonSingular"],       {{}},
          OV["ZeroValues"] =!= {}, OV["ZeroValues"],
          True,                    AddOptions[opts][FindZeroValues][ If[ OV["FindZerosUsingSums"], pols, bins ] ]
        ];
      
      (* Check the configurations with the rest of the system *)
      validPos =
        Position[ zeroConfigs, z_ /; ValidZerosQ[pols] @ z, {1} ] // Flatten;
      
      printlog[" SPS:zero_values_results", { procID, zeroConfigs, Complement[ Range @ Length @ zeroConfigs, validPos ] } ];
      
      (* Remove invalid configurations *)
      zeroConfigs = zeroConfigs[[ validPos ]];
      
      If[ (* No valid configurations *)
        Length[zeroConfigs] == 0,
        printlog["SPS:no_valid_zeros"]; Return @  { }
      ];
      
      (* Create systems for each configuration of zero values *)
      systems = UpdateRules[ sys, # ]& /@ zeroConfigs;
      
      (* ================================================= *)
      (*               Break gauge symmetry                *)
      (* ================================================= *)
      
      symmetries = OV["Symmetries"];
      
      (* Check whether symmetries are multiplicative *)
      If[
        symmetries =!= None && symmetries["Transforms"][[;;, 1]] =!= vars && symmetries["Transforms"] =!= {},
        Message[ SolvePolynomialSystem::wrongsymmetriesformat, symmetries["Transforms"][[;;, 1]], vars ];
        Abort[]
      ];
     
      If[
        symmetries =!= None,
        systems = AddOptions[opts][BreakAllMultiplicativeSymmetry][ symmetries, vars, systems ]
      ];
      
      (* ================================================= *)
      (*             Reduce via trivialities               *)
      (* ================================================= *)
      
      (* ================================================= *)
      (*               Reduce via Binomials                *)
      (* ================================================= *)
      
      (* ================================================= *)
      (*              Reduce via Linear Terms              *)
      (* ================================================= *)
      
      (* ================================================= *)
      (*          Solve equations with 1 variable          *)
      (* ================================================= *)
      
      (* ================================================= *)
      (*                Find Groebner Bases                *)
      (* ================================================= *)
      
      (* ================================================= *)
      (*             Solve Systems of Equations            *)
      (* ================================================= *)
      
    ];
    
    results
  ];
    

(* BreakAllMultiplicativeSymmetry updates systems by fixing the gauge symmetry for all configurations of 0 values
   by first breaking symmetry for all symbols that do not involve 0 values and then
   breaking further symmetry, case by case.
 *)

Options[BreakAllMultiplicativeSymmetry] :=
  Options[BreakMultiplicativeSymmetry];

BreakAllMultiplicativeSymmetry[ symmetries_, variables_, systems_, opts:OptionsPattern[] ] :=
  Module[ { zeroConfigs, unionZeros, sharedVars, firstFixedVars, remainingSym, fixedVars },
    zeroConfigs =
      Cases[ HoldPattern[ _ -> 0 ] ] @* GetRules /@ systems;
    
    If[
      zeroConfigs == { { } }
      ,
      fixedVars = Last @ AddOptions[opts][BreakMultiplicativeSymmetry][symmetries];
      Return @ { UpdateValues[ First @ systems, firstFixedVars ] }
    ] ;
    
    (* Break Gauge Symmetry: first for all variables that are never 0, i.e.
       that do not appear in any of the "zeros" from previous step *)
    
    (* Get all vars that could be 0 for some configuration in zeros *)
    unionZeros = Union @@ Normal[zeroConfigs][[;;,;;,1]];
    
    (* Get all vars that can never be 0 *)
    sharedVars = Complement[ variables, unionZeros ];
    
    (* Fix the gauge for all vars that can never be 0 *)
    { remainingSym, firstFixedVars } =
      AddOptions[opts][BreakMultiplicativeSymmetry][ symmetries, "ExcludedVariables" -> unionZeros ];
    
    
    (* Try to fix extra gauges, if possible, for each of the 0-configurations. *)
    
    If[ (* All gauges are fixed *)
      Equivalent @@@ remainingSym["Transforms"]
      ,
      (* THEN: no gauge freedom left. Return updated systems *)
      Table[
        UpdateValues[ sys, firstFixedVars ],
        { sys, systems }
      ]
      ,
      (* ELSE: still gauge freedom left. Breaking further symmetry per set of zero values *)
      Table[
        fixedVars =
          Join[
            firstFixedVars,
            Last @ BreakMultiplicativeSymmetry[ AddZerosToSymmetries[ remainingSym, zeroConfigs[[i]] ] ]
          ];
        
        UpdateValues[ systems[[i]], fixedVars ]
        ,
        { i, Length @ systems }
      ]
    ]
  ];


PackageScope["ValidZerosQ"]

ValidZerosQ[ pols_ ][ zeros_ ] :=
  FreeQ[ pols/.Dispatch[zeros], x_?MonomialQ /; x =!= 0 ];
