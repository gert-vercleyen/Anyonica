(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-03-17 *)

Package["Anyonica`"]

PackageExport["PentagonGroebnerSystems"]

PentagonGroebnerSystems::usage =
  "PentagonGroebnerSystems[r,z] calculates a Groebner basis for the pentagon equations of the "<>
  "fusion ring r in variables labeled by z.";
(*Options include all options for solving the pentagon equations and all options for finding groebner bases.";*)

Options[PentagonGroebnerSystems] :=
  Union[
    {
      "ReduceRoots" -> True,
      "ReducePowerSums" -> True,
      "SimplifyIntermediateResultsBy" -> Identity
    },
    Options[PreparePentagonSolverInput],
    Options[ReduceByLinearity],
    Options[IncrementalGroebnerBasis]
  ];

PentagonGroebnerSystems[ ring_FusionRing?FusionRingQ, var_, opts:OptionsPattern[] ] :=
  Which[
    (* CHECK proper format injected solution *)
    !MatchQ[ OptionValue["InjectSolution"], {} | { _FusionRing, _?PPSQ } ]
    ,
    Message[ PentagonGroebnerSystems::substitutesolutionwrongformat, ring ];
    Abort[]
    ,
    (* CHECK multiplicity *)
    Mult[ring] == 1
    ,
    MultiplicityFreePentagonGroebnerSystems[ ring, var, opts ]
    ,
    True
    ,
    Print["Not implemented yet"];
    Abort[]
  ];


Options[MultiplicityFreePentagonGroebnerSystems] :=
  Options[PentagonGroebnerSystems];

MultiplicityFreePentagonGroebnerSystems[ ring_, var_, opts:OptionsPattern[] ] :=
  Module[{
    preEqCheck, useDBQ, storeDecompQ, procID, result, time, solverInput,
    sumSystems, SumBinEqns, systems, AddValues, ReduceSystems, reducedSystems1, reducedSystems2, simplify,
    invertibilityConstraints
    },
    preEqCheck =
      OptionValue["PreEqualCheck"];
    useDBQ =
      OptionValue["UseDatabaseOfSmithDecompositions"];
    storeDecompQ =
      OptionValue["StoreDecompositions"];
    simplify =
      Composition[
        If[
          OptionValue["ReducePowerSums"], PowerSumReduce, Identity
        ],
        If[
          OptionValue["ReduceRoots"] && MemberQ[ sumSystems, _Root, Infinity ],
          SafeRootReduce,
          Identity
        ],
        OptionValue["SimplifyIntermediateResultsBy"]
      ];

    procID =
      ToString[Unique[]];

    printlog[ "MFPGS:init", { procID, ring, var, {opts} } ];

    { time, result } =
      AbsoluteTiming[
        If[
          Rank[ring] == 1,
          Return[ { { { }, { F[1,1,1,1,1,1] -> 1 } } } ]
        ];

        SumBinEqns =
          Reverse @* BinomialSplit;

        solverInput =
          AddOptions[opts][PreparePentagonSolverInput] @ ring;

        sumSystems =
          DeleteCases[ { _, {} } ] @
          Table[
            {
              Union[
                input["Zeros"],
                input["SpecificFs"],
                input["ExtraFixedFs"],
                input["SubSolution"],
                Thread[ Cases[ FSymbols[ring], $VacuumFPattern ] -> 1 ]
              ],
              ReduceByBinomials[
                Sequence @@ SumBinEqns[ input["Equations"] ],
                input["Variables"],
                var,
                "Symmetries" -> input["Symmetries"],
                "InvertibleMatrices" -> input["InvertibleMatrices"],
                "NonSingular" -> True,
                "PreEqualCheck" -> preEqCheck,
                "UseDatabaseOfSmithDecompositions" -> useDBQ,
                "StoreDecompositions" -> storeDecompQ
              ]
            },
            { input, solverInput }
          ];

        If[
          sumSystems === {},
          Return @
            {
              <|
                "GroebnerBasis" -> { 1 },
                "Assumptions" -> None,
                "Rules" -> { }
              |>
            }
        ];

        AddValues[ { preKnowns_, systems_ } ] :=
          Table[
            <|
              "Polynomials" -> ToPolynomial @ sys[[1]],
              "Assumptions" -> True,
              "Rules" -> Union[ preKnowns, sys[[2]] ]
            |>
            , { sys, systems }
          ];

        (* Set up polynomial systems *)
        systems =
          Flatten[ AddValues /@ sumSystems ];


        (* Reduce the systems using linearity *)
        ReduceSystems[ system_ ] :=
          With[
            { newSystems = AddOptions[opts][ReduceByLinearity][ system["Polynomials"], var ] },
            Table[
              <|
                "Polynomials" -> simplify @ nSys["Polynomials"],
                "Assumptions" -> nSys["Assumptions"],
                "Rules"       -> system["Rules"]/.nSys["Rules"]
              |>,
              { nSys, newSystems }
            ]
          ];

        reducedSystems1 =
          Flatten[ ReduceSystems /@ systems ];
        
        invertibilityConstraints[ rules_ ] :=
          And @@
          DeterminantConditions[ FMatrices[ring] ~ WithMinimumDimension ~ 2 ]/.Dispatch[rules];

        printlog["MFPGS:systems", { procID, reducedSystems1 } ];

        (* If only 1 variable remains it is often faster to solve the system directly *)
        reducedSystems2 =
          Flatten[ QuickSolve[ #, var ]& /@ reducedSystems1 ];

        printlog["MFPGS:quicksolve", { procID, reducedSystems2 } ];

        Table[
          <|
            "GroebnerBasis" ->
            simplify @
            AddOptions[opts][IncrementalGroebnerBasis][
              sys["Polynomials"],
              GetVariables[ sys["Polynomials"], var ]
            ],
            "Assumptions" -> sys["Assumptions"] && invertibilityConstraints[ sys["Rules"] ],
            "Rules" -> sys["Rules"]
          |>,
          { sys, reducedSystems2 }
        ]

      ];

    printlog["Gen:results", { procID, result, time }];

    result

  ];


PackageExport["HexagonGroebnerSystems"]

HexagonGroebnerSystems::usage =
  "HexagonGroebnerSystems[r,z] calculates a Groebner basis for the hexagon equations of the fusion ring r variable z.";
(*Options include all options for solving the hexagon equations and all options for finding groebner bases.";*)

Options[HexagonGroebnerSystems] :=
  Join[
    { "ReducePowerSums" -> True },
    { "ReduceRoots" -> True },
    Options[PrepareHexagonSolverInput],
    Options[ReduceByBinomials],
    Options[ReduceByLinearity],
    Options[IncrementalGroebnerBasis]
  ];

HexagonGroebnerSystems[ ring_FusionRing?FusionRingQ, var_, opts:OptionsPattern[] ] :=
  Which[
    Mult[ring] == 1,
      MultiplicityFreeHexagonGroebnerSystems[ ring, var, opts ],
    True,
      Print["Not implemented yet"]
  ];

Options[MultiplicityFreeHexagonGroebnerSystems] :=
  Options[HexagonGroebnerSystems];

MultiplicityFreeHexagonGroebnerSystems[ ring_FusionRing, var_, opts:OptionsPattern[] ] :=
  Module[{ equations, variables, symmetries, SumBinEqns, knowns, g, sumSystems,
    opt, systems, AddKnowns, ReduceSystems,  procID, result, time, reducedSystems, reduceRoots,
    parallelQ, Groebner, simplify },
    procID =
      ToString @ Unique[];
    parallelQ =
      OptionValue["Parallel"];

    simplify =
      Composition[
        OptionValue["SimplifyIntermediateResultsBy"],
        If[
          OptionValue["ReduceRoots"] && MemberQ[ sumSystems, _Root, Infinity ],
          SafeRootReduce,
          Identity
        ],
        If[
          OptionValue["ReducePowerSums"],
          PowerSumReduce,
          Identity
        ]
      ];


    printlog["MFHGS:init", { procID, ring, var, { opts } } ];

    { time, result } =
    AbsoluteTiming[
      If[
        Rank[ring] == 1,
        Return[ { { { }, { F[1,1,1,1,1,1] -> 1, R[1,1,1] -> 1 }  } } ]
      ];

      SumBinEqns =
        Reverse @* BinomialSplit;

      { equations, variables, symmetries, knowns } =
        AddOptions[opts][PrepareHexagonSolverInput][ring] /@
        { "Equations", "Variables", "Symmetries", "Knowns" };

      opt =
        If[
          FreeQ[ variables, F[__] ],
          (* THEN  *)
          "NonSingular" -> True,
          (* ELSE: some F-symbols might be 0: can't set option "NonSingular" *)
          "InvertibleMatrices" ->
            ReplaceAll[
              Join[
                {{#}}& /@ ( R @@@ NZSC[ring] ),
                FMatrices[ring]
              ],
              knowns
            ]
        ];

      (* TODO: can be shorter by using AddOptions *)
      sumSystems =
        ReduceByBinomials[
          Sequence @@ SumBinEqns[ equations ],
          variables,
          var,
          opt,
          "Symmetries" ->                       symmetries,
          "SimplifyIntermediateResultsBy" ->    simplify,
          "PreEqualCheck" ->                    OptionValue["PreEqualCheck"],
          "UseDatabaseOfSmithDecompositions" -> OptionValue["UseDatabaseOfSmithDecompositions"],
          "StoreDecompositions" ->              OptionValue["StoreDecompositions"]
        ];

      If[
        sumSystems === {},
        Return[
          {
            <|
              "GroebnerBasis" -> { 1 },
              "Assumptions" -> None,
              "Rules" -> FilterRules[ knowns, s_[__] /; s =!= R ]
            |>
          }
        ]
      ];

      AddKnowns[ { sumEqns_, rules_ } ] :=
        <| "Polynomials" -> ToPolynomial @ sumEqns, "Rules" -> Union[ knowns, rules ] |>;

      (* Set up polynomial systems *)
      systems =
        AddKnowns /@
        sumSystems;

      (* Reduce the systems using linearity *)
      ReduceSystems[ system_ ] :=
        With[
          { newSystems = AddOptions[opts][ReduceByLinearity][ system["Polynomials"], var ] },
          Table[
            <|
              "Polynomials" -> simplify @ nSys["Polynomials"],
              "Assumptions" -> nSys["Assumptions"],
              "Rules"       -> system["Rules"]/.nSys["Rules"]
            |>,
            { nSys, newSystems }
          ]
        ];

      reducedSystems[1] =
        Flatten[ simplify @* ReduceSystems /@ systems ];

      printlog["MFHGS:systems", { procID, reducedSystems[1] } ];

      (* If only 1 variable remains it is often faster to solve the system directly *)
      reducedSystems[2] =
        Flatten[ QuickSolve[ #, var ]& /@ reducedSystems[1] ];

      printlog["MFHGS:quicksolve", { procID, reducedSystems[2] } ];

      Table[
        <|
          "GroebnerBasis" ->
            simplify @
            AddOptions[opts][IncrementalGroebnerBasis][
              sys["Polynomials"],
              GetVariables[ sys["Polynomials"], var ]
            ],
          "Assumptions" -> sys["Assumptions"],
          "Rules" -> sys["Rules"]
        |>,
        { sys, reducedSystems[2] }
      ]

    ];

    printlog["Gen:results", { procID, result, time } ];

    result

  ];

PackageExport["HGS"]

HGS::usage = 
  "Shorthand for HexagonGroebnerSystems.";

HGS = 
  HexagonGroebnerSystems;


QuickSolve[ system_, var_ ] :=
  With[{pols = system["Polynomials"] },
    If[
      CountVariables[ pols, var ] === 1
      ,
      With[
        {
          simplestPol =
            First @
            Flatten @
            MinimalBy[ pols, Exponent[ #, var ]&, 1 ],
          assumptions =
            system["Assumptions"]
        },
        {
          soln =
            Cases[
              ToNumericRootIsolation @
              SolveUsingReduce[
                simplestPol == 0,
                { var }
              ],
              sol_ /;
              And[
                TrueQ[ assumptions /. sol ],
                MatchQ[ SafeRootReduce[ pols/.sol ], {0...} ]
              ]
            ]
        },
        Table[
          Association @ {
            "Polynomials" -> {},
            "Assumptions" -> True,
            "Rules" -> system["Rules"] /. s
          },
          { s, soln }
        ]
      ]
      ,
      system
    ]
  ];


PackageExport["ParallelGroebnerBasis"]

ParallelGroebnerBasis::usage =
  "Calculates Groebner bases with different permutations of the variables on different kernels and returns the "<>
  "first result found.";

Options[ ParallelGroebnerBasis ] :=
  Options[ GroebnerBasis ];

ParallelGroebnerBasis[ pols_, vars_, opts:OptionsPattern[] ] :=
  Module[ { Groebner, varsLists, basis },
    Quiet[
      LaunchKernels[];

      varsLists =
        If[
          (* More orders than kernels *)
          Length[vars]! > $KernelCount,
          (* THEN: take random permutations *)
          SeedRandom[1];
          Prepend[vars] @
          Table[ Permute[ vars, RandomPermutation[Length[vars]] ], { $KernelCount - 1 } ],
          (* ELSE: all permutations *)
          Permutations[ vars ]
        ];

      Groebner[ variables_ ] :=
        GroebnerBasis[ pols, variables, opts ];

      DistributeDefinitions[Groebner];

      basis =
        ParallelTry[
          Groebner, varsLists
        ];

      CloseKernels[]
      ,
      { LaunchKernels::nodef }
    ];

    basis
  ];

PackageExport["PGB"]

PGB::usage =
  "Shorthand for ParallelGroebnerBasis.";

PGB =
  ParallelGroebnerBasis;


PackageExport["IncrementalGroebnerBasis"]

IncrementalGroebnerBasis::usage =
  "Calculates Groebner basis by incrementally taking subsets of pols.";

Options[IncrementalGroebnerBasis] :=
  Join[
    {
      "SimplifyIntermediateResultsBy" -> Identity,
      "ReduceRoots" -> True,
      "ReducePowerSums" -> False,
      "Cutoff" -> .2,
      "Parallel" -> False,
      "GroebnerWeightFunction" -> PolynomialDegree
    },
    Options[GroebnerBasis]
  ];

IncrementalGroebnerBasis[ pols_, vars_ , opts : OptionsPattern[] ] :=
  Module[
    { cutoff, weight, ReduceSystem, Slice, RecursiveGroebner, parallelQ, Groebner,simplify },
    cutoff  =
      OptionValue["Cutoff"];
    weight =
      OptionValue["GroebnerWeightFunction"];
    parallelQ =
      OptionValue["Parallel"];

    simplify =
      Composition[
        OptionValue["SimplifyIntermediateResultsBy"],
        If[
          OptionValue["ReduceRoots"] && MemberQ[ sumSystems, _Root, Infinity ],
          SafeRootReduce,
          Identity
        ],
        If[
          OptionValue["ReducePowerSums"],
          PowerSumReduce,
          Identity
        ]
      ];

    Slice[ sys_, gb_ ] :=
      Join[
        gb,
        sys[[ ;; Ceiling[ Length[sys] * cutoff ] ]]
      ];

    Groebner =
      If[
        parallelQ,
        ParallelGroebnerBasis,
        GroebnerBasis
      ];

    ReduceSystem[ sys_, gb_ ] :=
      SortBy[ #, Function[ pol, weight[ pol, vars ] ] ] & @
      DeleteDuplicates @
      DeleteCases[0] @
      simplify @
      Map[
        PolynomialReduce[ #, gb, vars ][[2]] &,
        sys
      ];

    RecursiveGroebner[ sys_, gb_ ] :=
      If[
        sys === {},
        gb,
        With[{ newGB = simplify @ AddOptions[ opts ][ Groebner ][ Slice[ sys, gb ], vars ] },
          RecursiveGroebner[ ReduceSystem[ sys, newGB ], newGB ]
        ]
      ];

    RecursiveGroebner[ simplify @ pols, { } ]
  ];

PackageExport["IGB"]

IGB::usage =
  "Shorthand for IncrementalGroebnerBasis.";

IGB =
  IncrementalGroebnerBasis;
