(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-03-09 *)

Options[PreparePentagonSolverInput] =
  {
    "FindZerosUsingSums" -> True,
    "GaugeDemands" -> {},
    "ZeroValues" -> None,
    "NonSingular" -> False,
    "PreEqualCheck" -> Identity,
    "UseDatabaseOfSmithDecompositions" -> True,
    "StoreDecompositions" -> True,
    "InjectSolution" -> {}
  };

PreparePentagonSolverInput[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  Module[ {
    monEqns, gaugeSymmetries, pentEqns, sumEqns, invMats, extraFixedFs, zeros, fSymbols, tower,
    unionZeros, sharedVars, remainingSym, specificSym, specificFixedFs,
    solverInput, g, dz, solutions, time, procID, zeroEquations,
    useSumsQ, gaugeDemands, zeroValues, nonSingularQ, preEqCheck, useDBQ, storeDecompQ,
    subsSol, inject, compatibleSol,sRing, sSol, allFSymbols, vacuumSymbols
    },
    useSumsQ =
      OptionValue["FindZerosUsingSums"];
    gaugeDemands =
      OptionValue["GaugeDemands"];
    zeroValues =
      OptionValue["ZeroValues"];
    nonSingularQ =
      OptionValue["NonSingular"];
    preEqCheck =
      OptionValue["PreEqualCheck"];
    useDBQ =
      OptionValue["UseDatabaseOfSmithDecompositions"];
    storeDecompQ =
      OptionValue["StoreDecompositions"];
    subsSol =
      OptionValue["InjectSolution"];
    
    procID =
      ToString[Unique[]];
    
    printlog["PPSI:init", { procID, ring, {opts} } ];
    
    { time, solutions } =
    AbsoluteTiming[
    
    If[ (* Want to substitute solution *)
      subsSol =!= {},
      
      (* THEN *)
      { sRing, sSol } =
        subsSol;
     
      (* Function that maps labels in sRing to corresponding labels in ring *)
      inject =
        InjectionForm[ ring, sRing ];
      
      (* Rename all labels of sSol so they correspond to the labels of the sub-fusion-ring of ring *)
      compatibleSol =
        MapAt[
          ReplaceAll[ i_Integer  :> inject[i] ],
          sSol,
          { All, 1 }
        ],
      
      (* ELSE *)
      compatibleSol =
        {}
    ];
    
    allFSymbols =
      FSymbols[ring];

    vacuumSymbols =
      Cases[ allFSymbols, $VacuumFPattern ];
    
    fSymbols =
      Complement[
        allFSymbols,
        vacuumSymbols,
        GetVars[ compatibleSol, F ]
      ];
    
    tower =
      PentagonTower[ ring, "Knowns" -> compatibleSol ];

    { monEqns, sumEqns } =
      MonSumEquationsFromTower[ tower ];

    pentEqns =
      Join[ monEqns, sumEqns ];

    invMats  =
      FMatrices[ ring ];

    gaugeSymmetries =
      GaugeSymmetries[ allFSymbols, g ];

    (* Update matrices and gauge symmetries to take account of known F's *)
    printlog[ "PPSI:restricting_gauges", { procID } ];
    
    (* Update gauges *)
    gaugeSymmetries =
      RestrictMultiplicativeSymmetries[
        gaugeSymmetries,
        vacuumSymbols ~ Join ~ compatibleSol[[;;,1]],
        g
      ];

    (* Remove trivial and known F-matrices from list of invertible matrices *)
    invMats =
      With[{ trivialMatrixPattern = ( {{#}}& /@ $VacuumFPattern ) },
        invMats //
        DeleteCases[ {{ x_?NumericQ }} | trivialMatrixPattern  ]
      ];

    printlog[ "PPSI:original_system", { procID, pentEqns, fSymbols } ];
    
    printlog[ "PSI:zero_Fs", { procID } ];
    
    (* Find Configurations of non-trivial 0-values *)
    zeros =
      Which[
        OptionValue["NonSingular"],
          {{}},
        OptionValue["ZeroValues"] =!= None,
          Dispatch @ OptionValue["ZeroValues"],
        GroupQ[ring],
          {{}},
        True,
          zeroEquations =
            If[
              OptionValue[ "FindZerosUsingSums" ],
              pentEqns,
              monEqns
            ];
          
          Dispatch[
            Cases[ HoldPattern[ _ -> 0 ] ] /@
            FindZeroValues[
              zeroEquations,
              fSymbols,
              "InvertibleMatrices" -> invMats
            ]
          ]
      ];
    
    printlog["PSI:zero_Fs_results", { procID, Normal @ zeros } ];
    
    printlog["PSI:fixing_gauge", {procID } ];
    (* Break Gauge Symmetry: first for all variables that are never 0, i.e.
       that do not appear in any of the "zeros" from previous step *)

    (* Get all F-symbols that could be 0 for some configuration in zeros *)
    unionZeros =
      Union @@
      Normal[zeros][[;;,;;,1]];

    (* Get all F-symbols that can never be 0 *)
    sharedVars =
      Complement[ fSymbols, unionZeros ];

    (* Fix the gauge for all the F symbols that can never be 0 *)
    { remainingSym, extraFixedFs } =
      BreakMultiplicativeSymmetry[
        gaugeSymmetries,
        "GaugeDemands" -> OptionValue["GaugeDemands"],
        "ExcludedVariables" -> unionZeros
      ];

    (* Remove the newly fixed F-symbols from the list of variables *)
    fSymbols =
      Complement[ fSymbols, extraFixedFs[[;;,1]] ];

    (* Substitute the values of the newly fixed F-symbols in the invertible matrices
       and remove trivial 1D F-matrices. *)
    
    invMats =
      invMats/.extraFixedFs //
      DeleteCases[ {{n_?NumericQ}} /; n != 0 ];

    (* Substitute the values of the newly fixed F-symbols in the equations,
       remove trivial equations and delete duplicate equations *)
    
    pentEqns =
      pentEqns/.extraFixedFs //
      DeleteCases[True] //
      DeleteDuplicates;

    printlog[ "PSI:fixed_fs", { procID, extraFixedFs } ];

    (* Try to fix extra gauges, if possible, for each of the 0-configurations.
      Also substitute 0 values, and update equations, variables, and matrices *)
    
    printlog[ "PSI:fixing_extra_gauges", { procID } ];
    
    If[ (* All gauges are fixed *)
      remainingSym["Transforms"][[;;,1]] === remainingSym["Transforms"][[;;,2]],
      
      (* THEN *)
      printlog[ "PSI:no_gauge_freedom_left", { procID } ];
      solverInput =
        Reap[
          Do[
            dz =
              Dispatch[z];
            
            Sow[
              <|
                "Equations"  -> ( pentEqns/.dz // DeleteCases[True] // DeleteDuplicates ),
                "Variables"  -> Complement[ fSymbols, z[[;;,1]]  ],
                "Symmetries"   -> AddZerosToSymmetries[ remainingSym, dz ],
                "InvertibleMatrices"  -> ( invMats/.dz ),
                "SpecificFs" -> {},
                "ExtraFixedFs"-> extraFixedFs,
                "SubSolution" -> compatibleSol,
                "Zeros" -> z
              |>
            ],
            { z, Normal[zeros] }
          ]
        ][[2,1]],
      
      (* ELSE *)
      printlog[ "PSI:gauge_freedom_left", { procID } ];
      solverInput =
        Reap[
          Do[
            dz =
              Dispatch[z];
            
            { specificSym, specificFixedFs } =
              QuietLog[
                BreakMultiplicativeSymmetry[
                  AddZerosToSymmetries[ remainingSym, z ]
                ]
              ];

            Sow[
              <|
                "Equations" -> (pentEqns/.dz/.specificFixedFs // DeleteCases[True] // DeleteDuplicates),
                "Variables" -> Complement[ fSymbols, specificFixedFs[[;;,1]], z[[;;,1]] ],
                "Symmetries" -> specificSym,
                "InvertibleMatrices" -> (invMats/.dz/.specificFixedFs // DeleteCases[ {{n_?NumericQ}} /; n != 0 ]),
                "SpecificFs" -> specificFixedFs,
                "ExtraFixedFs" -> extraFixedFs,
                "SubSolution" -> compatibleSol,
                "Zeros" -> z
              |>
            ],
            { z, Normal[zeros] }
          ]
        ][[2,1]]
      ]
    ];
    
    printlog["Gen:results", { procID, solutions, time }];
    
    solutions
  ];

Options[ PrepareHexagonSolverInput ] =
  Options[ HexagonEquations ];

PrepareHexagonSolverInput[ ring_FusionRing, opts:OptionsPattern[]  ] :=
  Module[{ rSymbols, fSymbols, gaugeSym, g, knowns, time, result, procID, equations },
    rSymbols =
      R @@@ NZSC[ ring ];
    fSymbols =
      FSymbols[ ring ];

    knowns =
      Join[
        If[
          OptionValue["TrivialVacuumSymbols"],
          (* THEN *)
          Thread[
            Cases[ Join[ rSymbols, fSymbols ], $VacuumRPattern | $VacuumFPattern ] -> 1
          ],
          (* ELSE *)
          { }
        ],
        Normal @ OptionValue["Knowns"]
      ];

    procID =
      ToString @ Unique[];

    printlog["PHSI:init", { procID, ring, { opts } } ];

    printlog["PHSI:knowns", { procID, knowns } ];

    { time, result } =
    AbsoluteTiming[

      (* The known symbols are fixed so we have to reduce the gauge symmetries *)
      gaugeSym =
        MapAt[
          SortBy[ #, First ]&,
          RestrictMultiplicativeSymmetries[
            GaugeSymmetries[ Join[ fSymbols, rSymbols ], g ],
            knowns[[;;,1]],
            g
          ],
          {1}
        ];

      printlog["PHSI:symmetries", { procID, gaugeSym } ];

      equations =
        AddOptions[opts][HexagonEquations] @ ring;

      <|
        "Equations" ->  equations,
        "Variables" ->  GetVars[ equations, { R, F } ],
        "Symmetries" -> gaugeSym,
        "Knowns" ->     knowns
      |>

    ];

    printlog["Gen:results", { procID, result, time } ];

    result
  ]