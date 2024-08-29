(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-05 *)

Package["Anyonica`"]



PackageExport["ParallelGroebnerBasis"]

ParallelGroebnerBasis::usage =
"ParallelGroebnerBasis[pols,vars] calculates Groebner bases with different permutations of the variables on different kernels and returns the "<>
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
"IncrementalGroebnerBasis[pols,vars] calculates Groebner basis by incrementally taking subsets of pols.";

Options[IncrementalGroebnerBasis] :=
Join[
  {
    "SimplifyIntermediateResultsBy" -> Identity,
    "ReduceRoots" -> False,
    "ReducePowerSums" -> False,
    "Cutoff" -> .2,
    "Parallel" -> False,
    "GroebnerWeightFunction" -> PolynomialDegree
  },
  Options[GroebnerBasis]
];

IncrementalGroebnerBasis[ pols_, vars_, opts : OptionsPattern[] ] :=
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
  Map[
    simplify[ PolynomialReduce[ #, gb, vars ][[2]] ]&,
    sys
  ];
  
  RecursiveGroebner[ sys_, gb_ ] :=
  If[
    sys === {},
    gb,
    With[{ newGB = simplify /@ AddOptions[ opts ][ Groebner ][ Slice[ sys, gb ], vars ] },
      RecursiveGroebner[ ReduceSystem[ sys, newGB ], newGB ]
    ]
  ];
  
  RecursiveGroebner[ simplify /@ pols, { } ]
];

PackageExport["IGB"]

IGB::usage =
"Shorthand for IncrementalGroebnerBasis.";

IGB =
  IncrementalGroebnerBasis;


PackageScope["SolveGroebnerSystem"]

(* Construct solutions to the systems returned by the various GroebnerSystems functions *)
SolveGroebnerSystem[ system_Association, s_ ] :=
  With[{
    groebnerBasis = system["GroebnerBasis"],
    assumptions   = system["Assumptions"],
    rules         = system["Rules"]
    },
    Which[
      groebnerBasis === { 1 }
      ,
        { },
      groebnerBasis === { }
      ,
      If[ TrueQ[ assumptions/.rules ], { rules }, { } ]
      ,
      True
      ,
      With[{
        solutions =
          ToNumericRootIsolation @ (* Fix Root expressions with 1 as last arg *)
          Cases[
            SolveUsingReduce[
              Thread[ groebnerBasis == 0 ],
              GetVariables[ groebnerBasis, s ]
            ],
            sol_ /; TrueQ[ assumptions/.sol ]
          ]
        },
        rules/.Dispatch[solutions]
      ]
    ]
  ];

