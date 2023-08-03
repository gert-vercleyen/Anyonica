(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2023-04-05 *)

Package["Anyonica`"]

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
      True,
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


PackageExport["SolvePentagonEquations"]

SolvePentagonEquations::usage =
  "SolvePentagonEquations[ring] returns the solutions for the pentagon equations associated to the Fusion Ring ring.";

SolvePentagonEquations::substitutesolutionwrongformat =
  "\"SubstituteSolution\" should point to a couple { ring, solution } where ring is a fusion ring "<>
  "isomorphic to a subring of `1` and solution is a solution to the pentagon equations for ring.";

Options[SolvePentagonEquations] :=
  Options[PentagonGroebnerSystems];

SolvePentagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  Which[
    (* CHECK proper format injected solution *)
    !MatchQ[ OptionValue["InjectSolution"], {} | { r_FusionRing, s_?PPSQ } ]
    ,
    Message[ SolvePentagonEquations::substitutesolutionwrongformat, ring ]; Abort[]
    ,
    Mult[ring] == 1
    ,
    SolveMultiplicityFreePentagonEquations[ ring, opts ]
    ,
    True
    ,
    Print["Not implemented yet."];
    Abort[]
  ];

PackageExport["SPE"]

SPE::usage =
  "Shorthand for SolvePentagonEquations.";

SPE =
  SolvePentagonEquations;


Options[SolveMultiplicityFreePentagonEquations] :=
  Options[SolvePentagonEquations];


SolveMultiplicityFreePentagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  Module[ { procID, time, result, bases, z, simplify },
    procID =
      ToString[Unique[]];
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

    printlog[ "SMFPE:init", { procID, ring, {opts} } ];

    { time, result } =
      AbsoluteTiming[

        If[ (* Need special case for trivial ring to avoid errors *)
          Rank[ring] == 1,
          Return[ { { F[1,1,1,1,1,1] -> 1 } } ]
        ];

        bases =
          AddOptions[opts][PentagonGroebnerSystems][ ring, z ];

        printlog["SMFPE:solving_systems", { procID } ];

        MapAt[
          simplify,
          Flatten[
            AddOptions[opts][SolveGroebnerSystem][#,z]& /@
            bases,
            1
          ],
          { All, All, 2 }
        ]
      ];

    printlog["Gen:results", { procID, result, time } ];

    result
  ];


PackageExport["SolveHexagonEquations"]

SolveHexagonEquations::usage =
  "SolveHexagonEquations[r] solves the hexagon equations for the fusion ring r.";
(*"The option \"Knowns\" can be set to a list of rules of variables that are already known, e.g. a solution to the pentagon equations.";*)

Options[SolveHexagonEquations] :=
  Join[
    Options[SolveGroebnerSystem],
    Options[HexagonGroebnerSystems]
  ];

SolveHexagonEquations[ ring_FusionRing?FusionRingQ, z_Symbol, opts:OptionsPattern[] ] :=
  Which[
    Mult[ring] == 1,
      SolveMultiplicityFreeHexagonEquations[ ring, z, opts ],
    True,
      Print["Can't solve cases with multiplicity yet"]
  ];

SolveHexagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  SolveHexagonEquations[ ring, z, opts ];

PackageExport["SHE"]

SHE::usage =
  "Shorthand for SolveHexagonEquations.";

SHE =
  SolveHexagonEquations;


Options[SolveMultiplicityFreeHexagonEquations] :=
  Options[SolveHexagonEquations];

SolveMultiplicityFreeHexagonEquations[ ring_FusionRing, z_ , opts:OptionsPattern[] ]:=
  Module[{ procID, time, result, bases },
    procID =
      ToString @ Unique[];

    printlog["SMFHE:init", { procID, ring, { opts } } ];

    { time, result } =
      AbsoluteTiming[
        If[
          Rank[ring] == 1,
          Return[ { { F[ 1, 1, 1, 1, 1, 1 ] -> 1, R[ 1, 1, 1 ] -> 1 } } ]
        ];

        bases =
          AddOptions[opts][HexagonGroebnerSystems][ ring, z ];

        Flatten[
          AddOptions[opts][SolveGroebnerSystem][#,z]& /@
          bases,
          1
        ]
      ];

    printlog["Gen:results", { procID, result, time } ];

    result

  ];
