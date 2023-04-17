(* ::Package:: *)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                             BREAKING SYMMETRY                             |
|                                                                           |
+---------------------------------------------------------------------------+
*)

Options[BreakMultiplicativeSymmetry] =
  { "GaugeDemands" -> { }, "ExcludedVariables" -> {} };
BreakMultiplicativeSymmetry[ symmetries_, opts:OptionsPattern[] ] :=
  Module[
    {
      demands, SortTransforms, RemoveUnnecessarySymbols, g, SimplestVar, FixValue, UpdateSystem, procID, time, result,
      excludedVars, AssignValues, unfixedVars
    },
    procID =
      ToString @ Unique[];
    demands =
      OptionValue["GaugeDemands"];
    excludedVars =
      OptionValue["ExcludedVariables"];
    g =
      First @
      symmetries["Symbols"];
    (* put the demanded vars on top of the list and remove unnecessary symbols *)

    SortTransforms[ transforms_ ] :=
      Join @@
      BinSplit[
        transforms //
        DeleteCases[ ( v_ -> _ /; MemberQ[v] @ excludedVars ) | ( x_ -> x_ ) ],
        MemberQ[First[#]] @ demands[[;;,1]] &
      ];

    RemoveUnnecessarySymbols[ transforms_ ] :=
      Map[
        #[[1]] -> #[[2]]/#[[1]]&,
        transforms
      ];

    SimplestVar[ monomial_ ] :=
      With[{ fList = Cases[ monomial, Power[ g[i__], a_. ]  :> { g[i], a } ] },
        If[
          fList === {},
          Missing[],
          MinimalBy[
            fList,
            Last @* Abs,
            1
          ][[1,1]]
        ]
      ];

    FixValue[ a_ -> b_ ] :=
      With[{ var = SimplestVar @ b },
        Solve[ b == 1, var ][[1]] //
        ReplaceAll[ HoldPattern[ Times[ _?NumericQ, x__ ] ] :> Times[x] ]
      ];

    UpdateSystem[ {}, fixedVars_ ] :=
      fixedVars;
    UpdateSystem[ transforms_, fixedVars_ ] :=
      UpdateSystem[
        (* Substitute new fixed gauge variable, then remove all transforms without gauge variables *)
        transforms //
        ReplaceAll[ FixValue[ First @ transforms ]] //
        Cancel //
        DeleteCases[ rule_ /; GetVars[ rule, g ] === {} ]
        ,
        Append[ transforms[[1,1]] ] @ fixedVars
      ];

    AssignValues[ demands_ ][ fixedVars_ ] :=
      With[{ extendedDemands = Append[ _ -> 1 ] @ demands },
        Map[
          Function[ var, var -> Replace[ var, extendedDemands ] ],
          fixedVars
        ]
      ];

    printlog["BMS:init", { procID, symmetries, {opts} } ];

    { time, result } =
      AbsoluteTiming[
        With[
          {
            fixedVars =
              UpdateSystem[
                symmetries["Transforms"] //
                SortTransforms //
                RemoveUnnecessarySymbols,
                { }
              ]
          },
          {
            RestrictMultiplicativeSymmetries[ symmetries, fixedVars, g ],
            AssignValues[ demands ] @
            fixedVars
          }
        ]
      ];

    If[
      demands =!= {} &&
      ( unfixedVars = Complement[ demands[[;;,1]], result[[;;,1]] ] ) =!= {},
      printlog[ "BMS:unfixed_demanded_vars", { procID, unfixedVars } ];
    ];

    printlog["Gen:results", { procID, result, time } ];

    result
  ];

(* Sometimes you want to fix certain values of variables. In that case the number of DOF's
for the symmetry transformations gets reduced as well *)
Options[RestrictMultiplicativeSymmetries] =
  Options[SolveModZ];
RestrictMultiplicativeSymmetries[ None, __ ] :=
  None;
RestrictMultiplicativeSymmetries[ sym_, {}, __ ] :=
  sym;
RestrictMultiplicativeSymmetries[
  sym_Association?MultiplicativeSymmetriesQ, vars_, symbol_, opts:OptionsPattern[] ] :=
  Module[{
    s, gaugeVars, restrictingBinomials, newEqns, newVars, revertVars, CSpace,
    params, newTransforms, reParametrization, transforms, symbols, procID, result, time
    },
    transforms =
      sym["Transforms"];
    symbols =
      sym["Symbols"];
    procID =
      ToString @ Unique[];

    printlog[ "RMS:init", { procID, sym, vars, symbols, {opts} } ];

    { time, result } =
    AbsoluteTiming[
    (* If all gauges are trivial, remove the variables from the system *)
    If[
      !GaugeFreedomQ[sym],
      Return[
        <|
        "Transforms" -> Cases[ transforms, HoldPattern[ f_ -> f_ ] /; FreeQ[f] @ vars ],
        "Symbols" -> {symbol}
        |>
      ]
    ];

    (* Obtain al gauge factors *)
    restrictingBinomials =
      DeleteCases[True] @
      ( Thread[ (vars/.transforms) == 1 ]/.Thread[ vars -> 1 ] );

    gaugeVars =
      GetVars[ transforms, symbols ];

    { newEqns, newVars, revertVars } =
      SimplifyVariables[ restrictingBinomials, gaugeVars, s ];

    CSpace =
      Last @
      SolveModZSpace[ MonPolEqnsToMatSys[ newEqns, Length[newVars], s ], opts ];

    params =
      symbol /@ Range[ Dimensions[CSpace][[2]] ];

    reParametrization =
      Thread[ newVars -> ( Inner[ Power, params, #, Times ]& /@ CSpace ) ]/.revertVars;

    newTransforms =
      (
        sym["Transforms"] //
        DeleteCases[ HoldPattern[ var_ -> _ ] /; MemberQ[ vars, var ] ]
      ) /.reParametrization;

    <|
      "Transforms" -> newTransforms,
      "Symbols" -> { symbol }
    |>
    ];

    printlog["Gen:results", { procID, result, time }];

    result

  ];

AddZerosToSymmetries[ sym_Association, zeros_ ] :=
  With[{
    transforms = sym["Transforms"],
    symbols    = sym["Symbols"]
    },
    <|
      "Transforms" -> ((transforms/.zeros) // DeleteCases[ 0 -> _ ]),
      "Symbols" -> symbols
    |>
  ];

GaugeFreedomQ[ sym_Association ] := With[{
  transforms = sym["Transforms"]
  },
  transforms[[;;,1]] =!= transforms[[;;,2]]
];

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                       FORCING UNITARY GAUGE DEMANDS                       |
|                                                                           |
+---------------------------------------------------------------------------+
*)

Options[UnitaryGaugeDemands] = { "SimplifyBy" -> Identity };
UnitaryGaugeDemands[ ring_, opts:OptionsPattern[] ] :=
  With[{
    qds  = SimplifyQDs[ QuantumDimensions[ring], opts ],
    dual = CC[ring]
    },
    Table[
      F[ a, dual[a], a, a, 1, 1 ] -> 1 / qds[[a]],
      { a, Rank[ring] }
    ]
  ];

Options[SimplifyQDs] = Options[UnitaryGaugeDemands];
SimplifyQDs[ qds_, OptionsPattern[] ] :=
  With[{
    s = OptionValue["SimplifyBy"][ qds ]
    },
    Which[
      N[qds] != N[s],
        qds,
      MemberQ[ qds, d_ /; d < 1 ],
        qds,
      True,
        s
    ]
  ];

(*
+---------------------------------------------------------------------------+
|                                                                           |
|          REMOVING SOLUTIONS THAT ARE EQUIVALENT DUE TO SYMMETRY           |
|                                                                           |
+---------------------------------------------------------------------------+
*)


(* Note: assumes gaugeMatrix corresponds to system with non-zero F-symbols *)
Options[GaugeSymmetryEquivalentQ] =
  {
    "SimplifyBy" -> Identity,
    "Numeric" -> False,
    "Accuracy" -> 64,
    "UnitaryEquivalence" -> False,
    "PreEqualCheck" -> Identity
  };

GaugeSymmetryEquivalentQ[ gaugeMatrix_?MatrixQ, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Module[{
      mU, mR, rank, vec, numericQ, sd, simplify, values1, values2, unitaryGaugeQ, differentAbsQ,
      preEqCheck, expRHS, NonOneCoeff,noc, procID, time, result
    },
    procID =
      ToString @ Unique[];

    printlog[ "GSEQ:init", { procID, gaugeMatrix, sol1, sol2, { opts } } ];

    { time, result } =
      AbsoluteTiming[
        numericQ =
          OptionValue["Numeric"];
        sd =
          OptionValue["Accuracy"];
        simplify =
          OptionValue["SimplifyBy"];
        unitaryGaugeQ =
          OptionValue["UnitaryEquivalence"];
        preEqCheck =
          OptionValue["PreEqualCheck"];
        values1  =
          sol1[[;;,2]] //
          DeleteCases[0];
        values2  =
          sol2[[;;,2]] //
          DeleteCases[0];

        differentAbsQ =
          If[
            numericQ,
            Abs[ N[ #1, { Infinity, sd } ] ] - Abs[ N[ #2, { Infinity, sd } ] ] != 0&,
            simplify[ Abs[ #1 ] - Abs[ #2 ] ] != 0&
          ];

        Catch[
          If[(* test for Unitary equivalence *)
            unitaryGaugeQ,
            (* THEN *)
            MapThread[
              If[
                differentAbsQ[ #1, #2 ],
                printlog["GSEQ:unitary_different_abs", { procID } ];
                Throw[ False ]
              ]&,
              { values1, values2 }
            ]
          ];

          { mU, mR } =
            HermiteDecomposition[ gaugeMatrix ];

          rank =
            Length[mR] -
            Count[
              mR,
              row_ /;
              MatchQ[ row, { 0 .. } ]
            ];

          expRHS =
            simplify @
            Inner[
              Power,
              If[
                numericQ,
                N[ #, { Infinity, sd } ]&,
                Identity
              ] @
              (values1/values2),
              Transpose[ mU ],
              Times
            ];

          NonOneCoeff[ l_ ] :=
            FirstCase[ l, x_ /; preEqCheck[x] != 1 ];

          If[
            rank < Length[ expRHS ] &&
            Head[ noc = NonOneCoeff @ expRHS[[rank+1;;]] ] =!= Missing,
            printlog["GSEQ:nonone_coeff", { procID, expRHS, rank, noc } ];
            False,
            True
          ]
        ]
      ];

    printlog["Gen:results", { procID, result, time } ];

    result

  ];

GaugeSymmetryEquivalentQ[ symmetries_Association, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Module[ { simplify, numericQ, sd, gaugeMatrix, zeroPos1, zeroPos2, differentQ },
    simplify =
      OptionValue["SimplifyBy"];
    numericQ =
      OptionValue["Numeric"];
    sd =
      OptionValue["Accuracy"];
    differentQ =
      If[
        numericQ,
        TrueQ[ N[ #1 - #2, { Infinity, sd } ] == 0 ]&,
        TrueQ[ simplify[#1 - #2 ] == 0 ]&
      ];
    
    If[ (* Gauges are trivial *)
      Times @@ Dimensions[ MultiplicativeGaugeMatrix[ symmetries ] ] === 0,
      TrivialGaugeSymmetryEquivalentQ[ opts ][ sol1, sol2 ]
    ];
    
    (* Check whether zeros are at same positions *)
    { zeroPos1, zeroPos2 } =
      Position[ _ -> 0 ] /@
      { sol1, sol2 };

    If[
      zeroPos1 =!= zeroPos2,
      Return[ False ]
    ];

    gaugeMatrix =
      MultiplicativeGaugeMatrix[
        MapAt[
          Delete[ {#}& /@ zeroPos1 ],
          symmetries,
          {1}
        ]
      ];

    GaugeSymmetryEquivalentQ[ gaugeMatrix, opts ][ sol1, sol2 ]

  ];

Options[TrivialGaugeSymmetryEquivalentQ] = Options[GaugeSymmetryEquivalentQ];
TrivialGaugeSymmetryEquivalentQ[ opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  With[{
    differentQ =
      If[
        OptionValue["Numeric"],
        Not[TrueQ[ N[ #1 - #2, { Infinity, OptionValue["Accuracy"] } ] == 0 ]]&,
        Not[TrueQ[ OptionValue["SimplifyBy"][#1 - #2 ] == 0 ]]&
      ]
    },
    Catch[
      MapThread[
        If[ differentQ[ #1, #2 ], Throw[False] ]&,
        { sol1, sol2 }
      ];
      True
    ]
  ];

Options[SymmetryEquivalentQ] = Options[GaugeSymmetryEquivalentQ];
SymmetryEquivalentQ[ ring_FusionRing, symmetries_Association, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Catch[
    Do[
      If[
        GaugeSymmetryEquivalentQ[ symmetries, opts ][
          PermutedFSymbols[ sol1, auto ],
          sol2
        ],
        Throw[True]
      ],
      { auto, FusionRingAutomorphisms[ring] }
    ];
    False
  ];

SymmetryEquivalentQ[ ring_FusionRing, gaugeMat_?MatrixQ, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Catch[
    Do[
      If[
        GaugeSymmetryEquivalentQ[ gaugeMat, opts ][ PermutedFSymbols[ sol1, auto ], sol2 ],
        Throw[True]
      ],
      { auto, FusionRingAutomorphisms[ring] }
    ];
    False
  ];

PermutedFSymbols[ FSymb_, perm_List ] :=
  Thread[
    Rule[
      FSymb[[;;,1]],
      FSymb[[;;,1]] /. ( n_Integer :> perm[[n]] ) /. FSymb
    ]
  ];


Options[DeleteSymmetryEquivalentSolutions] =
  Options[SymmetryEquivalentQ];

DeleteSymmetryEquivalentSolutions[ soln_, ring_, symmetries_, opts:OptionsPattern[] ] :=
  Module[{ groupedSoln, gaugeMatrices, procID, result, time },
    procID =
      ToString @ Unique[];

    printlog[ "DSES:init", { procID, soln, ring, symmetries, { opts } } ];

    { time, result } =
      AbsoluteTiming[
        If[
          (* Trivial Gauge Transform *)
          Times @@ Dimensions[ MultiplicativeGaugeMatrix[ symmetries ] ] === 0,
          (* THEN *)
          printlog["DSES:trivial_gauge_transform", {procID} ];
          Return @
          DeleteDuplicates[
            soln,
            SymmetryEquivalentQ[ symmetries, ring, opts ]
          ]
        ];

        (* Group solutions by appearance of 0 values. *)
        groupedSoln =
          GroupBy[ soln, Position[ _ -> 0 ] ];

        printlog[ "DSES:groups", { procID, groupedSoln } ];

        gaugeMatrices =
          Table[
            MultiplicativeGaugeMatrix[ MapAt[ Delete[ # , zeroPos ]&,  symmetries, {1} ] ],
            { zeroPos, Keys[groupedSoln] }
          ];

        Join @@
        MapThread[
          DeleteDuplicates[ #1 , SymmetryEquivalentQ[ ring, #2, opts ]  ]&,
          { Values[ groupedSoln ], gaugeMatrices }
        ]
      ];

    printlog["Gen:results", { procID, result, time } ];
    result
  ];

MultiplicativeGaugeMatrix[ sym_Association ] :=
  Module[{ transforms, vars, gaugeVars, newPols, g, mat },
    transforms =
      sym["Transforms"][[;;,2]];
    vars =
      sym["Transforms"][[;;,1]];
    gaugeVars =
      GetVars[ transforms, sym["Symbols"] ];
    newPols =
      First @ SimplifyVariables[ (transforms/vars), gaugeVars, g ];
    
    MonPolsToMat[ newPols, Length[gaugeVars], g ]
  ];

MultiplicativeSymmetriesQ[ sym_Association ] :=
  With[{
    transforms =
      sym["Transforms"][[;;,2]],
    gaugeVars =
      sym["Transforms"][[;;,1]],
    gaugeFuncs =
      sym["Symbols"]},
    If[
      transforms === {},
      Return[ True ]
    ];

    With[{
      removeGauges = #/.Table[ x[__] -> 1, { x, gaugeFuncs } ]&
      },
      Not[ And @@ ( MonomialQ /@ ( transforms/gaugeVars ) ) ] ||
      Not[ And @@ ( NumericQ /@ removeGauges[transforms] ) ]
    ]
  ];

GaugeTransform[ g_Symbol ][ F[ a_, b_, c_, d_, e_, f_ ] ] :=
  ( g[ a, b, e ] g[ e, c, d ] )/( g[ a, f, d ] g[ b, c, f ] ) * F[ a, b, c, d, e, f ];

GaugeTransform[ g_Symbol ][ R[ a_, b_, c_ ] ] :=
  ( g[ a, b, c ] / g[ b, a, c ] )* R[a,b,c];

(* Special gauge transforms with extra parameter *)
GaugeTransform[ { g_Symbol, t_Symbol } ][ F[ a_, b_, c_, d_, e_, f_ ] ] :=
  (t[ a, b, c, d, e, f ] g[ a, b, e ] g[ e, c, d ] )/( g[ a, f, d ] g[ b, c, f ] ) * F[ a, b, c, d, e, f ];

GaugeTransform[ { g_Symbol, t_Symbol } ][ R[ a_, b_, c_ ] ] :=
  ( t[ a, b, c ] g[ a, b, c ] / g[ b, a, c ] )* R[ a, b, c ];

GaugeTransform[ g_Symbol ][ l_List ] :=
  GaugeTransform[g] /@ l;

GaugeTransform[ { g_Symbol, t_Symbol } ][ l_List ] :=
  GaugeTransform[ { g, t } ] /@ l;

ApplyGaugeTransform[ solution_, s_ ] :=
  With[
    { symbols = solution[[;;,1]]  },
    Thread[ symbols -> ReplaceAll[ GaugeTransform[s] /@ symbols, solution ]  ]
  ];

ApplyGaugeTransform[ solution_, gaugeValues_, s_ ] :=
  ApplyGaugeTransform[ solution, s ] /. gaugeValues;

GaugeSymmetries[ symbols_, g_ ] :=
  <|
    "Transforms" -> Table[ s -> GaugeTransform[g][s], { s, symbols } ],
    "Symbols" -> { g }
  |>;

TrivialGaugeMatrix[ symbols_ ] :=
  Module[{ constraints, g1, g2, newConstraints, newGaugeVars },
    constraints =
      DeleteCases[True] @
      Thread[
        ReplaceAll[
          GaugeTransform[g1][ symbols ],
          Thread[ symbols -> 1 ]
        ] == 1
      ];
    
    { newConstraints, newGaugeVars } =
      Most @
      SimplifyVariables[ constraints, GetVars[ constraints, g1 ], g2 ];
    
    Last @
    SolveModZSpace @
    MonPolEqnsToMatSys[ newConstraints, Length[newGaugeVars], g2 ]
  ];