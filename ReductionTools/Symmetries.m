(* ::Package:: *)

Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                             BREAKING SYMMETRY                             |
|                                                                           |
+---------------------------------------------------------------------------+
*)

PackageExport["BreakMultiplicativeSymmetry"]

BreakMultiplicativeSymmetry::usage =
  "BreakMultiplicativeSymmetry[symmetries] returns a couple of restricted symmetries and a list " <>
  "of values that have been fixed.";

BreakMultiplicativeSymmetry::clashingdemands =
  "Option \"Demands\" `1` is not allowed to assign values to variables given in Option \"ExcludedVariables\" `2`.";

BreakMultiplicativeSymmetry::symnotmultiplicative =
  "The symmetries `1` are not multiplicative";

Options[BreakMultiplicativeSymmetry] =
  {
    "GaugeDemands" -> { },
    "ExcludedVariables" -> { }
  };

BreakMultiplicativeSymmetry[ symmetries_, opts:OptionsPattern[] ] :=
  If[
    !MultiplicativeSymmetriesQ[symmetries]
    ,
    Message[ BreakMultiplicativeSymmetry::symnotmultiplicative, symmetries ];
    Abort[]
    ,
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
          DeleteCases[ ( (v_ -> _) /; MemberQ[v] @ excludedVars ) | ( x_ -> x_ ) ],
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
          DeleteCases[ rule_ /; GetVariables[ rule, g ] === {} ]
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
    ]
  ];

PackageExport["BMS"]

BMS::usage =
  "Shorthand for BreakMultiplicativeSymmetry.";

BMS =
  BreakMultiplicativeSymmetry;


PackageExport["RestrictMultiplicativeSymmetries"]

RestrictMultiplicativeSymmetries::usage =
  "RestrictMultiplicativeSymmetries[symmetries,vars,symbol] returns the symmetries that are left after demanding "<>
  "that the variables vars are constant.";
(*  The degrees of freedom of the new symmetries are parametrized by indexed variables: symbol[i]. The options are the same as those for the function SolveModZ and will be passed to an internal call to SolveModZ.";*)

RestrictMultiplicativeSymmetries::symnotmultiplicative =
  "The symmetries `1` are not multiplicative";

Options[RestrictMultiplicativeSymmetries] :=
  Options[SolveModZ];

RestrictMultiplicativeSymmetries[ None, __ ] :=
  None;

RestrictMultiplicativeSymmetries[ sym_, {}, __ ] :=
  If[
    !MultiplicativeSymmetriesQ[sym]
    ,
    Message[ RestrictMultiplicativeSymmetries::symnotmultiplicative, sym ];
    Abort[]
    ,
    sym
  ];

RestrictMultiplicativeSymmetries[ sym_, vars_, symbol_, opts:OptionsPattern[] ] :=
  If[
    !MultiplicativeSymmetriesQ[sym]
    ,
    Message[ RestrictMultiplicativeSymmetries::symnotmultiplicative, sym ];
    Abort[]
    ,
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
      (* If all gauge transforms of the knowns are trivial, remove the variables from the system *)
      If[
        !GaugeFreedomQ[ sym, vars ],
        Return[
          <|
          "Transforms" -> SimplifyVariables[
              DeleteCases[ transforms, HoldPattern[ f_ -> f_ ] /; MemberQ[f] @ vars ],
              symbols,
              symbol
            ][[1]],
          "Symbols" -> {symbol}
          |>
        ]
      ];

      (* Obtain al gauge factors *)
      restrictingBinomials =
        DeleteCases[True] @
        ( Thread[ (vars/.transforms) == 1 ]/.Thread[ vars -> 1 ] );

      gaugeVars =
        GetVariables[ transforms, symbols ];

      { newEqns, newVars, revertVars } =
        SimplifyVariables[ restrictingBinomials, gaugeVars, s ];

      CSpace =
        Last @
        SolveModZSpace[ BinToLin[ newEqns, newVars, s ], opts ];

      params =
        symbol /@ Range[ Dimensions[CSpace][[2]] ];

      reParametrization =
        Thread[ newVars ->  PowerDot[ params, CSpace ]  ]/.revertVars;

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

    ]
  ];

PackageExport["RMS"]

RMS::usage =
  "Shorthand for RestrictMultiplicativeSymmetries";

RMS =
  RestrictMultiplicativeSymmetries;


PackageScope["AddZerosToSymmetries"]

AddZerosToSymmetries::usage =
  "Removes the symbols that are 0 from the symmetries.";

AddZerosToSymmetries[ sym_Association, zeros_ ] :=
  With[{
    transforms = sym["Transforms"],
    symbols    = sym["Symbols"]
    },
    <|
      "Transforms" ->  DeleteCases[ 0 -> _ ] @ (transforms/.zeros),
      "Symbols" -> symbols
    |>
  ];


GaugeFreedomQ[ sym_Association ] :=
  With[{
    transforms = sym["Transforms"]
    },
    transforms[[;;,1]] =!= transforms[[;;,2]]
  ];

GaugeFreedomQ[ sym_Association, symbols_List ] :=
  With[ { rules = FilterRules[ sym["Transforms"], symbols ] },
    rules[[;;,1]] =!= rules[[;;,2]]
  ];

(*
+---------------------------------------------------------------------------+
|                                                                           |
|                       FORCING UNITARY GAUGE DEMANDS                       |
|                                                                           |
+---------------------------------------------------------------------------+
*)

(*Options[UnitaryGaugeDemands] = { "SimplifyBy" -> Identity };*)
(*UnitaryGaugeDemands[ ring_, opts:OptionsPattern[] ] :=*)
(*  With[{*)
(*    qds  = SimplifyQDs[ QuantumDimensions[ring], opts ],*)
(*    dual = CC[ring]*)
(*    },*)
(*    Table[*)
(*      F[ a, dual[a], a, a, 1, 1 ] -> 1 / qds[[a]],*)
(*      { a, Rank[ring] }*)
(*    ]*)
(*  ];*)

(*Options[SimplifyQDs] = Options[UnitaryGaugeDemands];*)
(*SimplifyQDs[ qds_, OptionsPattern[] ] :=*)
(*  With[{*)
(*    s = OptionValue["SimplifyBy"][ qds ]*)
(*    },*)
(*    Which[*)
(*      N[qds] != N[s],*)
(*        qds,*)
(*      MemberQ[ qds, d_ /; d < 1 ],*)
(*        qds,*)
(*      True,*)
(*        s*)
(*    ]*)
(*  ];*)

(*
+---------------------------------------------------------------------------+
|                                                                           |
|          REMOVING SOLUTIONS THAT ARE EQUIVALENT DUE TO SYMMETRY           |
|                                                                           |
+---------------------------------------------------------------------------+
*)

PackageExport["GaugeSymmetryEquivalentQ"]

GaugeSymmetryEquivalentQ::usage =
  "GaugeSymmetryEquivalentQ[sym][sol1,sol2] returns True if sol1 and sol2 are related via gauge symmetries sym.";

GaugeSymmetryEquivalentQ::symnotmultiplicative =
  "The symmetries `1` are not multiplicative";

(* Note: assumes gaugeMatrix corresponds to system with non-zero F-symbols: ADD TO POSSIBLE ISSUES *)

Options[GaugeSymmetryEquivalentQ] =
  {
    "SimplifyBy" -> Identity,
    "Numeric" -> False,
    "Accuracy" -> 64,
    "UnitaryEquivalence" -> False,
    "PreEqualCheck" -> Identity,
    "GaugeInvariants" -> {}
  };

GaugeSymmetryEquivalentQ[ gaugeMatrix_?MatrixQ, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  GaugeSymmetryEquivalentQ[ HermiteDecomposition @ gaugeMatrix, opts ][ sol1, sol2 ];


GaugeSymmetryEquivalentQ[ {r_?MatrixQ, h_?MatrixQ }, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Module[{
      rank, numericQ, sd, simplify, values1, values2, unitaryGaugeQ, differentAbsQ,
      preEqCheck, expRHS, NonOneCoeff,noc, procID, time, result, invariants
    },
    procID =
      ToString @ Unique[];

    printlog[ "GSEQ:init", { procID, { r, h }, sol1, sol2, { opts } } ];

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
        invariants =
          OptionValue["GaugeInvariants"];

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
          If[
            invariants =!= {} && N[ invariants/.sol1 , { Infinity, 16 } ] != N[ invariants/.sol2, { Infinity, 16 } ]
            ,
            Throw[ False ]
          ];

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

          rank =
            Length[h] -
            Count[
              h,
              row_ /;
              MatchQ[ row, { 0 .. } ]
            ];

          expRHS =
            simplify @
            PowerDot[
              If[
                numericQ,
                N[ #, { Infinity, sd } ]&,
                Identity
              ] @
              (values1/values2),
              mU
            ];

          NonOneCoeff[ l_ ] :=
            FirstCase[ l, x_ /; !TrueQ[ preEqCheck[x] == 1 ] ];

          If[
            rank < Length[ expRHS ] &&
            Head[ noc =  NonOneCoeff @ expRHS[[rank+1;;]] ] =!= Missing,
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
  If[
    !MultiplicativeSymmetriesQ[symmetries]
    ,
    Message[ GaugeSymmetryEquivalentQ::symnotmultiplicative, symmetries ];
    Abort[]
    ,
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
            Delete[ zeroPos1 ],
            symmetries,
            {1}
          ]
        ];

      GaugeSymmetryEquivalentQ[ gaugeMatrix, opts ][ sol1, sol2 ]

    ]
  ];

Options[TrivialGaugeSymmetryEquivalentQ] :=
  Options[GaugeSymmetryEquivalentQ];

TrivialGaugeSymmetryEquivalentQ[ opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  With[{
    differentQ =
      If[
        OptionValue["Numeric"],
        Not[TrueQ[ InfN[ #1 - #2, OptionValue["Accuracy"] ] == 0 ]]&,
        Not[TrueQ[ OptionValue["SimplifyBy"][#1 - #2] == 0 ]]&
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

PackageExport["SymmetryEquivalentQ"]

SymmetryEquivalentQ::usage =
  "SymmetryEquivalentQ[ring,symmetries][sol1,sol2] returns True if there exists a combination of a gauge transform and a fusion ring " <>
  "automorphism of ring that transforms sol1 into sol2";

Options[SymmetryEquivalentQ] :=
  Options[GaugeSymmetryEquivalentQ];

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
  SymmteryEquivalentQ[ ring, HermiteDecomposition @ gaugeMat, opts ][ sol1, sol2 ];

SymmetryEquivalentQ[ ring_FusionRing, { r_?MatrixQ, h_?MatrixQ }, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Catch[
    Do[
      If[
        GaugeSymmetryEquivalentQ[ { r, h }, opts ][ PermutedFSymbols[ sol1, auto ], sol2 ],
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
      FSymb[[;;,1]] /. ( n_Integer :> perm[[n]] ) /. Dispatch[FSymb]
    ]
  ];

PackageExport["SEQ"]

SEQ::usage =
  "Shorthand for SymmetryEquivalentQ.";

SEQ =
  SymmetryEquivalentQ;


PackageExport["DeleteEquivalentSolutions"]

DeleteEquivalentSolutions::usage =
  "DeleteEquivalentSolutions[ soln, ring, symmetries ] returns a list of representatives of " <>
  "equivalence classes of the solutions.";
(*"Here redundant means that there exists another solution in solList which is equivalent via a combination of a (possibly trivial) gauge transform and (possibly trivial) automorphism of the fusion ring. Possible options are (a) \"SimplifyBy\": function to simplify expressions whose values are checked, (b) \"Numeric\": Set to True to let the function check equality nummerically, (c) \"Accuracy\": Set the accuracy to use when checking values numerically (precision is always infinite).";*)

Options[DeleteEquivalentSolutions] :=
  Options[SymmetryEquivalentQ];

DeleteEquivalentSolutions[ soln_, ring_FusionRing, symmetries_, opts:OptionsPattern[] ] :=
  Module[{ groupedSoln, decomposedGaugeMatrices, procID, result, time, invariants },
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

        decomposedGaugeMatrices =
          HermiteDecomposition /@
          Table[
            MultiplicativeGaugeMatrix[ MapAt[ Delete[ # , zeroPos ]&, symmetries, {1} ] ],
            { zeroPos, Keys[groupedSoln] }
          ];

        invariants =
          GaugeInvariants @ ring;

        Join @@
        MapThread[
          DeleteDuplicates[ #1 , SymmetryEquivalentQ[ ring, #2, "GaugeInvariants" -> invariants, opts ] ]&,
          { Values[ groupedSoln ], decomposedGaugeMatrices }
        ]
      ];

    printlog["Gen:results", { procID, result, time } ];
    result
  ];

PackageExport["DES"]

DES::usage =
  "Shorthand for DeleteEquivalentSolutions.";

DES =
  DeleteEquivalentSolutions;


PackageScope["MultiplicativeGaugeMatrix"]

MultiplicativeGaugeMatrix[ sym_Association ] :=
  Module[{ t, var, factors, g, newVars },
    t =
      sym["Transforms"];
    var =
      sym["Symbols"];

    { factors, newVars } =
      Most @ SimplifyVariables[ t[[;;,2]]/t[[;;,1]], GetVariables[ t, var ], g ];

    GaugeMatRow[ #, Length[newVars], g ]& /@ factors
  ];


GaugeMatRow[ 1, n_, x_ ] :=
  ConstantArray[ 0, n ];

GaugeMatRow[ factor_, n_, x_ ] :=
  Normal @
  SparseArray[
    Cases[
      factor,
      Power[ x[i_], b_. ] :> ( { i } -> b  )
    ],
    { n }
  ];

(*
MultiplicativeGaugeMatrix[ sym_Association ] :=
  Module[{ transforms, vars, newVars, gaugeVars, eqns, g },
    transforms =
      sym["Transforms"][[;;,2]];
    vars =
      sym["Transforms"][[;;,1]];
    gaugeVars =
      GetVariables[ transforms, sym["Symbols"] ];

    If[ gaugeVars == {}, Return[{{}}] ];

    { eqns, newVars } =
      Most @ SimplifyVariables[ Thread[ ( transforms/vars ) == 1], gaugeVars, g ];

    First @ BinToSemiLin[ eqns, newVars , g ]
  ];
*)

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

    TrueQ @
    With[{
      removeGauges = #/.Table[ x[__] -> 1, { x, gaugeFuncs } ]&
      },
      Not[ And @@ ( MonomialQ /@ ( transforms/gaugeVars ) ) ] ||
      Not[ And @@ ( NumericQ /@ removeGauges[transforms] ) ]
    ]
  ];

PackageExport["GaugeTransform"]

GaugeTransform::usage =
  "GaugeTransform[g][ s[ ind ] ] applies a gauge transformation with parameters labeled by g to the symbol s" <>
  " with indices ind.";
(*Possible values of s are: \[ScriptCapitalF], \[ScriptCapitalR] (not implemented yet), etc.";*)

GaugeTransform[ g_Symbol ][ F[ a_, b_, c_, d_, e_, f_ ] ] :=
  ( g[ a, b, e ] g[ e, c, d ] )/( g[ a, f, d ] g[ b, c, f ] ) * F[ a, b, c, d, e, f ];

GaugeTransform[ g_Symbol ][ R[ a_, b_, c_ ] ] :=
  ( g[ a, b, c ] / g[ b, a, c ] )* R[a,b,c];

(* Special gauge transforms with extra parameter *)
GaugeTransform[ { g_Symbol, t_Symbol } ][ F[ a_, b_, c_, d_, e_, f_ ] ] :=
  (t[ a, b, c, d, e, f ] g[ a, b, e ] g[ e, c, d ] )/( g[ a, f, d ] g[ b, c, f ] ) * F[ a, b, c, d, e, f ];

GaugeTransform[ { g_Symbol, t_Symbol } ][ R[ a_, b_, c_ ] ] :=
  ( t[ a, b, c ] g[ a, b, c ] / g[ b, a, c ] ) * R[ a, b, c ];

GaugeTransform[ g_Symbol ][ l_List ] :=
  GaugeTransform[g] /@ l;

GaugeTransform[ { g_Symbol, t_Symbol } ][ l_List ] :=
  GaugeTransform[ { g, t } ] /@ l;

PackageExport["GT"]

GT::usage =
  "Shorthand for GaugeTransform.";

GT =
  GaugeTransform;


PackageExport["ApplyGaugeTransform"]

ApplyGaugeTransform::usage =
  "ApplyGaugeTransform[ solution, s ] applies a formal gauge transformation, with gauge variables labeled by"<>
  " s, to all values of solution.\n" <>
  "ApplyGaugeTransform[ solution, gaugeVals, s ] applies the gauge transformation with values determined by" <>
  " gaugeVals to solution.";

ApplyGaugeTransform[ solution_, s_ ] :=
  With[
    { symbols = solution[[;;,1]] },
    Thread[ symbols -> ReplaceAll[ GaugeTransform[s] /@ symbols, solution ]  ]
  ];

ApplyGaugeTransform[ solution_, gaugeValues_, s_ ] :=
  ApplyGaugeTransform[ solution, s ] /. gaugeValues;


PackageExport["AGT"]

AGT::usage =
  "Shorthand for ApplyGaugeTransform.";

AGT =
  ApplyGaugeTransform;


PackageExport["GaugeSymmetries"]

GaugeSymmetries::usage =
  "GaugeSymmetries[ symbols, s ] returns the gauge transforms of the symbols, with gauge variables labeled by s.";

GaugeSymmetries[ symbols_, g_ ] :=
  <|
    "Transforms" -> Table[ s -> GaugeTransform[g][s], { s, symbols } ],
    "Symbols" -> { g }
  |>;


PackageExport["GS"]

GS::usage =
  "Shorthand for GaugeSymmetries.";

GS =
  GaugeSymmetries;


PackageScope["TrivialGaugeMatrix"]

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
      SimplifyVariables[ constraints, GetVariables[ constraints, g1 ], g2 ];

    Last @
    SolveModZSpace @
    BinToLin[ newConstraints, newGaugeVars, g2 ]
  ];


PackageExport["GaugeSplitTransform"]

GaugeSplitTransform::usage =
  "GaugeSplitTransform[ ring ] returns { V, n } where V is a matrix and n an integer for which " <> "
  F'_1 = (F_1)^(V_1) * ... * (F_l)^(V_l) * (R_1)^(V_{l+1}) * ... * (R_k)^(V_{l+k}), ... } is a basis for all polynomials
  in the F-and R-symbols where the first n elements provides a basis for the gauge independent polynomials in the "<>
  "F-and R-symbols.";

GaugeSplitTransform::invalidoptionincludeonly =
  "The option for \"IncludeOnly\", `1`, must be either All, \"FSymbols\" or \"RSymbols\".";

Options[GaugeSplitTransform] :=
  {
    "IncludeOnly" -> All
  };

GaugeSplitTransform[ ring_, opts:OptionsPattern[] ] :=
  With[{ io = OptionValue["IncludeOnly"] },

    If[ Rank[ring] == 1, Return @ If[ io =!= All, IdentityMatrix[1], IdentityMatrix[2] ] ];

    Module[{ symbols, g, sym, m, monomial, powers, d, v, r, sortf },

      symbols =
        Switch[ io,
          All
          ,
          Join[ FSymbols[ring], RSymbols[ring] ]
          ,
          "FSymbols"
          ,
          FSymbols[ring]
          ,
          "RSymbols"
          ,
          RSymbols[ring]
          ,
          _
          ,
          Message[ GaugeSplitTransform::invalidoptionincludeonly, io ];
          Abort[]
        ];

      sym =
        GaugeSymmetries[ symbols, g ];

      monomial =
        PowerExpand[
          Inner[ Power, symbols, Array[ m, Length @ symbols ], Times ] /. sym["Transforms"] /. Thread[symbols -> 1]
        ];

      powers =
        Expand @
        Cases[ monomial, Power[ g[__], p_. ] :> p ];

      { d, v } =
        Rest @
        SmithDecomposition[
          powerToRow[ m, Length @ symbols ] /@ powers
        ];

      r =
        Length[
          DeleteCases[0] @ Diagonal[d]
        ];

      (* First sort criterium: number of factors, where powers are counted as multiple factors.
         Second criterium: canonical lexicographic order on the indices (we need Reverse because the greater the row,
         the smaller the labels *)

      sortf =
        Order[ Prepend[ Total @ Abs @ #1 ] @ Reverse[#1], Prepend[ Total @ Abs @ #2 ] @ Reverse[#2] ]&;

      {
        Transpose @ Join[
          Sort[ Transpose[v][[r+1;;]], sortf ],
          Sort[ Transpose[v][[;;r]], sortf ]
        ],
        Length[v] - r
      }
    ]
  ];


powerToRow[ s_, n_ ][ pow_ ] :=
  Normal @ SparseArray[ Cases[ pow, i_. * s[j_] :> { j } -> i ], {n} ];

PackageExport["GaugeSplitBasis"]

GaugeSplitBasis::usage =
  "GaugeSplitBasis[ ring ] returns a tuple of lists of monomials { m1, m2 } in F-and R-symbols where m1 contains a "<>
  "basis for all gauge invariant polynomials, and m2 contains the remaining monomials needed for m1 to be a basis"<>
  " of all F-and R-symbols of the ring. Options are \"IncludeOnly\" which can be set to \"FSymbols\" or \"RSymbols\""<>
  " if one only wants a split basis for the F-symbols or R-symbols respectively."

Options[ GaugeSplitBasis ] :=
  Options[GaugeSplitTransform];

GaugeSplitBasis[ ring_FusionRing, opts:OptionsPattern[] ] :=
  Module[ { V, n, symbols, io },
    io =
      OptionValue["IncludeOnly"];

    { V, n } =
      GaugeSplitTransform[ ring, opts ];

    symbols =
      Switch[ io,
        All
        ,
        Join[ FSymbols[ring], RSymbols[ring] ]
        ,
        "FSymbols"
        ,
        FSymbols[ring]
        ,
        "RSymbols"
        ,
        RSymbols[ring]
      ];

    Map[
      Inner[ Power, symbols, #, Times ]&,
      {
        Transpose @ V[[;;,;;n]],
        Transpose @ V[[;;,n+1;;]]
      },
      {2}
    ]
  ];

PackageExport["GaugeInvariants"]

GaugeInvariants::usage =
  "GaugeInvariants[ ring ] returns a basis of the gauge invariant polynomials in the F-and R-symbols of the ring.";

Options[GaugeInvariants] =
  Options[GaugeSplitBasis];

GaugeInvariants[ ring_, opts:OptionsPattern[] ] :=
  First @ GaugeSplitBasis[ ring, opts ];


(* Returns rows corresponding to the invariants  *)
(*interestingInvariants[ ring_ ] :=*)
