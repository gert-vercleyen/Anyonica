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
      procID  = ToString @ Unique[];
      demands = OptionValue["GaugeDemands"];
      excludedVars = OptionValue["ExcludedVariables"];
      g = First @ symmetries["Symbols"];
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
              Abs[ Abs[Last[#]] - 1 ]&, (* Want power as close to 1 as possible *)
              1
            ][[1,1]]
          ]
        ];

      SimplestVar[ g[i_] ]:= g[i];

      FixValue[ a_ -> b_ ] :=
        With[{ var = SimplestVar @ b },
          Quiet[
            Solve[ b == 1,var ][[1]] //
            ReplaceAll[ HoldPattern[ Times[ _?NumericQ, x__ ] ] :> Times[x] ]
            ,
            Solve::nongen (* Sometimes throws errors because square roots appear *)
          ]
        ];

      UpdateSystem[ {}, fixedVars_ ] :=
        fixedVars;

      UpdateSystem[ transforms_, fixedVars_ ] :=
        UpdateSystem[
          (* Substitute new fixed gauge variable, then remove all transforms without gauge variables *)
          transforms //
          ReplaceAll[ FixValue[ First @ transforms ] ] //
          PowerExpand //
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
        ( unfixedVars = Complement[ demands[[;;,1]], result[[2,;;,1]] ] ) =!= {},
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
    Module[
      {
      s, gaugeVars, restrictingBinomials, newEqns, newVars, revertVars, CSpace,
      params, newTransforms, reParametrization, transforms, symbols, procID, result, time
      },
      transforms = sym["Transforms"];
      symbols    = sym["Symbols"];
      procID     = ToString @ Unique[];

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
              GetVariables[ transforms, symbols ],
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

checkArgsGaugeSymmetryEquivalentQ[ symmetries_ ] := 
  If[ 
    !MultiplicativeSymmetriesQ @ symmetries,
    Message[ GaugeSymmetryEquivalentQ::symnotmultiplicative, symmetries ];
    Abort[]
  ];

GaugeSymmetryEquivalentQ[ symmetries_Association, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Module[ { gaugeMatrix, zeroPos1, zeroPos2 },
    
    checkArgsGaugeSymmetryEquivalentQ @ symmetries;

    If[ (* Gauges are trivial *)
      Times @@ Dimensions[ MultiplicativeGaugeMatrix @ symmetries ] === 0,
      Return @ TrivialGaugeSymmetryEquivalentQ[ opts ][ sol1, sol2 ]
    ];

    (* Check whether zeros are at same positions *)
    { zeroPos1, zeroPos2 } =
      Position[ _ -> 0 ] /@
      { sol1, sol2 };

    If[ zeroPos1 =!= zeroPos2, Return @ False ];

    gaugeMatrix =
      MultiplicativeGaugeMatrix @
      MapAt[
        Delete[ zeroPos1 ],
        symmetries,
        {1}
      ];

    GaugeSymmetryEquivalentQ[ HermiteDecomposition @ gaugeMatrix, opts ][ sol1, sol2 ]

  ];

Options[TrivialGaugeSymmetryEquivalentQ] :=
  Options[GaugeSymmetryEquivalentQ];

TrivialGaugeSymmetryEquivalentQ[ opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  With[{
    differentQ =
      If[
        OptionValue["Numeric"],
        Not[ TrueQ[ InfN[ #1 - #2, OptionValue["Accuracy"] ] == 0 ] ]&,
        Not[ TrueQ[ OptionValue["SimplifyBy"][ #1 - #2 ] == 0 ] ]&
      ]
    },
    Catch[ (* Throw False immediately when 2 different values are encountered *)
      MapThread[
        If[ differentQ[ #1, #2 ], Throw @ False ]&, 
        { sol1, sol2 }
      ];
      True
    ]
  ];

GaugeSymmetryEquivalentQ[ gaugeMatrix_?MatrixQ, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  GaugeSymmetryEquivalentQ[ HermiteDecomposition @ gaugeMatrix, opts ][ sol1, sol2 ];

GaugeSymmetryEquivalentQ[ { r_?MatrixQ, h_?MatrixQ }, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
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
          DeleteCases[0] @ 
          Values @ 
          sol1;
        values2  =
          DeleteCases[0] @ 
          Values @ 
          sol2;

        differentAbsQ =
          If[
            numericQ,
            TrueQ[ Abs[ InfN[ #1, sd ] ] - Abs[ InfN[ #2, sd ] ] != 0 ]&,
            TrueQ[ simplify[ Abs[ #1 ] - Abs[ #2 ] ] != 0 ]&
          ];

        Catch[
          If[ (* Gauge invariants do not coincide up to 16 digits (Infinite precision) *)
            invariants =!= {} && 
            InfN[ invariants/.Dispatch[sol1], 16 ] != 
            InfN[ invariants/.Dispatch[sol2], 16 ]
            ,
            Throw @ False 
          ];

          If[ (* test for Unitary equivalence *)
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
            Count[ h, row_ /; MatchQ[ row, { 0 .. } ] ];

          divRHS = 
              If[ numericQ, N[ #, { Infinity, sd } ]&, Identity ] @
              (values1/values2);

          expRHS =
            simplify @
            PowerDot[ divRHS, r ];

          NonOneCoeff[ l_ ] :=
            FirstCase[ l, x_ /; !TrueQ[ preEqCheck[x] == 1 ] ];

          If[
            rank < Length[ expRHS ] &&
            !MissingQ[ noc = NonOneCoeff @ expRHS[[rank+1;;]] ],
            printlog["GSEQ:nonone_coeff", { procID, expRHS, rank, noc } ];
            False,
            True
          ]
        ]
      ];

    printlog["Gen:results", { procID, result, time } ];

    result

  ];


PackageExport["GSEQ"]
GSEQ::usage =
  "GSEQ is shorthand for GaugeSymmetryEquivalentQ.";

GSEQ = GaugeSymmetryEquivalentQ;

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
          PermuteFSymbols[ sol1, auto ],
          sol2
        ],
        Throw[True]
      ],
      { auto, FusionRingAutomorphisms[ring] }
    ];
    False
  ];
  


SymmetryEquivalentQ[ ring_FusionRing, gaugeMat_?MatrixQ, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  SymmeteryEquivalentQ[ ring, HermiteDecomposition @ gaugeMat, opts ][ sol1, sol2 ];

SymmetryEquivalentQ[ ring_FusionRing, { r_?MatrixQ, h_?MatrixQ }, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Catch[
    Do[
      If[
        GaugeSymmetryEquivalentQ[ { r, h }, opts ][ PermuteFSymbols[ sol1, auto ], sol2 ],
        Throw[True]
      ],
      { auto, FusionRingAutomorphisms[ring] }
    ];
    False
  ];

PackageScope["PermuteSymbols"]

PermuteSymbols[ Symb_, perm_List ] :=
  Sort @
  MapAt[
    ReplaceAll[ i_Integer :> perm[[i]] ],
    Symb,
    { All, 1 }
  ];

PackageExport["SEQ"]

SEQ::usage =
  "Shorthand for SymmetryEquivalentQ.";

SEQ =
  SymmetryEquivalentQ;


PackageExport["DeleteEquivalentSolutions"]

DeleteEquivalentSolutions::usage =
  "DeleteEquivalentSolutions[ soln, ring ] returns a list of representatives of " <>
  "equivalence classes of the solutions to the pentagon equations (possibly combined"<>
  " with those of the hexagon and/or pivotal equations).";

DeleteEquivalentSolutions::wrongrstructure =
  "Either all solutions should be braided or none should be braided.";

DeleteEquivalentSolutions::differentzeros =
  "Some solutions have zeros at different positions. These solutions can never be equivalent.\n"<>
  "Gather these solutions via GroupBy, or GatheBy and evaluate DeleteEquivalentSolutions on each set.";

Options[DeleteEquivalentSolutions] :=
  Union[
    Options[GaugeSymmetryEquivalentQ],
    {
      "PreEqualCheck" -> Identity,
      "Numeric" -> False,
      "Accuracy" -> 64
    }
  ];

DeleteEquivalentSolutions[ soln_List, ring_FusionRing, opts:OptionsPattern[] ] :=
  Module[{ zeroPositions, procID, result, time, invariants, zeroFs, braidedCheck, check, orbits,
    symbols },

    If[ Length @ soln === 0, Return @ soln ];
    (*procID =
      ToString @ Unique[];

    printlog[ "DSES:init", { procID, soln, ring, symmetries, { opts } } ];*)

    check =
      If[
        OptionValue["Numeric"],
        InfN[OptionValue["Accuracy"]],
        OptionValue["PreEqualCheck"]
      ];

    zeroPositions = Position[ _ -> 0 ] /@ soln;

    If[ !MatchQ[ zeroPositions, { x_ .. } ], Message[ DeleteEquivalentSolutions::differentzeros ]; Abort[]  ];

    { time, result } =
      AbsoluteTiming[

        zeroFs = Keys @ Cases[ First @ soln, HoldPattern[ _ -> 0 ] ];

        (*printlog[ "DSES:groups", { procID, groupedSoln } ];*)

        braidedCheck = Length @* FilterRRules /@ soln;

        If[
          Not[ Equal @@ braidedCheck ],
          Message[ DeleteEquivalentSolutions::wrongrstructure ]; Abort[]
        ];

        symbols = 
          Switch[ { FilterRRules[soln] =!= {}, FilterPRules[soln] =!= {} },
            { True, True }, { "FSymbols", "RSymbols", "PSymbols" },
            { True, False }, { "FSymbols", "RSymbols" },
            { False, True }, { "FSymbols", "PSymbols" },
            { False, False }, { "FSymbols" }
          ];

        invariants = GaugeInvariants[ ring, "Zeros" -> zeroFs, "IncludeOnly" -> symbols ];

        (* For each solution we will map its index to the evaluated 
           gauge-invariants over all automorphic solutions *)
        orbits = 
          Association @
          Table[
            i -> 
              check[ 
                invariants/.Dispatch[ PermuteSymbols[ soln[[i]], # ]& /@ FRA @ ring ]
              ],
            { i, Length @ soln }
          ];

        soln[[ 
          DeleteDuplicates[ 
            Range @ Length @ orbits, 
            intersectingQ[ orbits[#1], orbits[#2], check ]& 
          ] 
        ]]

      ];

    (*printlog["Gen:results", { procID, result, time } ];*)
    result
  ];

intersectingQ[ l1_List, l2_List, check_ ] :=
  Catch[
    Do[
      If[ TrueQ[ check[l1[[i]]] == check[l2[[j]]] ], Throw @ True ],
      { i, Length @ l1 }, { j, Length @ l2 }
    ]; False
  ];

Options[DeleteGaugeEquivalentSolutions] := 
  {
    "Accuracy" -> 64, 
    "Numeric" -> False, 
    "PreEqualCheck" -> Identity, 
    "SimplifyBy" -> Identity 
  }
    
DeleteGaugeEquivalentSolutions[ soln_, invariants_, opts:OptionsPattern[] ] :=
  Module[{ inv, indices },
    inv =
      If[
        OptionValue["Numeric"],
        N[ #, { Infinity, OptionValue["Accuracy"] } ]&,
        OptionValue["PreEqualCheck"] @* OptionValue["SimplifyBy"]
      ] @
      ReplaceAll[ invariants, Dispatch[soln] ];


    indices =
      Range @ Length @ soln;

    soln[[ DeleteDuplicatesBy[ indices, inv[[#]]& ] ]]
  ];

PackageExport["DES"]

DES::usage =
  "Shorthand for DeleteEquivalentSolutions.";

DES =
  DeleteEquivalentSolutions;




PackageExport["SymmetryEquivalentQ2"]

SymmetryEquivalentQ2::usage =
  "Experimental version of SymmetryEquivalentQ";

Options[SymmetryEquivalentQ2] :=
  Options[DeleteGaugeEquivalentSolutions2];

SymmetryEquivalentQ2[ ring_, invariants_, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Catch[
    Do[
      If[
        AddOptions[opts][GaugeSymmetryEquivalentQ2][invariants] @@  { sol1, PermuteSymbols[ sol2, auto ] },
        Throw[True]
      ],
      { auto, FusionRingAutomorphisms[ring] }
    ];
    False
  ];

Options[GaugeSymmetryEquivalentQ2] :=
  Options[SymmetryEquivalentQ2];

GaugeSymmetryEquivalentQ2[ invariants_, opts:OptionsPattern[] ][ sol1_, sol2_ ] :=
  Equal @@
  (
    If[
      OptionValue["Numeric"],
      N[ #, { Infinity, OptionValue["Accuracy"] } ]&,
      OptionValue["PreEqualCheck"] @* OptionValue["SimplifyBy"]
    ] @
    ReplaceAll[ invariants, Dispatch[ { sol1, sol2 } ] ]
  );


PackageScope["MultiplicativeGaugeMatrix"]

MultiplicativeGaugeMatrix[ sym_Association ] :=
  Module[{ t, var, factors, g, newVars },
    t = sym["Transforms"];
    var = sym["Symbols"];

    { factors, newVars } =
      Most @ SimplifyVariables[ Values[t]/Keys[t], GetVariables[ t, var ], g ];

    GaugeMatRow[ #, Length @ newVars, g ]& /@ factors
  ];


GaugeMatRow[ 1, n_, x_ ] :=
  ConstantArray[ 0, n ];

GaugeMatRow[ factor_, n_, x_ ] :=
  Normal @
  SparseArray[
    Flatten[ Rest[FactorList[factor]]/.{ x[i_], j_ }:> { {i} -> j } ]
    ,
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
  " with indices ind.\n"<>
  "GaugeTransform[ring,g][ s[ ind ] ] applies a gauge transformation with parameters labeled by g to the symbol s" <>
  " with indices ind. This form of the function can also be applied to pivotal symbols.";

(*Possible values of s are: \[ScriptCapitalF], \[ScriptCapitalR] (not implemented yet), etc.";*)

GaugeTransform[ g_Symbol ][ F[ a_, b_, c_, d_, e_, f_ ] ] :=
  ( g[ a, b, e ] g[ e, c, d ] )/( g[ a, f, d ] g[ b, c, f ] ) * F[ a, b, c, d, e, f ];

GaugeTransform[ g_Symbol ][ R[ a_, b_, c_ ] ] :=
  ( g[ a, b, c ] / g[ b, a, c ] )* R[a,b,c];

GaugeTransform[ ring_, g_Symbol ][ \[ScriptP][ a_ ] ] :=
  With[ { d = CC[ring] },
    ( g[ d[a], a, 1 ] g[ 1, d[a], d[a] ] )/( g[ d[a], 1, d[a] ] g[ a, d[a], 1 ] ) \[ScriptP][a]
  ];

GaugeTransform[ ring_, g_Symbol ][ F[i__] ] :=
  GaugeTransform[ g ] @ F[i];

GaugeTransform[ ring_, g_Symbol ][ R[i__] ] :=
  GaugeTransform[ g ] @ R[i];

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
  "GaugeSymmetries[ symbols, s ] returns the gauge transforms of the symbols, with gauge variables labeled by s.\n"<>
  "GaugeSymmetries[ ring, symbols, s ] returns the gauge transforms of the symbols, with gauge variables labeled by s."<>
  "This version also works for pivotal symbols.";

GaugeSymmetries[ symbols_, g_ ] :=
  <|
    "Transforms" -> Table[ s -> GaugeTransform[g][s], { s, symbols } ],
    "Symbols" -> { g }
  |>;

GaugeSymmetries[ ring_, symbols_, g_ ] :=
  <|
    "Transforms" -> Table[ s -> GaugeTransform[ring,g][s], { s, symbols } ],
    "Symbols" -> { g }
  |>;

PackageExport["GS"]

GS::usage =
  "Shorthand for GaugeSymmetries.";

GS = GaugeSymmetries;


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
  "GaugeSplitTransform[ ring ] returns { V, n } where V is a matrix and n an integer such that"<>
  " the first n rows of V provide a basis transform to obtain the gauge independent polynomials in the"<>
  " F-symbols, P-symbols, and R-symbols (in that order) via the use of the PowerDot function.";

GaugeSplitTransform::invalidoptionincludeonly =
  "The option for \"IncludeOnly\", `1`, must be a list containing at least \"FSymbols\", \"RSymbols\", and/or \"PSymbols\".";

Options[GaugeSplitTransform] :=
  {
    "IncludeOnly" -> {"FSymbols","PSymbols","RSymbols"},
    "Zeros" -> {}
  };

GaugeSplitTransform[ ring_, opts:OptionsPattern[] ] :=
  With[{ io = OptionValue["IncludeOnly"] },

    If[ 
      Head[io] =!= List || io === {} ||
      !SubsetQ[ { "FSymbols", "RSymbols", "PSymbols" },  io ]
      ,
      Message[ GaugeSplitTransform::invalidoptionincludeonly, io ];
      Abort[]
    ];

    If[ 
      Rank[ring] === 1, 
      Return @ 
      { IdentityMatrix @ Length @ io, Length @ io } 
    ];

    Module[{ symbols, g, sym, m, monomial, powers, d, v, r, sortf, zeros, zeroPos, symbolsWithZeros, getSymbols },
      zeros = OptionValue["Zeros"];
      If[ MatchQ[ zeros, { _Rule .. } ], zeros = Keys @ zeros ];

      getSymbols = 
        Comap[ 
          { 
            If[ MemberQ["FSymbols"] @ io, FSymbols, Splice[{}] ], 
            If[ MemberQ["PSymbols"] @ io, PSymbols, Splice[{}] ],
            If[ MemberQ["RSymbols"] @ io, RSymbols, Splice[{}] ] 
          }
        ]; 

      symbolsWithZeros =  Join @@ getSymbols @ ring;

      If[ (* Trivial Ring is always an annoying case *)
        Rank @ ring === 1, 
        Return @ { IdentityMatrix @ Length @ symbolsWithZeros, 3 } 
      ];

      zeroPos = 
        Flatten @ 
        Position[ symbolsWithZeros, x_ /; MemberQ[x] @ zeros, 1, Heads -> False ];

      symbols = Complement[ symbolsWithZeros, zeros ];

      sym = GaugeSymmetries[ ring, symbols, g ];

      monomial =
        PowerExpand[
          PowerDot[ symbols, Array[ m, Length @ symbols ] ] //
          ReplaceAll[ Dispatch @ sym["Transforms"] ] //
          ReplaceAll[ Dispatch @ Thread[symbols -> 1] ]
        ];

      powers =
        Expand @
        Cases[ monomial, Power[ g[__], p_. ] :> p ];

      d = (* It's faster to do a Hermite decomp first and throw away rows of zeros *) 
				DeleteCases[
					Last @ 
					HermiteDecomposition[
						powerToRow[ m, Length @ symbols ] /@ powers
					], 
					{ 0 .. }
				];

      { d, v } = Rest @ SmithDecomposition @ d;

      r = Length @ DeleteCases[0] @ Diagonal[d];

      (* First sort criterium: number of factors, where powers are counted as multiple factors.
         Second criterium: canonical lexicographic order on the indices (we need Reverse because the greater the row,
         the smaller the labels *)

      sortf =
        Order[
          Prepend[ Total @ Abs @ #1 ] @ Reverse[#1],
          Prepend[ Total @ Abs @ #2 ] @ Reverse[#2]
        ]&;

      {
        If[ zeroPos === {}, Identity, AddZerosToTransform[ zeroPos ] ] @
        Transpose @ 
        Join[
          Sort[ Transpose[v][[r+1;;]], sortf ],
          Sort[ Transpose[v][[;;r]], sortf ]
        ]
        ,
        Length[v] - r + Length[zeroPos]
      }
    ]
  ];

AddZerosToTransform[ {} ][ tMat_ ] := tMat;

AddZerosToTransform[ zeroPos_List][ tMat_?MatrixQ ] := 
  Module[ { nZeros, newDim, nonZeroPos, mat }, 
    nZeros = Length @ zeroPos;

    newDim = nZeros + First @ Dimensions @ tMat;
		
		nonZeroPos = Complement[ Range @ newDim, zeroPos ]; 
		
		mat = ConstantArray[ 0, { newDim, newDim } ];
		
		mat[[zeroPos]] = PadRight[ #, newDim ]& /@ IdentityMatrix[nZeros];
		mat[[nonZeroPos]] = PadLeft[ #, newDim ]& /@ tMat;
		
		mat
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
  Module[ { V, n, symbols, io, zeros },
    zeros = OptionValue["Zeros"];
    If[ MatchQ[ zeros, { _Rule ..  } ], zeros = Keys @ zeros ];

    io = OptionValue["IncludeOnly"];

    { V, n } = GaugeSplitTransform[ ring, opts ];

    getSymbols = 
      Comap[ 
        { 
          If[ MemberQ["FSymbols"] @ io, FSymbols, Splice[{}] ], 
          If[ MemberQ["PSymbols"] @ io, PSymbols, Splice[{}] ], 
          If[ MemberQ["RSymbols"] @ io, RSymbols, Splice[{}] ] 
        }
      ]; 

    symbols = Join @@ getSymbols @ ring;

    If[ Rank @ ring === 1, Return @ {symbols,{}} ];

    Map[
      PowerDot[ symbols, Transpose[#] ]&,
      {
        V[[;;,;;n]],
        V[[;;,n+1;;]]
      }
    ]
  ];

PackageExport["GaugeInvariants"]

GaugeInvariants::usage =
  "GaugeInvariants[ ring ] returns a basis of the gauge invariant Laurent polynomials in the F-and R-symbols of the fusion ring ring.\n"<>
  "GaugeInvariants[ cat ] returns a basis of the gauge invariant Laurent polynomials in the F-and R-symbols with their values of the fusion category cat.";

Options[GaugeInvariants] :=
  Options[GaugeSplitBasis];

GaugeInvariants[ ring_, opts:OptionsPattern[] ] :=
  First @ GaugeSplitBasis[ ring, opts ];





(*
+---------------------------------------------------------------------------+
|                            ADJUSTING THE GAUGE                            |
+---------------------------------------------------------------------------+
*)

PackageExport["UnitaryGaugeQ"]

UnitaryGaugeQ::usage =
"UnitaryGaugeQ[ring,fSymbols] returns True if the F-symbols fSymbols have unitary F-matrices.";
(*Here ring is the fusion ring for which the F-symbols are a solution to the pentagon equations. Options are (a) \"Numeric\": set to true to do a numerical check with a standard tolerance of 64, (b) \"Tolerance\" to set the Tolerance used for the internal call to UnitaryMatrixQ (see the documentation centre for more info), (c) \"SimplifyBy\": a function applied to symbolic matrices during the checks for unitarity. This option only works if the \"Numeric\" option is False";*)

UnitaryGaugeQ::notmultfree =
"Function does not yet support rings with multiplicity.";

UnitaryGaugeQ::wrongsolformat =
"`1` should be a proper solution to the pentagon equations.";

Options[UnitaryGaugeQ] :=
Options[unitaryMatrixQ];

UnitaryGaugeQ[ ring_, fSymbols_, opts:OptionsPattern[] ] :=
Which[
  Mult[ ring ] != 1
  ,
  Message[ UnitaryGaugeQ::notmultfree ];
  Abort[]
  ,
  True
  ,
  With[ { fMats = FMatrices[ring]/.Dispatch[fSymbols] },
    Catch[
      Do[
        If[ !AddOptions[opts][unitaryMatrixQ][mat], Throw @ False ],
        { mat, fMats }
      ];
      True
    ]
  ]
];

Options[unitaryMatrixQ] :=
{
  "SimplifyBy" -> Identity,
  "Numeric" -> False,
  "Accuracy" -> 64
};

unitaryMatrixQ[ mat_?MatrixQ, opts:OptionsPattern[] ] :=
With[{
  simplify = OptionValue["SimplifyBy"],
  numQ = OptionValue["Numeric"],
  acc = OptionValue["Accuracy"],
  n = Length[mat],
  idMat = mat.ConjugateTranspose[mat]
},
  If[
    numQ,
    Catch[
      Do[
        If[
          Not @TrueQ @ N[ idMat[[i,j]] == KroneckerDelta[ i, j ], { Infinity, acc } ],
          Throw @ False
        ],
        { i, n },
        { j, n }
      ];
      True
    ]
    ,
    Catch[
      Do[
        If[
          Not @ TrueQ[ simplify[ idMat[[i,j]] ] == KroneckerDelta[ i, j ] ],
          Throw @ False
        ],
        { i, n },
        { j, n }
      ];
      True
    ]
  ]
];

PackageExport["UGQ"]

UGQ::usage =
  "Shorthand for UnitaryGaugeQ.";

UGQ = UnitaryGaugeQ;

PackageExport["ToUnitaryGauge"]

Options[ToUnitaryGauge] :=
  {
    "SimplifyBy" -> Identity,
    "PreEqualCheck" -> Identity
  };

ToUnitaryGauge[ ring_FusionRing, FSol_, opts:OptionsPattern[] ] :=
  Module[ {
    solInvSol, FInv, symmetries, transforms, g, SimplestVar, varPowers,
    UnitaryValue, FixValue, UpdateSystem, time, result, gaugeVals, simplify, check,nextTransform
    },
    simplify = OptionValue["SimplifyBy"];
    check    = OptionValue["PreEqualCheck"];


    varPowers[monomial_] :=
      Cases[ monomial, Power[ g[i__], a_. ] :> { g[i], Abs[ a ] } ];

    SimplestVar[ monomial_ ] :=
      With[{ fList = varPowers @ monomial },
        If[ fList === {}, Return[ Missing[] ] ];
        MinimalBy[ fList, Last, 1 ][[1, 1]]
      ];

    UnitaryValue[ F[ abcd__, e_, f_ ] ] :=
      simplify @ ReplaceAll[ Sqrt[ F[ abcd, e, f ] FInv[ abcd, f, e ] ], solInvSol ];

    FixValue[ a_ -> b_ ] :=
      simplify @ Solve[ b == UnitaryValue @ a, SimplestVar @ b ][[1, 1]];

    nextTransform[ transf_ ] :=
      First @ MinimalBy[ transf, Min[ Abs[ varPowers[#[[2]]][[;;,2]] ] ]&, 1 ];

    UpdateSystem[ {}, vals_ ] := vals;

    UpdateSystem[ transf_, vals_ ] :=
      With[{ gaugeVal = FixValue[ nextTransform @ transf ] },
        UpdateSystem[
          DeleteCases[
            simplify[ Cancel @* Together /@ (transf /. gaugeVal ) ],
            rule_ /; CountVariables[ rule, g ] === 0
          ],
          Append[ vals /. gaugeVal, gaugeVal ]
        ]
      ];

    { time, result } =
    AbsoluteTiming[
      solInvSol =
      Dispatch @ Join[ FSol, InverseFSymbols[ ring, FInv, FSol ] ];

      symmetries =
      GaugeSymmetries[ FSymbols @ ring, g];

      transforms =
      MapAt[
        ReplaceAll[solInvSol],
        DeleteCases[ symmetries["Transforms"], rule_ /; CountVariables[ rule, g ] === 0 ],
        { All, 2 }
      ];

      gaugeVals =
      UpdateSystem[ transforms, {} ];

      ApplyGaugeTransform[ FSol, gaugeVals, g ]

    ];

    (*    If[ !UnitaryGaugeQ[ ring, result, opts ], printlog[ "TUG:sol_not_unitary", {procID} ] ];*)

    result
  ];


PackageExport["SymmetricGaugeQ"]

SymmetricGaugeQ::usage =
"SymmetricGaugeQ[ring,fSymb] returns True if the F-matrices corresponding to fSymb are symmetric.";

SymmetricGaugeQ::notmultfree =
"Function does not yet support rings with multiplicity.";

SymmetricGaugeQ::wrongsolformat =
"`1` should be a proper solution to the pentagon equations.";

Options[SymmetricGaugeQ] =
{
  "SimplifyBy" -> Identity,
  "Numeric" -> False,
  "Accuracy" -> 64
};

checkArgsSymmetricGaugeQ[ ring_, symb_ ] :=
Which[
  Mult[ring] != 1
  ,
  Message[ SymmetricGaugeQ::notmultfree ];
  Abort[]
  ,
  !PPSQ[symb]
  ,
  Message[ SymmetricGaugeQ::wrongsolformat, symb ];
  Abort[]
];

SymmetricGaugeQ[ ring_, symb_, OptionsPattern[] ] :=
(
  checkArgsSymmetricGaugeQ[ring, symb];
  With[
    {
      fMats = (FMatrices[ring] ~ WithMinimumDimension ~ 2) /. symb,
      simplify = OptionValue["SimplifyBy"],
      numQ = OptionValue["Numeric"],
      acc = OptionValue["Accuracy"]
    },
    With[
      {
        check =
        Evaluate @
        If[
          numQ,
          TrueQ[ N[ #, { Infinity, acc } ] == 0 ]&,
          TrueQ[ simplify[ # ] == 0 ]&
        ]
      },
      Catch[
        Do[
          Map[
            If[
              Not[ check[ # ] ],
              Throw[ False ]
            ]&,
            mat - Transpose[mat],
            { 2 }
          ]
          , { mat, fMats }
        ];
        True
      ]
    ]
  ]
);

PackageExport["SGQ"]

SGQ::usage =
"Shorthand for SymmetricGaugeQ.";

SGQ =
SymmetricGaugeQ;


PackageExport["ToSymmetricGauge"]

ToSymmetricGauge::usage =
"ToSymmetricGauge[ring,FSymb] tries to find a gauge for which the F-matrices are symmetric.";

ToSymmetricGauge::notmultfree =
"Function does not yet support rings with multiplicity.";

ToSymmetricGauge::wrongsolformat =
"`1` should be a proper solution to the pentagon equations.";

Options[ToSymmetricGauge] =
{
  "Numeric" -> False,
  "Accuracy" -> 64,
  "SimplifyBy" -> Identity,
  "ReturnGaugeTransform" -> False,
  "UseDatabaseOfSmithDecompositions" -> False,
  "PreEqualCheck" -> Identity,
  "UnitaryGaugeTransform" -> False,
  "GaugeDemands" -> {},
  "PreserveTrivialValues" -> True
};

CheckArgsToSymmetricGauge[ ring_, FSymb_ ] :=
Which[
  Mult[ring] != 1
  ,
  Message[ ToSymmetricGauge::notmultfree ];
  Abort[]
  ,
  !PPSQ[FSymb]
  ,
  Message[ ToSymmetricGauge::wrongsolformat, FSymb ];
  Abort[]
];

ToSymmetricGauge[ ring_, FSymb_, opts:OptionsPattern[] ] :=
(
  CheckArgsToSymmetricGauge[ ring, FSymb ];
  Module[
    { gaugeSymmetries, transforms, g, u, newFs, constraints, newVars, newConstraints, revertVars,
      unitaryGTQ, acc, numericQ, simplify, symGaugeQ, binomialMat, rhsVec, mU, mD, mV, rankBinomialMat, expRHS,
      NonOneCoeff,noc, diagonalElements,ZSpace,constVec, useDataBaseQ, preEqCheck, CheckSolution,newFSolution,
      time, result, procID, returnTransformQ, gaugeDemands, gauge, vacuumConstraints
    },
    acc = OptionValue["Accuracy"];
    numericQ = OptionValue["Numeric"];
    simplify = OptionValue["SimplifyBy"];
    useDataBaseQ = OptionValue["UseDatabaseOfSmithDecompositions"];
    preEqCheck =
      If[
        numericQ,
        InfN[acc],
        OptionValue["PreEqualCheck"]
      ];
    unitaryGTQ = OptionValue["UnitaryGaugeTransform"];
    returnTransformQ = OptionValue["ReturnGaugeTransform"];
    gaugeDemands = Equal @@@ OptionValue["GaugeDemands"];

    procID = ToString[Unique[]];

    printlog["TSG:init", { procID, ring, FSymb, {opts} } ];

    { time, result } = Normal @
    AbsoluteTiming[

      gaugeSymmetries =
        GaugeSymmetries[ FSymbols[ring], g ];

      transforms =
        gaugeSymmetries["Transforms"];

      vacuumConstraints =
        If[
          OptionValue["PreserveTrivialValues"],
          (* THEN *)
          Thread[
            ReplaceAll[
              Cases[ FSymbols[ring], $VacuumFPattern ]/.transforms,
              F[__] -> 1
            ] == 1
          ],
          (* ELSE *)
          {}
        ];

      newFs =
        MapAt[
          If[ numericQ, InfN[2*acc], simplify ],
          FSymb,
          { All, 2 }
        ];

      Catch[
        symGaugeQ[ symb_ ] :=
        AddOptions[opts][SymmetricGaugeQ][ ring, symb ];

        If[ (* Already in symmetric gauge *)
          symGaugeQ @ newFs,
          printlog["TSG:already_symmetric", {procID}];
          Throw @
          If[
            returnTransformQ,
            { FSymb, Thread[ GetVariables[ transforms, g ] -> 1 ] },
            FSymb
          ]
        ];

        (*
          Set up the constraints
        *)

        constraints =
          Join[
            vacuumConstraints,
            SymmetricGaugeConstraints[g][ring]/.Dispatch[newFs],
            gaugeDemands
          ];

        printlog[ "TSG:constraints", { procID, constraints } ];

        { newConstraints, newVars, revertVars } =
          SimplifyVariables[
            constraints,
            g @@@ NZSC[ring],
            u
          ];

        { binomialMat, rhsVec } =
          Catch[
            AddOptions[opts][BinToSemiLin][
              Rationalize[ newConstraints, 10^(-2*acc) ],
              newVars,
              u
            ],
            "Inconsistent"
          ];

        If[
          binomialMat === {} && rhsVec === {},
          printlog[ "TSG:off_diagonal_zero", {procID} ];
          Throw @
          If[
            returnTransformQ,
            { FSymb, $Failed },
            FSymb
          ]
        ];

        { mU, mD, mV } =
          If[
            useDataBaseQ,
            AddOptions[opts][MemoizedSmithDecomposition][ binomialMat ],
            SmithDecomposition @ binomialMat
          ];

        printlog["TSG:decomposition",{procID,{mU,mD,mV}}];

        rankBinomialMat =
          Length[
            diagonalElements = DeleteCases[0] @ Diagonal @ Normal @ mD
          ];

        expRHS =
          PowerDot[ rhsVec, mU ];

        NonOneCoeff[ l_ ] :=
          FirstCase[ l, x_ /; preEqCheck[x] != 1 ];

        If[
          (* RHS and LHS are incompatible *)
          rankBinomialMat < Length[ expRHS ] &&
          Head[ noc = NonOneCoeff  @ expRHS[[ rankBinomialMat + 1;; ]] ] =!= Missing,
          (* THEN *)
          printlog["TSG:nonone_coeff", { procID, expRHS, rankBinomialMat, preEqCheck @ noc } ];
          Throw[$Failed],
          (* ELSE *)
          ZSpace =
            mV[[ ;;, ;; rankBinomialMat ]] . DiagonalMatrix[ 1 / diagonalElements ];
          constVec =
            PowerDot[ rhsVec, ZSpace.mU[[;;rankBinomialMat]] ];
        ];

        CheckSolution[ sol_ ] :=
          If[
            Not @ AddOptions[opts][SymmetricGaugeQ][ ring, sol ],
            printlog["TSG:sol_not_symmetric", {procID} ];
          ];

        gauge =
          If[
            numericQ,
            Thread[ newVars -> InfN[ constVec, acc ] ],
            Thread[ newVars -> constVec ]
          ]/.revertVars;

        newFSolution =
          ApplyGaugeTransform[ FSymb, gauge, g ];

        CheckSolution[ newFSolution ];

        If[ (* Don't have further demands on gauges *)
          !unitaryGTQ,

          (* THEN *)
          Throw @
          If[
            returnTransformQ,
            { newFSolution, gauge },
            newFSolution
          ],

          (* ELSE *)
          If[ (* const vec is not vector of phases *)
            Not[ And @@ Thread[ preEqCheck[ simplify[ Abs[expRHS] ] ] == 1 ] ],
            printlog["TSG:non_unitary_transform", {procID} ]
          ];

          Throw @
          If[
            returnTransformQ,
            { newFSolution, gauge },
            newFSolution
          ];
        ];
      ] (* END CATCH *)

    ];

    printlog[ "Gen:results", { procID, result, time }];

    result/.g -> List

  ]
);


PackageExport["TSG"]

TSG::usage =
"Shorthand for ToSymmetricGauge.";

TSG =
ToSymmetricGauge;


(* Try to find a gauge where the F symbols are symmetric matrices *)
(* Putting solutions into a symmetric gauge *)
SymmetricGaugeConstraints[g_][ ring_FusionRing ] :=
With[
  {
    transformedFMats =
    ( mL[g][#].#.Inverse[mR[g][#]]& ) /@
    ( FMatrices[ring] ~ WithMinimumDimension ~ 2 )
  },
  DeleteCases[True] @
  Flatten @
  Table[
    MapThread[
      Equal,
      { mat, Transpose[mat] },
      2
    ],
    { mat, transformedFMats }
  ]
];


PackageExport["WhichGaugeTransform"]

WhichGaugeTransform::usage =
"WhichGaugeTransform[ ring, sol1, sol2, s ] returns a gauge transform, in the variable s, that transforms "<>
"sol1 into sol2";
(*  , where sol1 and sol2 are solutions to the pentagon and/or hexagon equations for the Fusion Ring ring. Here g is the variable that will be used as a gauge variable."*)

WhichGaugeTransform::wrongsolformat =
"`1` and `2` should be proper solutions to the pentagon equations.";

WhichGaugeTransform::notmultfree =
"Function does not yet support rings with multiplicity.";

Options[WhichGaugeTransform] :=
{
  "OnlyMatchAbsoluteValues" -> False,
  "Numeric" -> False,
  "Accuracy" -> 64,
  "UseDatabaseOfSmithDecompositions" -> False,
  "PreEqualCheck" -> Identity,
  "SimplifyBy" -> Identity
};

checkArgsWhichGaugeTransform[ ring_, sol1_, sol2_ ] := 
  Which[
    Mult[ring] != 1
    ,
    Message[ WhichGaugeTransform::notmultfree ];
    Abort[]
    ,
    !PPSQ[sol1] || !PPSQ[sol2]
    ,
    Message[ WhichGaugeTransform::wrongsolformat, sol1, sol2 ];
    Abort[]
  ];

WhichGaugeTransform[ ring_, sol1_, sol2_, g_, opts:OptionsPattern[] ] :=
  Module[
    { fs, gaugeSymmetries, transforms, u, constraints, newVars, newConstraints, revertVars,
      acc, numericQ, simplify, binomialMat, rhsVec, mU, mD, mV, rankBinomialMat, expRHS, listOfOnesQ, newSol1, newSol2,
      diagonalElements, ZSpace, constVec, useDataBaseQ, preEqCheck, nGaugeVars, nonZeroFs,  trivialSpace, CSpace,
      monomials, time, result, procID, onlyAbsQ, values1, values2, vars, t, z, normSquaredExtraVars, absSol, a, b
    },
    checkArgsWhichGaugeTransform[ ring, sol1, sol2 ];

    acc          = OptionValue["Accuracy"];
    numericQ     = OptionValue["Numeric"];
    simplify     = OptionValue["SimplifyBy"];
    useDataBaseQ = OptionValue["UseDatabaseOfSmithDecompositions"];
    preEqCheck   = OptionValue["PreEqualCheck"];
    onlyAbsQ     = OptionValue["OnlyMatchAbsoluteValues"];
    procID       = ToString[Unique[]];

    { newSol1, newSol2 } =
      If[ 
        numericQ,
        InfN[ { sol1, sol2 }, acc ],
        Map[ simplify, { sol1, sol2 }, { 2 } ]
      ];

    fs = FSymbols @ ring;

    { values1, values2 } = fs /. Dispatch @ { newSol1, newSol2 };

    nGaugeVars = NNZSC @ ring;

    (* printlog["TSG:init", { procID, ring, FSymb, {opts} } ]; *)

    { time, result } = Normal @
    AbsoluteTiming[

      nonZeroFs = 
        Keys @ DeleteCases[ _ -> 0 ] @ newSol1;

      gaugeSymmetries =
        GaugeSymmetries[ nonZeroFs, g ];

      transforms =
        gaugeSymmetries["Transforms"];

      Catch[
        If[ (* Solutions are the same *)
          TrivialGaugeSymmetryEquivalentQ[
            "Numeric" -> numericQ,
            "Accuracy" -> acc,
            "SimplifyBy" -> simplify
          ][ values1, values2 ],
          (* Then *)
          printlog["WGT:same_solution", { procID, sol1, sol2 } ];
          Throw @ Thread[ ( g @@@ NZSC[ring] ) -> 1  ]
        ];

        If[ (* Different zero values for F-symbols *)
          Unequal @@
          Map[ Position[#,0,1]&, { values1, values2 } ],
          (* THEN *)
          printlog["WGT:different_zero_positions", { procID, sol1, sol2 } ];
          Throw @ {},
          (* ELSE: remove 0 values and continue *)
          values1 =
            DeleteCases[0] @ values1;
          values2 =
            DeleteCases[0] @ values2;
        ];

        constraints = 
          DeleteCases[True] @
          Thread[
            values2 == 
            ReplaceAll[
              GaugeTransform[ If[ onlyAbsQ, { g, t }, g ] ][ nonZeroFs ],
              sol1
            ]
          ];

        If[
          MemberQ[False] @ constraints,
          Throw @ { }
        ];

        vars =
          GetVariables[ constraints, { g, t } ];

        { newConstraints, newVars, revertVars } =
          SimplifyVariables[ constraints, vars , u ];

        { binomialMat, rhsVec } = 
          AddOptions[opts][BinToSemiLin][
            newConstraints,
            newVars,
            u,
            "SimplifyIntermediateResultsBy" -> simplify
          ];

        { mU, mD, mV } = 
          If[
            useDataBaseQ,
            AddOptions[opts][MemoizedSmithDecomposition][ binomialMat ],
            SmithDecomposition @ binomialMat
          ];

        printlog["WGT:decomposition",{procID,{mU,mD,mV}}];

        rankBinomialMat =
          Length[
            diagonalElements = DeleteCases[0] @ Diagonal @ Normal @ mD
          ];

        expRHS =
          simplify /@
          PowerDot[ rhsVec, mU ];

        listOfOnesQ[ l_ ] :=
          And @@
          Thread[ preEqCheck[l] == 1 ];

        If[
          (* RHS and LHS are incompatible *)
          rankBinomialMat < Length[ expRHS ] &&
          Not[ TrueQ[ listOfOnesQ[ expRHS[[ rankBinomialMat + 1;; ]] ] ] ],

          (* THEN *)
          printlog["WGT:nonone_coeff", { procID, expRHS, rankBinomialMat } ];
          Throw @ { },

          (* ELSE *)
          ZSpace =
            mV[[ ;;, ;; rankBinomialMat ]] . DiagonalMatrix[ 1 / diagonalElements ];
          constVec =
            PowerDot[ rhsVec, ZSpace.mU[[;;rankBinomialMat]] ];
        ];

        trivialSpace =
          With[ { ntVars = Length[vars] - NNZSC[ring] },
            MatrixDirectSum[
              {
                TrivialGaugeMatrix @ nonZeroFs,
                If[
                  onlyAbsQ,
                  ConstantArray[ 0, { ntVars, ntVars } ],
                  {{}}
                ]
              }
            ]
          ];

        CSpace =
          IntegerOrthogonalSpace[ mV[[ ;;, rankBinomialMat+1;; ]], trivialSpace ];

        monomials =
          If[
            CSpace === {{}},
            ConstantArray[ 1, Length[constVec] ],
            With[ { parameters = z /@ Range[ Dimensions[CSpace][[2]] ] },
              PowerDot[ parameters, CSpace ]
            ]
          ];

        If[ (* Any solution goes *)
          !onlyAbsQ,

          (* THEN *)
          Throw[
            Thread[ newVars -> constVec ]/.revertVars
          ],

          (* ELSE *)
          (* Only want abs vals of solutions to be the same *)
          If[ (* All extra vars are already phases *)
            listOfOnesQ[
              simplify @* Abs /@ constVec[[ nGaugeVars + 1 ;;  ]]
            ],

            (* THEN *)
            Throw[ Thread[ newVars -> constVec ][[;;nGaugeVars]]/.revertVars ],

            (* ELSE: add monomials and try to find vals of monomials s.t. extra vars become phases *)

            If[ (* No freedom left *)
              CSpace === {{}},
              Throw @ {}
            ];

            (* Calculate squared norm of extra vars *)
            normSquaredExtraVars =
              ( ComplexExpand[ Norm /@ constVec ]^2 * ( monomials/. z[i_] :> a[i]^2 + b[i]^2 ) )[[ nGaugeVars + 1 ;; ]];

            (* Try to find solution with norm 1 *)
            absSol =
              FindInstance[
                Thread[ normSquaredExtraVars == 1 ],
                GetVariables[ normSquaredExtraVars, { a, b } ],
                Reals
              ];

            If[
              (* No solution found *)
              absSol === {},

              (* THEN *)
              Throw @ {},

              (* ELSE *)
              Throw[
                Thread[
                  newVars -> constVec * monomials/. z[i_] :> a[i] + I b[i] /. absSol /. revertVars
                ][[ ;; nGaugeVars ]]
              ]
            ]
          ]
        ];
      ]
    ];

    printlog[ "Gen:results", { procID, result, time }];

    result
  ];

PackageExport["WGT"]

WGT::usage =
  "Shorthand for WhichGaugeTransform.";

WGT = WhichGaugeTransform;
