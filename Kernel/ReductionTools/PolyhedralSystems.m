(* Mathematica Source File *)
(* Created by the Wolfram Language Plugin for IntelliJ, see http://wlplugin.halirutan.de/ *)
(* :Author: gertvercleyen *)
(* :Date: 2022-02-18 *)


Package["Anyonica`"]

(*
+---------------------------------------------------------------------------+
|                           SETTING UP EQUATIONS                            |
+---------------------------------------------------------------------------+
*)

(*
+---------------------------------------------------------------------------+
|                            PENTAGON EQUATIONS                             |
+---------------------------------------------------------------------------+
*)

PackageExport["PentagonEquations"]

PentagonEquations::usage =
  "PentagonEquations[ ring ] returns the pentagon equations related to ring.;"

(*   "all trivial and duplicate equations have been removed.";*)

Options[ PentagonEquations ] :=
  Options[ PentagonTower ];

PentagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
  If[
    Mult[ ring ] == 1
    ,
    PentagonEquationsWithoutMultiplicity[ ring, opts ]
    ,
    PentagonEquationsWithMultiplicity[ ring, opts ]
  ];

PackageExport["PE"]

PE::usage =
  "Shorthand for PentagonEquations.";

PE =
  PentagonEquations;

Options[PentagonEquationsWithoutMultiplicity] =
  {
    "TrivialVacuumSymbols" -> True,
    "Knowns" -> {}
  };

PentagonEquationsWithoutMultiplicity[ ring_, opts:OptionsPattern[] ] := 
  Block[ { a, b, c, d, e, f, g, l, k, r, rr, lFInd, zsc, nzsc, trivVacQ, knowns, matches1, matches2, matches3, eqn, sF },
    trivVacQ  = OptionValue["TrivialVacuumSymbols"];
    knowns    = OptionValue["Knowns"];
    r         = Rank[ring];
    If[ r == 1 && ( trivVacQ || knowns === { F[1,1,1,1,1,1] -> 1 } ) , Return @ {} ];
    rr        = Range @ r;
    lFInd     = List @@@ FSymbols[ring];
    nzsc      = NZSC @ ring;
    zsc       = Complement[ Tuples @ { rr, rr, rr }, nzsc ];

    sF =
      SparseArray @
      ReplaceAll[
        ReplaceAll[
          Normal[SparseFTensor[ring]],
          knowns
        ],
        If[ trivVacQ, Thread[ Cases[ FSymbols @ ring, $VacuumFPattern ] -> 1 ], {} ]
      ];

      Reap[
        (* Collect equations of the form Non0LHS == RHS *)
        Do[
          { f, c, d, e, g, l } = label;
          matches1 = Cases[ lFInd, { a_, b_, l, e, f, k_ } ];
          Do[
            { a, b, k } = label2[[ { 1, 2, 6 } ]];

            eqn =
            (
              sF[[f,c,d,e,g,l]] sF[[a,b,l,e,f,k]] ==
              Sum[ sF[[a,b,c,g,f,h]] sF[[a,h,d,e,g,k]] sF[[b,c,d,k,h,l]], {h,r} ]
            );

            If[ !TrueQ[eqn], Sow @ eqn ]
            ,
            { label2, matches1 }
          ],
          { label, lFInd }
        ];

        (* Collect equations of the form 0 == RHS. This is done 
           by constructing the symmetric tree with non-existent  
           bottom fusion channel N[f,l,e] and matching the other
           labels *)
        Do[
          { f, l, e } = n1;
          matches2 = Cases[ nzsc, { a_, b_, f } ];
          matches3 = Cases[ nzsc, { c_, d_, l } ];
          Do[
            { a, b } = Most @ n2;
            { c, d } = Most @ n3;
            eqn = 
              (
                0 == Sum[ sF[[a,b,c,g,f,h]] sF[[a,h,d,e,g,k]] sF[[b,c,d,k,h,l]], {h,r} ]
              );
            If[ !TrueQ[eqn], Sow @ eqn ]
            ,
            { k, r },
            { g, r },
            { n2, matches2 },
            { n3, matches3 }
          ]
          ,
          { n1, zsc }
        ];

      ][[2,1]]

  ];

(* (* Might be useful later*)
DimF[regMats_] :=
  Module[ { matToRules },
    matToRules[ mat_ ] :=
    With[{ n = Length[mat] },
      Flatten @ Map[ # -> n &, mat, {2} ]
    ];
    Association @@
    Flatten[
      matToRules /@ regMats
    ]
  ];
*)

(*
+---------------------------------------------------------------------------+
|                             Multiplicity Case                             |
+---------------------------------------------------------------------------+
*)

(* a     b     c                                a     b     c *)
(*  \   /     /                                  \     \   /  *)
(*   \ /     /                                    \     \ /   *)
(*    α     /                                      \     δ    *)
(*     \   /                                        \   /     *)
(*    e \ /      = Σ F[a,b,c,d,{e,α,β},{f,ɣ,δ}]      \ / f    *)
(*       β                                            ɣ       *)
(*       │                                            │       *)
(*       │                                            │       *)


Options[PentagonEquationsWithMultiplicity] =
{ "Knowns" -> {} };

PentagonEquationsWithMultiplicity[ ring_FusionRing?FusionRingQ, OptionsPattern[] ] :=
Module[
  { fgGroupedLabels, gGroupedLabels, mt, a, b, c, d, e, l, k, h, replaceKnowns,
    matchingLabels, dim, fLabels, gLabels, hLabels, kLabels, lLabels },
  mt =
  MT[ring];

  fgGroupedLabels =
  GroupBy[(* Group left 3-vertex trees by equal values of a,b,c,d,e *)
    LeftOrderedFusionTrees[ ring, 3 ],
    #[[{1,2,3,4,7}]]&
  ];

  gGroupedLabels =
  GroupBy[ (#[[5]]&) -> (#[[6]]&) ] /@
  fgGroupedLabels;

  (* Substitutes known values in F or R tensors *)
  replaceKnowns[ expr_ ] :=
  expr/.Dispatch[ OptionValue["Knowns"] ];

  (* Searches for intermediate fusion labels s where the two patterns are vertex labels, each containing 1 s *)
  matchingLabels[ pattern1_, pattern2_, s_ ] :=
  With[{
    non0Ns = NZSC[ring],
    n1 = Flatten @ FirstPosition[ pattern1, s ],
    n2 = Flatten @ FirstPosition[ pattern2, s ]
  },
    Flatten @
    Intersection[
      Cases[ non0Ns, ReplaceAll[ pattern1, s->Blank[] ] ][[;;,n1]],
      Cases[ non0Ns, ReplaceAll[ pattern2, s->Blank[] ] ][[;;,n2]]
    ]
  ];

  dim[ a_, b_, c_ ] :=
  mt[[a,b,c]];

  DeleteCases[True] @
  Flatten @
  Reap[
    Do[ (* Loop over valid a,b,c,d labels *)
      { a, b, c, d, e } =
      treeLabels;
      fLabels =
      DeleteDuplicates @
      fgGroupedLabels[ treeLabels ][[;;,5]];
      gLabels =
      gGroupedLabels[ treeLabels ];
      Do[
        lLabels =
        matchingLabels[ { f, l, e }, { c, d, l }, l ];

        kLabels[ l_ ] :=
        matchingLabels[ { a, k, e }, { b, l, k }, k ];

        hLabels[ k_ ] :=
        Intersection[
          matchingLabels[ { a, h, g }, { b, c, h }, h ],
          matchingLabels[ { b, c, h }, { h, d, k }, h ]
        ];

        Sow @
        replaceKnowns @
        Table[
          Sum[
            F[ f, c, d, e, { g, bet, gam }, { l, zet, eps } ] *
            F[ a, b, l, e, { f, alp, zet }, { k, the, eta } ]
            , { zet, dim[ f, l, e ] }
          ]
          ==
          Sum[
            F[ a, b, c, g, { f, alp, bet }, { h, kap, iot } ] *
            F[ a, h, d, e, { g, kap, gam }, { k, the, lam } ] *
            F[ b, c, d, k, { h, iot, lam }, { l, eta, eps } ]
            , { h, hLabels[ k ] }
            , { iot, dim[ b, c, h ] }
            , { kap, dim[ a, h, g ] }
            , { lam, dim[ h, d, k ] }
          ]
          , { l, lLabels }, { k, kLabels[ l ] }
          , { alp, dim[ a, b, f ] }, { bet, dim[ f, c, g ] }, { gam, dim[ g, d, e ] }
          , { eps, dim[ c, d, l ] }
          , { eta, dim[ b, l, k ] }, { the, dim[ a, k, e ] }
        ]
        , { f, fLabels }, { g, gLabels[f] }
      ]
      , { treeLabels, Keys[ fgGroupedLabels ]  }
    ]
  ][[2,1]]
];

(* TRIVIAL FS:
{
 F[ 1, b_, c_,
   d_, { e_, \[Alpha]_, \[Beta]_}, {f_ , \[Gamma]_, \[Delta]_}] :>
  delta[{\[Beta], \[Delta]}, {b, e}, {f, d}],
 F[ a_, 1 , c_,
   d_, { e_, \[Alpha]_, \[Beta]_}, {f_ , \[Gamma]_, \[Delta]_}] :>
  delta[{\[Beta], \[Gamma]}, {a, e}, {f, c}],
 F[ a_, b_, 1,
   d_, { e_, \[Alpha]_, \[Beta]_}, {f_ , \[Gamma]_, \[Delta]_}] :>
  delta[{\[Alpha], \[Gamma]}, {d, e}, {f, b}]
 }


*)

(*
+---------------------------------------------------------------------------+
|                             HEXAGON EQUATIONS                             |
+---------------------------------------------------------------------------+
*)

(*
+---------------------------------------------------------------------------+
|                               General Case                                |
+---------------------------------------------------------------------------+
*)
PackageExport["HexagonEquations"]

HexagonEquations::usage =
"HexagonEquations[ r ] returns the hexagon equations related to the fusion ring r.";
(*The \"Knowns\" can be set to a list of rules of variables that are already known, e.g. a solution to the pentagon equations.";*)
HexagonEquations::badformat =
"The argument given to \"Knowns\", `1`, should be a list of rules.";

Options[HexagonEquations] =
{
  "Knowns" -> {},
  "TrivialVacuumSymbols" -> True
};

HexagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
If[
  Mult[ring] === 1,
  HexagonEquationsWithoutMultiplicity[ ring, opts ],
  HexagonEquationsWithMultiplicity[ ring, opts ]
];

(*
+---------------------------------------------------------------------------+
|                          Multiplicity Free Case                           |
+---------------------------------------------------------------------------+
*)

Options[HexagonEquationsWithoutMultiplicity] :=
Options[HexagonEquations];

HexagonEquationsWithoutMultiplicity[ ring_, OptionsPattern[] ] :=
Module[{ a, b, c, d, e, g, sR, sF, rank, knowns, rSymbols,fSymbols, matchingLabels },
  rSymbols =
  R @@@ NZSC[ring];

  fSymbols =
  FSymbols[ring];

  rank =
  Rank[ring];

  If[
    !MatchQ[ OptionValue["Knowns"], { _Rule ... } ],
    Message[HexagonEquations::badformat, OptionValue["Knowns"] ]
  ];

  knowns =
  Dispatch @
  Join[
    If[
      TrueQ @ OptionValue["TrivialVacuumSymbols"],
      (* THEN *)
      Thread[
        Cases[ Join[ rSymbols, fSymbols ], $VacuumRPattern | $VacuumFPattern ] -> 1
      ],
      (* ELSE *)
      {}
    ],
    Normal @ OptionValue["Knowns"]
  ];

  (* construct a sparse array of R symbols *)
  sR =
  SparseArray[
    Thread[ NZSC[ring] -> rSymbols ]/.knowns,
    { rank, rank, rank }
  ];

  sF =
  SparseArray @
  ReplaceAll[
    Normal @ SparseFTensor[ ring ],
    knowns
  ];

  matchingLabels[ { a_, c_, b_, d_, e_} ] :=
  Cases[ List @@@ fSymbols, { c, a, b, d, e, _ } ][[;;,6]];

  DeleteDuplicates @
  DeleteCases[True] @
  Flatten[
    Reap[
      Do[ (* Loop over valid F-symbols and construct equations *)
        { a, c, b, d, e, g } = List @@ ff;
        Sow[
          {
            sR[[ c, a, e ]] sF[[a,c,b,d,e,g]] sR[[ c, b, g ]]
            ==
            Sum[
              sF[[ c, a, b, d, e, f ]] sR[[ c, f, d ]] sF[[ a, b, c, d, f, g ]],
              { f, matchingLabels[ { a, c, b, d, e } ] }
            ]
            ,
            sR[[ a, c, e ]]^(-1) sF[[a,c,b,d,e,g]] sR[[ b, c, g ]]^(-1)
            ==
            Sum[
              sF[[ c, a, b, d, e, f ]] sR[[ f, c, d ]]^(-1) sF[[ a, b, c, d, f, g ]],
              { f, matchingLabels[ { a, c, b, d, e } ] }
            ]
          }
        ],
        { ff, fSymbols }
      ]
    ][[2]]
  ]
];

(*
+---------------------------------------------------------------------------+
|                             Multiplicity Case                             |
+---------------------------------------------------------------------------+
*)

Options[ HexagonEquationsWithMultiplicity ] :=
Options[HexagonEquations];
HexagonEquationsWithMultiplicity[ ring_, OptionsPattern[] ] :=
Module[ {
  fGroupedLabels, replaceKnowns, matchingLabels, Rt, Ft,
  DS, TP, ID, a, b, c, d, e, g, eLabels, fLabels, gLabels, op,
  SwapOperator, swap, mt
},
  mt =
  MT[ring];

  fGroupedLabels =
  GroupBy[(* Group right 2-vertex trees by equal values of a,b,c,d *)
    RightOrderedFusionTrees[ ring, 2 ],
    #[[{1,2,3,5}]]&
  ];

  (* Substitutes known values in F or R tensors *)
  replaceKnowns[ tensors_ ] :=
  ( SparseArray[ Normal[#]/.Dispatch[ OptionValue["Knowns"] ] ] & ) /@
  tensors;

  (* Searches for intermediate fusion labels s where the two patterns are vertex labels, each containing 1 s *)
  matchingLabels[ pattern1_, pattern2_, s_ ] :=
  With[{
    non0Ns = NZSC[ring],
    n1 = Flatten @ FirstPosition[ pattern1, s ],
    n2 = Flatten @ FirstPosition[ pattern2, s ]
  },
    Flatten @
    Intersection[
      Cases[ non0Ns, ReplaceAll[ pattern1, s->Blank[] ] ][[;;,n1]],
      Cases[ non0Ns, ReplaceAll[ pattern2, s->Blank[] ] ][[;;,n2]]
    ]
  ];

  SwapOperator[ { a_, b_, e_ }, { e_, c_, d_ } ] :=
  ArrayFlatten[
    SparseArray[
      Flatten @
      Table[
        { i, j, j, i } -> 1,
        { i, mt[[a,b,e]] },
        { j, mt[[e,c,d]] }
      ]
    ]
  ];

  Rt =
  replaceKnowns @
  RTensors[ring];
  Ft =
  replaceKnowns @
  FTensors[ring];
  DS =
  MatrixDirectSum;
  TP =
  KroneckerProduct;
  ID =
  IdentityMatrix[ mt[[##]] ]&;

  DeleteCases[True] @
  Flatten @
  Reap[
    Do[ (* Loop over valid a,b,c,d labels *)
      { a, b, c, d } =
      treeLabels;
      eLabels =
      matchingLabels[ { a, b, e }, { e, c, d }, e ];
      fLabels =
      fGroupedLabels[ treeLabels ][[;;,4]];
      gLabels =
      matchingLabels[ { c, a, g }, { g, b, d }, g ];

      op[1,1] =
      DS @ Table[ TP[ Rt[ { c, a, g } ], ID[ g, b, d ] ], { g, gLabels } ];

      op[1,2] =
      Ft[ { a, c, b, d } ];

      op[1,3] =
      DS @ Table[ TP[ ID[ a, f, d ], Rt[ { c, b, f } ] ], { f, fLabels } ];

      op[2,1] =
      Ft[ { c, a, b, d } ];

      swap =
      DS @ Table[ SwapOperator[ { a, b, e }, { e, c, d } ], { e, eLabels } ];

      op[2,2] =
      DS @ Table[ TP[ ID[ a, b, e ], Rt[ { c, e, d } ] ], { e, eLabels } ];

      (* DS @ Table[ ID[ a, b, e ] ~ TP ~ Rt[ { c, e, d } ], { e, eLabels } ]; *)
      op[2,3] =
      Ft[ { a, b, c, d } ];

      Sow @
      {
        ThreadMatrixEquality[
          op[1,1].op[1,2].op[1,3] == op[2,1].swap.op[2,2].op[2,3]
        ],
        ThreadMatrixEquality[
          Inverse[op[1,1]].op[1,2].Inverse[op[1,3]] == op[2,1].swap.Inverse[op[2,2]].op[2,3]
        ]
      };
      , { treeLabels, Keys[ fGroupedLabels ]  }
    ]
  ][[2,1]]

];

(*
+---------------------------------------------------------------------------+
|                           TWIST FACTOR EQUATIONS                          |
+---------------------------------------------------------------------------+
*)

PackageExport["TwistFactorEquations"]

TwistFactorEquations::usage =
"TwistFactorEquations[ring,twistFactors] returns the equations that relate twist factors to R-symbols for a" <>
"ribbon fusion category with ring as Grothendieck ring.";

TwistFactorEquations[ ring_FusionRing, twistFactors_ ] :=
Module[
  { t, d, a, b, c, rs },
  t =
  Exp[ 2 Pi I twistFactors ][[#]]&;
  d =
  QD[ring][[#]]&;
  rs =
  RSymbols[ring];

  TEL[
    Join[
      Table[
        { a, b, c } =
        List @@ r;
        R[ a, b, c ] * R[ b, a, c ] == t[c] / ( t[a] t[b] ),
        { r, rs }
      ]
      ,
      Table[
        Sum[ ( d[c] / d[a] ) R[ a, a, c ], { c, Cases[ rs, R[ a, a, _ ] ][[;;,3]] } ] == t[a],
        { a, Rank[ring] }
      ]
    ]/.$VacuumRPattern -> 1
  ]

];


(*
+---------------------------------------------------------------------------+
|                 PREPARING INPUT FOR POLYHEDRON SOLVERS                    |
+---------------------------------------------------------------------------+
*)

PackageScope["PreparePentagonSolverInput"]

PreparePentagonSolverInput::usage =
"Finds admissible sets of 0 values, gauge fixes the F-symbols, and then returns a list of associations containing all "<>
"necessary info to compute solutions to the pentagon equations for these systems.";

PreparePentagonSolverInput::notsubring =
"`1` is not isomorphic to any subring of `2`.";

PreparePentagonSolverInput::notyetsupported = 
  "Only the option \"Method\" -> \"HermiteDecomposition\" is supported for solving pentagon equations.\n"<>
  "Proceeding with option value equal to \"HermiteDecomposition\"";

Options[PreparePentagonSolverInput] :=
Union[
  {
    "GaugeDemands" -> {},
    "ZeroValues" -> None,
    "NonSingular" -> False,
    "PreEqualCheck" -> Identity,
    "UseDatabaseOfSmithDecompositions" -> True,
    "UseDatabaseOfZeroValues" -> True,
    "StoreDecompositions" -> True,
    "InjectSolution" -> {},
    "FindZerosUsingSums" -> True
  },
  Options[FindZeroValues],
  Options[ReduceBinomialSystem]
];

PreparePentagonSolverInput[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
Module[
  {
    binEqns, gaugeSymmetries, pentEqns, sumEqns, invMats, extraFixedFs, zeros, fSymbols, tower,
    unionZeros, sharedVars, remainingSym, specificSym, specificFixedFs,
    g, dz, solutions, time, procID, gaugeDemands, zeroValues, nonSingularQ, preEqCheck, useDBQ, storeDecompQ,
    subsSol, inject, compatibleSol,sRing, sSol, allFSymbols, vacuumSymbols, useSumsQ, newBinEqns, newSumEqns,
    neverZeroBinEqns
  },
  gaugeDemands =
    OptionValue["GaugeDemands"];
  zeroValues =
    OptionValue["ZeroValues"];
  nonSingularQ =
    OptionValue["NonSingular"];
  useSumsQ =
    OptionValue["FindZerosUsingSums"];
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
      { sRing, sSol } = subsSol;

      (* Function that maps labels in sRing to corresponding labels in ring *)
      inject =
        InjectionForm[ ring, sRing ];

      If[
        inject === None,
        Message[PreparePentagonSolverInput::notsubring, sRing, ring ];
        Abort[]
      ];

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

    allFSymbols = FSymbols @ ring;

    vacuumSymbols = Cases[ allFSymbols, $VacuumFPattern ];

    fSymbols =
      Complement[
        allFSymbols,
        vacuumSymbols,
        GetVariables[ compatibleSol, F ]
      ];

    tower =
      PentagonTower[ ring, "Knowns" -> compatibleSol ];

    { binEqns, sumEqns } =
      BinSumEquationsFromTower[ tower ];

    pentEqns = 
      Join[ binEqns, sumEqns ];

    (* For the inverse matrices we add the condition that removing zigzags is an isomorphism *)
    invMats =
      With[ { d = CC[ring] },
        Join[
          FMatrices[ ring ],
          Array[ {{F[ #, d[#], #, #, 1, 1 ]}}&, Rank[ring] ] ]
      ];

    gaugeSymmetries = GaugeSymmetries[ allFSymbols, g ];

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
        DeleteCases[ {{ _?NumericQ }} | trivialMatrixPattern  ]
      ];

    printlog[ "PPSI:original_system", { procID, pentEqns, fSymbols } ];

    printlog[ "PPSI:zero_Fs", { procID } ];

    (* Find Configurations of non-trivial 0-values *)
    zeros =
      Dispatch @
      Which[
        OptionValue["NonSingular"] || GroupRingQ[ring]
        ,
        {{}}
        ,
        OptionValue["ZeroValues"] =!= None
        ,
        OptionValue["ZeroValues"]
        ,
        OptionValue["UseDatabaseOfZeroValues"]
        ,
        AddOptions[opts][MemoizedZeroValues][
          MT[ring],
          Which[
            useSumsQ && OptionValue["SumSubsetParameter"] === 1
            , (* use all sum eqns so no check needed *)
            { pentEqns , {} }
            , (* don't use all sum eqns so need to check for consistency *)
            useSumsQ
            ,
            { pentEqns, sumEqns }
            ,
            True
            ,
            { binEqns, sumEqns  }
          ],
          fSymbols,
          "InvertibleMatrices" -> invMats,
          "Equivalences" -> 
            If[ 
              CommutativeQ @ ring,
              ProjectiveTetrahedralSymmetries[ ring, fSymbols ],
              {}
            ]
        ]
        ,
        True
        ,
        AddOptions[opts][FindZeroValues][
          If[ useSumsQ, pentEqns, binEqns ],
          fSymbols,
          "InvertibleMatrices" -> invMats,
          "Equivalences" -> 
            If[ 
              CommutativeQ @ ring,
              ProjectiveTetrahedralSymmetries[ ring, fSymbols ],
              {}
            ]
        ]
      ];

    (* TODO: even if "FindZeroValuesUsingSums" -> False then we should also filter out 
      solutions that are incompatible with the sumequations.*)

    printlog["PPSI:zero_Fs_results", { procID, Normal @ zeros } ];

    If[
      Length @ Normal @ zeros === 0
      ,
      Return[ { } ]
    ];

    printlog["PPSI:fixing_gauge", {procID } ];
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
        "GaugeDemands" -> gaugeDemands,
        "ExcludedVariables" -> unionZeros
      ];

    (* Remove the newly fixed F-symbols from the list of variables *)
    fSymbols =
      Complement[ fSymbols, extraFixedFs[[;;,1]] ];

    (* Substitute the values of the newly fixed F-symbols in the invertible matrices
       and remove trivial 1D F-matrices. *)

    invMats = WithMinimumDimension[ invMats/.extraFixedFs, 2 ];

    (* Substitute the values of the newly fixed F-symbols in the equations,
       remove trivial equations and delete duplicate equations *)

    pentEqns = TEL[ pentEqns/.extraFixedFs ];

    { newBinEqns, newSumEqns } = BinomialSplit[ pentEqns ]; 

    Remove[binEqns,sumEqns];

    printlog[ "PPSI:fixed_fs", { procID, extraFixedFs } ];


    (* Set up a common set of binomial equations that don't contain F-symbols that could
       be zero *)

    printlog[ "PPSI:reducing_bin_eqns", { procID } ];

    neverZeroBinEqns = 
      With[{ 
        varsLists = 
          Join @@@
          Map[ 
            GetVariables[ #, F ]&,
            List @@@ newBinEqns, 
            {2}
          ]
        },
      
        pentEqns[[
          DeleteCases[ (* delete indices of equations that contain a possible zero F-symbol *)
            Range @ Length @ varsLists, 
            i_/; IntersectingQ[ varsLists[[i]], unionZeros ]
          ]
        ]]
      ];

    (* Reduce this system of binomial equations and add it to the non-binomial equations *)
    (* At the moment we only support the HermiteDecomposition because its annoying to 
       merge deduced values of F-symbols at this point. The HermiteDecomposition does 
       not create assumptions, nor does it find values of F-symbols *)
    (* TODO: this should be implemented in the future!!! *)

    If[ 
      MemberQ[ { opts }, x_ /; Keys @ x == "Method" && Values @ x =!= "HermiteDecomposition" ], 
      Message[ PrepareHexagonSolverInput::notyetsupported ];
      { opts } = DeleteCases[ { opts }, "Method" -> _ ];
    ];

    pentEqns = 
      Join[
        ( # == 0 )& /@
        AddOptions[opts][ReduceBinomialSystem][ 
          neverZeroBinEqns, 
          Complement[ fSymbols, unionZeros ] 
        ]["Polynomials"]
        ,
        newSumEqns
      ];

    ClearAll[ newBinEqns, newSumEqns, neverZeroBinEqns ];


    (* Try to fix extra gauges, if possible, for each of the 0-configurations.
      Also substitute 0 values, and update equations, variables, and matrices *)

    printlog[ "PPSI:fixing_extra_gauges", { procID } ];

    If[ (* All gauges are fixed *)
      remainingSym["Transforms"][[;;,1]] === remainingSym["Transforms"][[;;,2]]
      ,
      (* THEN *)
      printlog[ "PPSI:no_gauge_freedom_left", { procID } ];

      Reap[
        Do[
          dz =
            Dispatch[z];

          Sow[
            <|
              "Equations"  -> TEL[ pentEqns/.dz // TEL ],
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
      ][[2,1]]
      ,
      (* ELSE *)
      printlog[ "PPSI:gauge_freedom_left", { procID } ];

      Reap[
        Do[
          dz =
          Dispatch[z];

          { specificSym, specificFixedFs } =
            BreakMultiplicativeSymmetry[
              AddZerosToSymmetries[ remainingSym, z ]
            ];

          Sow[
            <|
              "Equations" -> TEL[ pentEqns/.dz/.specificFixedFs ],
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


PackageScope["ValidZerosQ"]

ValidZerosQ[ eqns_ ][ zeros_ ] :=
  FreeQ[ eqns/.Dispatch[zeros], False | 0 == HoldPattern[Times[__]] | HoldPattern[Times[__]] == 0 ];


PackageScope["PrepareHexagonSolverInput"]

PrepareHexagonSolverInput::usage =
  "Constructs the hexagon equations and symmetries.";

Options[ PrepareHexagonSolverInput ] :=
  Join[
    Options[ HexagonEquations ],
    { "TwistFactors" -> None }
  ];

PrepareHexagonSolverInput[ ring_FusionRing, opts:OptionsPattern[] ] :=
  Module[{ rSymbols, fSymbols, gaugeSym, g, knowns, time, result, procID, equations, tf },
    tf =
      OptionValue["TwistFactors"];

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
          Join[
            If[ tf =!= None, TwistFactorEquations[ ring, tf ], {} ],
            AddOptions[opts][HexagonEquations] @ ring
          ];

        <|
          "Equations" ->  equations,
          "Variables" ->  GetVariables[ equations, { R, F } ],
          "Symmetries" -> gaugeSym,
          "Knowns" ->     knowns
        |>

      ];

    printlog["Gen:results", { procID, result, time } ];

    result
  ];

(*+---------------------------------------------------------------------------+*)
(*|              FINDING GROEBNER BASES FOR POLYHEDRAL SYSTEMS                |*)
(*+---------------------------------------------------------------------------+*)

PackageExport["PentagonGroebnerSystems"]

PentagonGroebnerSystems::usage =
"PentagonGroebnerSystems[r,z] calculates Groebner bases for the pentagon equations of the "<>
"fusion ring r, with remaining variables labeled by z.";
(*Options include all options for solving the pentagon equations and all options for finding groebner bases.";*)

Options[PentagonGroebnerSystems] :=
  Union[
    {
      "ReduceRoots" -> False,
      "ReducePowerSums" -> False,
      "SimplifyIntermediateResultsBy" -> Identity
    },
    Options[PreparePentagonSolverInput],
    Options[ReduceByBinomials],
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
  procID, result, time, solverInput,
  sumSystems, SumBinEqns, systems, AddValues, ReduceSystems, reducedSystems1, reducedSystems2, simplify,
  invertibilityConstraints
},

  simplify =
    Composition[
      If[ OptionValue["ReducePowerSums"], PowerSumReduce, Identity ],
      If[ OptionValue["ReduceRoots"], RootReduce, Identity ],
      OptionValue["SimplifyIntermediateResultsBy"]
    ];

  procID =
    ToString[Unique[]];

  printlog[ "MFPGS:init", { procID, ring, var, {opts} } ];

  { time, result } =
  AbsoluteTiming[

    If[ (* Ring is trivial *)
      Rank[ring] == 1,
      Return[ { { { }, { F[1,1,1,1,1,1] -> 1 } } } ]
    ];

    SumBinEqns =
      Reverse @* BinomialSplit;

    solverInput = 
      AddOptions[opts][PreparePentagonSolverInput] @ ring;


    (* TODO: This is very inefficient: we should reduce the system of binomial equations 
       first for all equations that never contain a 0 F-symbol. Otherwise we're reducing that same set
       again for each set of zero values. *)
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
          AddOptions[opts][ReduceByBinomials][
            Sequence @@ SumBinEqns[ input["Equations"] ],
            input["Variables"],
            var,
            "Symmetries" -> input["Symmetries"],
            "InvertibleMatrices" -> input["InvertibleMatrices"],
            "NonSingular" -> True,
            "SimplifyIntermediateResultsBy" -> simplify
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

    (* TODO: it would be more optimal to use the invertibilityConstraints
       as assumptions
    *)
    AddValues[ { preKnowns_, systems_ } ] :=
      Table[
        <|
          "Polynomials" -> TPL @ ToReducedPolynomial[ sys[[1]], var , "SimplifyBy" -> simplify ],
          "Assumptions" -> True,
          "Rules" -> Union[ preKnowns, sys[[2]] ]
        |>
        , { sys, systems }
      ];

    (* Set up polynomial systems *)
    systems = Flatten[ AddValues /@ sumSystems ];


    (* Reduce the systems using linearity *)
    ReduceSystems[ system_ ] :=
      With[
        { newSystems = AddOptions[opts][ReduceByLinearity][ system["Polynomials"], var ] },
        Table[
          <|
            "Polynomials" -> simplify /@ nSys["Polynomials"],
            "Assumptions" -> nSys["Assumptions"],
            "Rules"       -> MapAt[ simplify, system["Rules"]/.nSys["Rules"], { All, 2 } ]
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
        simplify /@
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
"HexagonGroebnerSystems[r,z] calculates Groebner bases for the hexagon equations of the fusion ring r, " <>
"in remaining variable z.";
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
  opt, systems, AddKnowns, ReduceSystems,  procID, result, time, reducedSystems, simplify },
  procID =
  ToString @ Unique[];

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

    sumSystems =
      AddOptions[opts][ReduceByBinomials][
        Sequence @@ SumBinEqns[ equations ],
        Sort @ variables,
        var,
        opt,
        "Symmetries" ->                       symmetries,
        "SimplifyIntermediateResultsBy" ->    simplify
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
          "Polynomials" -> simplify /@ nSys["Polynomials"],
          "Assumptions" -> nSys["Assumptions"],
          "Rules"       -> MapAt[ simplify, system["Rules"]/.nSys["Rules"], { All, 2 } ]
        |>,
        { nSys, newSystems }
      ]
    ];

    reducedSystems[1] =
    Flatten[ ReduceSystems /@ systems ];

    printlog["MFHGS:systems", { procID, reducedSystems[1] } ];

    (* If only 1 variable remains it is often faster to solve the system directly *)
    reducedSystems[2] =
    Flatten[
      QuickSolve[ #, var, "SimplifyBy" -> simplify ]& /@
      reducedSystems[1]
    ];

    printlog["MFHGS:quicksolve", { procID, reducedSystems[2] } ];

    Table[
      <|
        "GroebnerBasis" ->
        simplify /@
        AddOptions[opts][IncrementalGroebnerBasis][
          sys["Polynomials"],
          GetVariables[ sys["Polynomials"], var ]
        ],
        "Assumptions" -> sys["Assumptions"],
        "Rules" -> MapAt[ simplify, sys["Rules"], { All, 2 } ]
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

Options[QuickSolve] = { "SimplifyBy" -> Identity };

QuickSolve[ system_, var_, opts:OptionsPattern[] ] :=
With[{pols = system["Polynomials"] },
  If[ CountVariables[ pols, var ] =!= 1, Return[system] ];

  With[
    {
      simplestPol =
      First @
      Flatten @
      MinimalBy[ pols, Exponent[ #, First @ GetVariables[ pols, var ] ]&, 1 ],
      assumptions =
      system["Assumptions"]
    },
    {
      soln =
      Cases[
        OptionValue["SimplifyBy"]@
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

];

(*
+---------------------------------------------------------------------------+
|                      SOLVING POLYHEDRON EQUATIONS                         |
+---------------------------------------------------------------------------+
*)

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
  procID = ToString[Unique[]];

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

    bases = AddOptions[opts][PentagonGroebnerSystems][ ring, z ];

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
  Block[ {z}, SolveHexagonEquations[ ring, z, opts ] ];

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
