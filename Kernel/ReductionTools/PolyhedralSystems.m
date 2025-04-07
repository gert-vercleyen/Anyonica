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
  Union[ 
    Options[ MultiplicityFreePentagonEquations ],
    Options[ PentagonEquationsWithMultiplicity ]
  ];

PentagonEquations[ ring_FusionRing?FusionRingQ, opts:OptionsPattern[] ] :=
If[
  Mult[ ring ] == 1
  ,
  MultiplicityFreePentagonEquations[ ring, opts ]
  ,
  PentagonEquationsWithMultiplicity[ ring, opts ]
];

PackageExport["PE"]

PE::usage =
  "Shorthand for PentagonEquations.";

PE = PentagonEquations;


Options[MultiplicityFreePentagonEquations] = 
  { 
   "TrivialVacuumSymbols" -> True,
   "Knowns" -> {}
    };

MultiplicityFreePentagonEquations[ ring_FusionRing, opts : OptionsPattern[]] := 
  Module[{ ntvkQ, trivVacQ, knowns, loft, roft, grpdByLabels, a, b, c, 
   d, e, f, g, l, k, abcdeLabels, labcdeTrees, rabcdeTrees, mt, 
   knownsQ, dKnowns, eqn, fs, zeroFQ, oneFQ },

    trivVacQ  = OptionValue["TrivialVacuumSymbols"];
    knowns    = OptionValue["Knowns"];
    knownsQ   = knowns =!= {};
    dKnowns   = Dispatch[knowns];
    
    ntvkQ = (* True if the given known values set a vacuum F to a value != 1 *)
      And[
        knowns =!= {},
        Cases[
          knowns, 
          HoldPattern[ f_ -> x_] /; MatchQ[f, $VacuumFPattern] && x != 1 ] =!= {}
      ];
    
    
    loft = LeftOrderedFusionTrees[ ring, 3 ];
    roft = RightOrderedFusionTrees[ ring, 3 ];
    abcdeLabels = 
      Union[ 
        loft[[;; , {1, 2, 3, 4, 7}]], 
        roft[[;; , {1, 2, 3, 4, 7}]] 
      ];
    labcdeTrees = GroupBy[ loft, #[[{1, 2, 3, 4, 7}]] &  ];
    rabcdeTrees = GroupBy[ loft, #[[{1, 2, 3, 4, 7}]] &  ];
    
    mt = MT @ ring;

    zeroFQ[a_, b_, c_, d_, e_, f_] :=
      Or[
        mt[[a, b, e]] == 0, mt[[e, c, d]] == 0,
        mt[[a, f, d]] == 0, mt[[b, c, f]] == 0
      ];

    oneFQ[a_, b_, c_, d_, e_, f_] := 
      Or[ a == 1, b == 1, c == 1 ];
    
    If[
      ! knownsQ
      ,(* THEN *)
      If[
        trivVacQ
        ,
        fs[a_, b_, c_, d_, e_, f_] :=
          Which[
            zeroFQ[a, b, c, d, e, f], 0,
            oneFQ[a, b, c, d, e, f], 1,
            True, F[a, b, c, d, e, f]
          ]
        ,
        fs[a_, b_, c_, d_, e_, f_] :=
        Which[
          zeroFQ[a, b, c, d, e, f], 0,
          True, F[a, b, c, d, e, f]
        ]
      ]
      , (*Else*)
      Which[
        ntvkQ, (* Override option for trivial vacuum symbols *)
        fs[a_, b_, c_, d_, e_, f_] :=
          Which[
          zeroFQ[a, b, c, d, e, f], 0,
          NumericQ[F[a, b, c, d, e, f] /. dKnowns], 
          F[a, b, c, d, e, f] /. dKnowns,
          True, F[a, b, c, d, e, f]
          ]
        ,
        trivVacQ, (* we can replace the knowns at the end in one go *)
        fs[a_, b_, c_, d_, e_, f_] :=
          Which[
            zeroFQ[a, b, c, d, e, f], 0,
            oneFQ[a, b, c, d, e, f], 1,
            True, F[a, b, c, d, e, f]
          ]
        ,
        True,
        fs[a_, b_, c_, d_, e_, f_] :=
          If[ zeroFQ[a, b, c, d, e, f], 0, F[a, b, c, d, e, f] ]
      ]
    ];
    
    TEL[
      Reap[
        Do[
          {a, b, c, d, f, g, e} = lTreeLabels;
          {l, k} = rTreeLabels[[{5, 6}]];
          eqn = 
            ( 
              fs[f, c, d, e, g, l] fs[a, b, l, e, f, k] == 
              Sum[ 
                fs[a, b, c, g, f, h] *
                fs[a, h, d, e, g, k] *
                fs[b, c, d, k, h, l] , 
                { h, Rank @ ring } 
              ]
            );

          If[! TrueQ[eqn], Sow[eqn]]
          ,
          {outerLabels, abcdeLabels}, (* Top and bottom labels *)
          {lTreeLabels, labcdeTrees[outerLabels]}, (* intr labels left tree*)
          {rTreeLabels, rabcdeTrees[outerLabels]} (* intr labels right tree *)
        ]
      ][[2, 1]] /. dKnowns
    ]
  ];


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

Options[PreparePentagonSolverInput] :=
Union[
  {
    "GaugeDemands" -> {},
    "ZeroValues" -> None,
    "NonSingular" -> False,
    "PreEqualCheck" -> Identity,
    "UseDatabaseOfSmithDecompositions" -> True,
    "UseDatabaseOfZeroValues" -> False,
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
    gaugeDemands  = OptionValue["GaugeDemands"];
    preEqCheck    = OptionValue["PreEqualCheck"];
    useDBQ        = OptionValue["UseDatabaseOfSmithDecompositions"];
    storeDecompQ  = OptionValue["StoreDecompositions"];
    subsSol       = OptionValue["InjectSolution"];

    procID = ToString[Unique[]];

    printlog["PPSI:init", { procID, ring, {opts} } ];

    { time, solutions } =
    AbsoluteTiming[

      If[ (* Don't want to substitute solution to subsystem *)
        subsSol === {},
        compatibleSol = {},
        (* ELSE: want to substitute solution *)
        { sRing, sSol } = subsSol;

        (* Function that maps labels in the subring sRing to corresponding labels in ring *)
        inject = InjectionForm[ ring, sRing ];

        If[ (* Check whether ring is proper subring *)
          inject === None,
          Message[PreparePentagonSolverInput::notsubring, sRing, ring ];
          Abort[]
        ];

        (* Rename all labels of the subsolution sSol so they correspond 
          to the labels of the sub-fusion-ring of ring *)
        compatibleSol =
          MapAt[
            ReplaceAll[ i_Integer  :> inject[i] ],
            sSol,
            { All, 1 }
          ]
      ];

      allFSymbols = FSymbols @ ring;

      vacuumSymbols = Cases[ allFSymbols, $VacuumFPattern ];

      (* Unknown F-symbols *)
      fSymbols =
        Complement[
          allFSymbols,
          vacuumSymbols,
          GetVariables[ compatibleSol, F ]
        ];

      pentEqns = PentagonEquations[ ring, "Knowns" -> compatibleSol ];

      { binEqns, sumEqns } = BinomialSplit @ pentEqns;

      (* Set up the constraints on the solutions: F-matrices are invertible and 
        removing zig-zags is an isomorphism *)
      constraint =
        And @@
        Map[
          Det[#] != 0 &,
          With[ { d = CC[ring] },
            Join[
              FMatrices[ ring ],
              Array[ {{F[ #, d[#], #, #, 1, 1 ]}}&, Rank[ring] ] ]
          ]/.Dispatch[ Thread[ vacuumSymbols -> 1 ] ] 
        ];

      gaugeSymmetries = GaugeSymmetries[ allFSymbols, g ];

      (* Update matrices and gauge symmetries to take account of known F's *)
      printlog[ "PPSI:restricting_gauges", { procID } ];

      (* Update gauges: vacuum F-symbols and given subsolution need to remain fixed *)
      gaugeSymmetries =
        RestrictMultiplicativeSymmetries[
          gaugeSymmetries,
          vacuumSymbols ~ Join ~ compatibleSol[[;;,1]],
          g
        ];

      printlog[ "PPSI:original_system", { procID, pentEqns, fSymbols } ];

      printlog[ "PPSI:zero_Fs", { procID } ];

      (* Find Configurations of non-trivial 0-values *)
      zeros = 
        AddOptions[opts][PentagonZeroValues][ 
          ring, fSymbols, pentEqns, sumEqns, binEqns, constraint
        ];

      printlog["PPSI:zero_Fs_results", { procID, zeros } ];

      If[ Length @ zeros === 0, Return @ { } ];

      printlog["PPSI:fixing_gauge", {procID } ];
      (* Break Gauge Symmetry: first for all variables that are never 0, i.e.
        that do not appear in any of the "zeros" from previous step *)

      (* Get all F-symbols that could be 0 for some configuration in zeros *)
      unionZeros = Union @@ Keys /@ zeros;

      (* Get all F-symbols that can never be 0 *)
      sharedVars = Complement[ fSymbols, unionZeros ];

      (* Fix the gauge for all the F symbols that can never be 0 *)
      { remainingSym, extraFixedFs } =
        BreakMultiplicativeSymmetry[
          gaugeSymmetries,
          "GaugeDemands" -> gaugeDemands,
          "ExcludedVariables" -> unionZeros
        ];

      (* Remove the newly fixed F-symbols from the list of variables *)
      fSymbols = Complement[ fSymbols, Keys @ extraFixedFs ];

      (* Substitute the values of the newly fixed F-symbols in the constraints *)
      constraints = constraints/.Dispatch[extraFixedFs];

      (* Substitute the values of the newly fixed F-symbols in the equations,
        remove trivial equations and delete duplicate equations *)

      pentEqns =
        TEL[ pentEqns/.Dispatch[extraFixedFs] ];

      { newBinEqns, newSumEqns } =
        BinomialSplit[ pentEqns ]; 

      ClearAll[binEqns,sumEqns];

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
      
      pentEqns = 
        Join[
          AddOptions[opts][ReduceBinomialSystem][ 
            neverZeroBinEqns, 
            Complement[ fSymbols, unionZeros ] 
          ]
          ,
          newSumEqns
        ];

      ClearAll[newBinEqns,newSumEqns,neverZeroBinEqns];


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

Options[PentagonZeroValues] := 
  Join[ 
    Options[FindZeroValues],
    { 
      "ZeroValues" -> None,
      "NonSingular" -> True
    }
  ];

PentagonZeroValues[ r_FusionRing, fs_, pEqns_, sEqns_, bEqns_, constr_, OptionsPattern[] ] :=
  Module[{ equivs, sumsQ, allSumsQ, system },

    If[ GroupRingQ[r] || OptionValue["NonSingular"], Return @ {{}} ];

    If[ OptionValue["ZeroValues"] =!= None, Return @ OptionValue["ZeroValues"] ];

    equivs = 
      If[ 
        CommutativeQ @ r, 
        ProjectiveTetrahedralSymmetries[r, fs],
        {}
      ];

    sumsQ     = OptionValue["FindZerosUsingSums"];
    allSumsQ  = sumsQ && OptionValue["SumSubsetParameter"] == 1;		
    
    If[
      OptionValue["UseDataBaseOfZeroValues"]
      , (* THEN *)
      system = 
        Which[ 
            allSumsQ, { pEqns, {} },
            sumsQ, { pEqns, sEqns },
            True, { bEqns, sEqns}
          ];
            
      AddOptions[opts][MemoizedZeroValues][
        MT @ r, system, fs,
        "Constraint" -> constr, 
        "Equivalences" -> equivs
      ]
      , (* ELSE *)
      system = If[ sumsQ, pEqns, bEqns ];	

      Select[ValidZerosQ[sEqns]] @
      AddOptions[opts][FindZeroValues][
        system,
        fs,
        "Constraint" -> constr, 
        "Equivalences" -> equivs
      ]
    ]
  ];


PackageScope["ValidZerosQ"]

ValidZerosQ[ eqns_ ][ zeros_ ] :=
  FreeQ[ 
    eqns/.Dispatch[zeros], 
    False | 
    0 == HoldPattern[Times[__]] | HoldPattern[Times[__]] == 0 |
    0 == HoldPattern[Power[_,_.]] | HoldPattern[Power[_,_.]] == 0 
  ];


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

    procID = ToString[Unique[]];

    printlog[ "MFPGS:init", { procID, ring, var, {opts} } ];

    { time, result } =
    AbsoluteTiming[

      If[ (* Ring is trivial *)
        Rank[ring] == 1,
        Return[ { { { }, { F[1,1,1,1,1,1] -> 1 } } } ]
      ];

      SumBinEqns = Reverse @* BinomialSplit;

      solverInput = AddOptions[opts][PreparePentagonSolverInput] @ ring;


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
